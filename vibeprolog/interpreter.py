"""Main Prolog interpreter interface."""

import io
import re
import sys
import warnings
from pathlib import Path
from typing import Any

from vibeprolog.utils.term_utils import term_to_string

from lark.exceptions import LarkError

from vibeprolog.exceptions import PrologError, PrologThrow
from vibeprolog.engine import CutException, PrologEngine
from vibeprolog.parser import (
    Clause,
    Directive,
    List as ParserList,
    PredicateIndicator,
    PredicatePropertyDirective,
    PrologParser,
    _strip_comments,
    extract_op_directives,
    tokenize_prolog_statements,
)
from vibeprolog.operators import OperatorTable
from vibeprolog.terms import Atom, Compound, Number, Variable
from vibeprolog.unification import Substitution, apply_substitution
from vibeprolog.dcg import expand_dcg_clause
from .utils import reconstruct_operator_name_from_term

# Constants
PROJECT_ROOT = Path(__file__).resolve().parent.parent
LIBRARY_SEARCH_PATHS = [
    PROJECT_ROOT / "library",
    PROJECT_ROOT / "examples" / "modules",
]
LOADED_MODULE_PREFIX = "loaded:"

# Scryer-specific directives that are recognized but ignored with a warning
IGNORED_DIRECTIVES = {"non_counted_backtracking", "meta_predicate"}

# Control construct names that are invalid in predicate indicators (ISO §5.1.2.1)
CONTROL_CONSTRUCT_NAMES = {"!", ",", ";", "->"}


class Module:
    def __init__(self, name: str, exports: set[tuple[str, int]] | None):
        self.name = name
        self.exports = exports if exports is not None else set()
        self.predicates: dict[tuple[str, int], list] = {}
        self.file: str | None = None
        # Import table: (functor, arity) -> source_module_name
        self.imports: dict[tuple[str, int], str] = {}
        # Exported operators: set of (precedence, associativity, name)
        self.exported_operators: set[tuple[int, str, str]] = set()
        # Predicates that shadow built-ins: set of (functor, arity)
        self.shadowed_builtins: set[tuple[str, int]] = set()


class PrologInterpreter:
    """Main interface for the Prolog interpreter."""

    def __init__(
        self,
        argv: list[str] | None = None,
        max_recursion_depth: int = 10000,
        builtin_conflict: str = "skip",
    ) -> None:
        self.operator_table = OperatorTable(builtin_conflict=builtin_conflict)
        self.parser = PrologParser(self.operator_table)
        self._import_scanner_parser = PrologParser(OperatorTable(builtin_conflict=builtin_conflict))
        self.clauses = []
        # Module system
        self.modules: dict[str, "Module"] = {}
        self.modules["user"] = Module("user", None)  # Default user module
        self.current_module: str = "user"

        self._argv: list[str] = argv or []
        self.max_recursion_depth = max_recursion_depth
        self.builtin_conflict = builtin_conflict
        self.engine = None
        self.initialization_goals = []
        # Global predicate properties - for built-ins and backward compatibility
        self.predicate_properties: dict[tuple[str, int], set[str]] = {}
        self._predicate_sources: dict[tuple[str, int], set[str]] = {}
        # Module-scoped predicate properties: module_name -> {(functor, arity) -> set[str]}
        self._module_predicate_properties: dict[str, dict[tuple[str, int], set[str]]] = {}
        # Module-scoped predicate sources: module_name -> {(functor, arity) -> set[str]}
        self._module_predicate_sources: dict[str, dict[tuple[str, int], set[str]]] = {}
        self.predicate_docs: dict[tuple[str, int], str] = {}
        self._consult_counter = 0
        self._builtins_seeded = False
        self._tabled_predicates: set[tuple[str, int]] = set()
        # Cache of imported operator directives by resolved file path.
        # Maps file paths to lists of (precedence, associativity, name) tuples.
        # Grows unbounded with the number of unique modules consulted, but memory
        # footprint is small (~1-2KB per module). Naturally bounded by project size.
        # Cleared when the interpreter instance is destroyed.
        self._import_operator_cache: dict[str, list[tuple[int, str, str]]] = {}
        self._current_source_path: Path | None = None
        # Stack for conditional compilation directives (if/else/endif)
        # Each entry is (is_active, has_seen_else, any_branch_taken) where:
        # - is_active: True if we are currently including code
        # - has_seen_else: True if we have seen an else clause (to prevent multiple else)
        # - any_branch_taken: True if any if/elif branch has been taken (for else logic)
        self._conditional_stack: list[tuple[bool, bool, bool]] = []

    @property
    def argv(self) -> list[str]:
        return self._argv

    @argv.setter
    def argv(self, value: list[str]) -> None:
        self._argv = value
        if self.engine is not None:
            self.engine.argv = value

    def _raise_syntax_error(self, exc: Exception, location: str) -> None:
        """
        Centralized helper to convert parsing exceptions into Prolog syntax errors
        and raise a PrologThrow with the resulting error term.
        """
        error_term = PrologError.syntax_error(str(exc), location)
        raise PrologThrow(error_term)

    def _is_conditionally_active(self) -> bool:
        """Return True if all parent conditionals are active (we should include code)."""
        return all(is_active for is_active, _, _ in self._conditional_stack)

    def _handle_if_directive(self, condition) -> None:
        """Handle :- if(Condition). directive."""
        # If a parent conditional is inactive, this nested if is also inactive
        # We don't evaluate the condition but still track the nesting
        if not self._is_conditionally_active():
            self._conditional_stack.append((False, False, False))
            return

        # Evaluate the condition
        condition_succeeded = self._evaluate_condition(condition)
        # (is_active, has_seen_else, any_branch_taken)
        self._conditional_stack.append((condition_succeeded, False, condition_succeeded))

    def _handle_elif_directive(self, condition) -> None:
        """Handle :- elif(Condition). directive (else-if)."""
        if not self._conditional_stack:
            error_term = PrologError.syntax_error("elif without matching if", "elif/1")
            raise PrologThrow(error_term)

        is_active, has_seen_else, any_branch_taken = self._conditional_stack[-1]

        if has_seen_else:
            error_term = PrologError.syntax_error("elif after else in conditional block", "elif/1")
            raise PrologThrow(error_term)

        # Check if parent conditionals are active (excluding current level)
        parent_active = all(active for active, _, _ in self._conditional_stack[:-1])

        if not parent_active:
            # Parent is inactive, so this elif is also inactive
            # Keep has_seen_else=False to allow more elif/else
            return

        if any_branch_taken:
            # A previous branch was taken, so skip this elif and all subsequent
            self._conditional_stack[-1] = (False, False, True)
        else:
            # No previous branch taken, evaluate this condition
            condition_succeeded = self._evaluate_condition(condition)
            self._conditional_stack[-1] = (condition_succeeded, False, condition_succeeded)

    def _handle_else_directive(self) -> None:
        """Handle :- else. directive."""
        if not self._conditional_stack:
            error_term = PrologError.syntax_error("else without matching if", "else/0")
            raise PrologThrow(error_term)

        is_active, has_seen_else, any_branch_taken = self._conditional_stack[-1]

        if has_seen_else:
            error_term = PrologError.syntax_error("multiple else clauses in conditional block", "else/0")
            raise PrologThrow(error_term)

        # Check if parent conditionals are active (excluding current level)
        parent_active = all(active for active, _, _ in self._conditional_stack[:-1])

        if not parent_active:
            # Parent is inactive, so this else block is also inactive
            self._conditional_stack[-1] = (False, True, any_branch_taken)
        else:
            # Include else only if no previous branch was taken
            self._conditional_stack[-1] = (not any_branch_taken, True, True)

    def _handle_endif_directive(self) -> None:
        """Handle :- endif. directive."""
        if not self._conditional_stack:
            error_term = PrologError.syntax_error("endif without matching if", "endif/0")
            raise PrologThrow(error_term)

        self._conditional_stack.pop()

    def _evaluate_condition(self, condition) -> bool:
        """Evaluate a conditional compilation condition.
        
        Returns True if the condition succeeds, False otherwise.
        """
        # Create a temporary engine if needed
        if self.engine is None:
            temp_engine = PrologEngine(
                self.clauses,
                self.argv,
                self.predicate_properties,
                self._predicate_sources,
                self.predicate_docs,
                operator_table=self.operator_table,
                max_depth=self.max_recursion_depth,
            )
            temp_engine.interpreter = self
        else:
            temp_engine = self.engine

        # Try to find at least one solution
        try:
            solutions = temp_engine._solve_goals([condition], {})
            # Check if at least one solution exists
            next(solutions)
            return True
        except (StopIteration, CutException):
            return False

    def _ensure_builtin_properties(self) -> None:
        """Seed predicate_properties with built-ins for directive validation."""

        if not self._builtins_seeded:
            eng = PrologEngine(
                self.clauses,
                self.argv,
                self.predicate_properties,
                self._predicate_sources,
                self.predicate_docs,
                max_depth=self.max_recursion_depth,
            )
            # Expose interpreter to engine for module introspection
            eng.interpreter = self
            self._builtins_seeded = True

    def _is_conditional_directive(self, item) -> bool:
        """Check if item is a conditional compilation directive (if/else/endif/elif)."""
        if not isinstance(item, Directive):
            return False
        goal = item.goal
        if isinstance(goal, Compound) and len(goal.args) == 1:
            return goal.functor in ("if", "elif")
        if isinstance(goal, Atom):
            return goal.name in ("else", "endif")
        return False


    def _process_items(
        self,
        items: list,
        source_name: str,
        closed_predicates: set[tuple[str, int]] | None = None,
        last_predicate: tuple[str, int] | None = None,
    ) -> tuple[str, int] | None:
        """Process parsed clauses and directives.
        
        Args:
            items: List of parsed clauses and directives
            source_name: Name of the source being consulted
            closed_predicates: Set of predicates that have been "closed"
            last_predicate: The last predicate key that was added
            
        Returns:
            The last predicate key processed
        """
        self._ensure_builtin_properties()
        if closed_predicates is None:
            closed_predicates = set()

        for item in items:
            # Conditional compilation directives are always processed
            if self._is_conditional_directive(item):
                self._handle_directive(item, closed_predicates, source_name)
                continue
            
            # Skip non-conditional items when not in an active conditional block
            if not self._is_conditionally_active():
                continue

            if isinstance(item, Clause):
                # Expand DCG clauses before adding
                if item.dcg:
                    expanded_clause = expand_dcg_clause(item.head, item.body)
                    # Create a new clause with the expanded form, preserving doc and meta
                    processed_clause = Clause(
                        head=expanded_clause.args[0],  # Head of the :- clause
                        body=[expanded_clause.args[1]],  # Body of the :- clause
                        doc=item.doc,
                        meta=item.meta,
                        dcg=False  # Mark as no longer DCG
                    )
                else:
                    processed_clause = item

                # Associate clause with current module context
                processed_clause.module = self.current_module

                last_predicate = self._add_clause(
                    processed_clause, source_name, closed_predicates, last_predicate
                )
                # Store PlDoc if present
                if processed_clause.doc:
                    head = processed_clause.head
                    if isinstance(head, Compound):
                        key = (head.functor, len(head.args))
                    elif isinstance(head, Atom):
                        key = (head.name, 0)
                    else:
                        continue
                    self.predicate_docs[key] = processed_clause.doc
            elif isinstance(item, Directive):
                self._handle_directive(item, closed_predicates, source_name)
                # Store PlDoc for directives if needed
                if item.doc:
                    # For now, ignore directive docs
                    pass
        
        return last_predicate

    def _handle_directive(
        self, directive: Directive, closed_predicates: set[tuple[str, int]], source_name=None
    ):
        """Handle a directive."""
        goal = directive.goal

        # Simplify ignored directive detection
        functor = None
        if isinstance(goal, Compound):
            functor = goal.functor
        elif isinstance(goal, Atom):
            functor = goal.name

        directive_name = functor if functor in IGNORED_DIRECTIVES else None

        if directive_name is not None:
            # Emit warning and ignore unsupported directives
            warnings.warn(
                f"Ignoring unsupported directive: {directive_name}",
                SyntaxWarning,
                stacklevel=2
            )
            return

        # Handle conditional compilation directives: if/else/elif/endif
        # :- if(Condition). - begin conditional block
        if isinstance(goal, Compound) and goal.functor == "if" and len(goal.args) == 1:
            self._handle_if_directive(goal.args[0])
            return

        # :- elif(Condition). - else-if (alternative condition)
        if isinstance(goal, Compound) and goal.functor == "elif" and len(goal.args) == 1:
            self._handle_elif_directive(goal.args[0])
            return

        # :- else. - toggle inclusion state
        if isinstance(goal, Atom) and goal.name == "else":
            self._handle_else_directive()
            return

        # :- endif. - end conditional block
        if isinstance(goal, Atom) and goal.name == "endif":
            self._handle_endif_directive()
            return

        # Module declaration: :- module(Name, Exports).
        if isinstance(goal, Compound) and goal.functor == "module" and len(goal.args) == 2:
            name_term, exports_term = goal.args
            # Validate name
            if not isinstance(name_term, Atom):
                error_term = PrologError.type_error("atom", name_term, "module/2")
                raise PrologThrow(error_term)
            module_name = name_term.name

            # Parse export list (should be a List AST)
            exports = set()
            exported_operators = set()

            if isinstance(exports_term, ParserList):
                for elt in exports_term.elements:
                    # Check if it's an op/3 term
                    if isinstance(elt, Compound) and elt.functor == "op" and len(elt.args) == 3:
                        prec_term, spec_term, name_term = elt.args
                        # Validate operator definition
                        precedence = self.operator_table._parse_precedence(prec_term, "module/2")
                        spec = self.operator_table._parse_specifier(spec_term, "module/2")
                        names = self.operator_table._parse_operator_names(name_term, "module/2")
                        for name in names:
                            exported_operators.add((precedence, spec, name))
                    # Refactor DCG (//) and predicate indicator handling into a shared path
                    # Handle DCG indicator and / indicator using a common helper to reduce duplication
                    elif isinstance(elt, Compound) and elt.functor == "//" and len(elt.args) == 2:
                        name_arg, arity_arg = elt.args
                        result = self._export_predicate_indicator(name_arg, arity_arg, is_dcg=True, elt=elt)
                        if isinstance(result, tuple):
                            exports.add(result)
                    # Each elt should be Name/Arity (Compound "/") or op/3
                    elif isinstance(elt, Compound) and elt.functor == "/" and len(elt.args) == 2:
                        name_arg, arity_arg = elt.args
                        result = self._export_predicate_indicator(name_arg, arity_arg, is_dcg=False, elt=elt)
                        if isinstance(result, tuple):
                            exports.add(result)
                    else:
                        # Skip invalid predicate indicators (e.g., control constructs like !/0)
                        # with a warning for Scryer compatibility
                        warnings.warn(
                            f"Skipping invalid predicate indicator in module export: {term_to_string(elt)}",
                            SyntaxWarning,
                            stacklevel=2
                        )
            else:
                error_term = PrologError.type_error("list", exports_term, "module/2")
                raise PrologThrow(error_term)

            # Extract filepath from source_name (remove the #counter suffix)
            if source_name.startswith("file:") and "#" in source_name:
                filepath = source_name.split("#")[0][5:]  # Remove "file:" prefix and "#counter" suffix
            else:
                filepath = source_name

            # Create Module object and set current module context
            mod = Module(module_name, exports)
            mod.file = filepath
            mod.exported_operators = exported_operators
            self.modules[module_name] = mod
            self.current_module = module_name
            # Reset closed predicates when entering a new module
            closed_predicates.clear()
            return

        # use_module directives: :- use_module(File). or :- use_module(File, Imports).
        if isinstance(goal, Compound) and goal.functor == "use_module":
            if len(goal.args) == 1:
                # use_module(File)
                file_term = goal.args[0]
                imports = None
            elif len(goal.args) == 2:
                # use_module(File, Imports)
                file_term, imports_term = goal.args
                imports = self._parse_import_list(imports_term, "use_module/2")
            else:
                error_term = PrologError.type_error("callable", goal, "use_module/1,2")
                raise PrologThrow(error_term)

            base_dir = self._current_source_path.parent if self._current_source_path else None
            # Resolve the module file
            module_file = self._resolve_module_file(
                file_term, "use_module/1,2", base_path=base_dir
            )

            # Load the module if not already loaded
            module_name = self._load_module_from_file(module_file)

            # Add imports to current module
            current_mod = self.modules.get(self.current_module)
            if current_mod is None:
                # Create user module if it doesn't exist
                current_mod = Module(self.current_module, None)
                self.modules[self.current_module] = current_mod

            # Get the source module
            source_mod = self.modules.get(module_name)
            if source_mod is None:
                error_term = PrologError.existence_error("module", Atom(module_name), "use_module/1,2")
                raise PrologThrow(error_term)

            # Add imports
            if imports is None:
                # Import all exported predicates
                for pred_key in source_mod.exports:
                    current_mod.imports[pred_key] = module_name
                # Import all exported operators (full import only)
                for op_def in source_mod.exported_operators:
                    precedence, spec, name = op_def
                    if self.operator_table.is_protected(name):
                        continue
                    self.operator_table.define(
                        Number(precedence),
                        Atom(spec),
                        Atom(name),
                        "use_module/1",
                        module_name=self.current_module,
                    )
            else:
                # Import specific predicates (operators not imported in selective import)
                for pred_key in imports:
                    if pred_key not in source_mod.exports:
                        indicator = self._indicator_from_key(pred_key)
                        error_term = PrologError.permission_error(
                            "access", "private_procedure", indicator, "use_module/2"
                        )
                        raise PrologThrow(error_term)
                    current_mod.imports[pred_key] = module_name
            return

        if isinstance(goal, PredicatePropertyDirective):
            self._handle_predicate_property_directive(goal, closed_predicates)
            return

        # Reject unsupported directives
        if isinstance(goal, Compound) and goal.functor == "op" and len(goal.args) == 3:
            prec_term, spec_term, name_term = goal.args
            self.operator_table.define(
                prec_term,
                spec_term,
                name_term,
                "op/3",
                module_name=self.current_module,
            )
            return

        # Handle char_conversion/2 directive
        if isinstance(goal, Compound) and goal.functor == "char_conversion" and len(goal.args) == 2:
            from_term, to_term = goal.args
            # Validate both arguments are single-character atoms
            if isinstance(from_term, Variable) or isinstance(to_term, Variable):
                error_term = PrologError.instantiation_error("char_conversion/2")
                raise PrologThrow(error_term)

            # Extract characters
            from_char = self._extract_character(from_term, "char_conversion/2")
            to_char = self._extract_character(to_term, "char_conversion/2")

            # Update the parser's conversion table
            self.parser.set_char_conversion(from_char, to_char)
            return

        # Handle attribute/1 directive for attributed variables
        if isinstance(goal, Compound) and goal.functor == "attribute" and len(goal.args) == 1:
            self._handle_attribute_directive(goal.args[0])
            return

        # Handle supported directives
        if isinstance(goal, Compound) and goal.functor == "initialization" and len(goal.args) == 1:
            init_goal = goal.args[0]
            # Validate the goal
            if isinstance(init_goal, Variable):
                error_term = PrologError.instantiation_error("initialization/1")
                raise PrologThrow(error_term)
            # Check if callable (not number, etc.)
            if not isinstance(init_goal, (Compound, Atom)):
                error_term = PrologError.type_error("callable", init_goal, "initialization/1")
                raise PrologThrow(error_term)
            self.initialization_goals.append(init_goal)
            return

        # Tabling directive: :- table Name/Arity[, Name2/Arity2].
        if isinstance(goal, Compound) and goal.functor == "table" and len(goal.args) == 1:
            indicators = self._parse_table_indicators(goal.args[0])
            for indicator in indicators:
                self._tabled_predicates.add(indicator)
            return
        # Other directives can be added here

    def _export_predicate_indicator(self, name_arg, arity_arg, is_dcg: bool, elt=None):
        """
        Normalize and validate a predicate indicator for module exports.
        Returns a tuple (name, arity) to export or None if skipped.
        Handles both DCG (//) and standard (/) forms via is_dcg flag.
        """
        normalized_name = self._normalize_operator_in_indicator(name_arg)
        # Validate types: Name must be Atom, Arity must be Number
        if isinstance(normalized_name, Atom) and isinstance(arity_arg, Number):
            arity = int(arity_arg.value)
            # Non-negative arity check (both DCG and standard)
            if arity < 0:
                warnings.warn(
                    f"Skipping invalid predicate indicator with negative arity: {normalized_name.name}{'//' if is_dcg else '/'}{arity}",
                    SyntaxWarning,
                    stacklevel=2
                )
                return None
            # Skip control constructs if present (ISO restriction)
            if normalized_name.name in CONTROL_CONSTRUCT_NAMES:
                warnings.warn(
                    f"Skipping invalid predicate indicator in module export: {normalized_name.name}{'//' if is_dcg else '/'}{arity}",
                    SyntaxWarning,
                    stacklevel=2
                )
                return None
            # Compute export arity: DCG adds 2 to arity
            export_arity = arity + (2 if is_dcg else 0)
            return (normalized_name.name, export_arity)
        else:
            # Invalid shape (Name/Arity not formed properly)
            warnings.warn(
                f"Skipping invalid predicate indicator in module export: {term_to_string(elt) if elt is not None else name_arg}",
                SyntaxWarning,
                stacklevel=2
            )
            return None

    def _parse_table_indicators(self, term: Any) -> list[tuple[str, int]]:
        """Parse the argument to a table/1 directive into predicate indicators."""

        def _flatten_args(arg: Any) -> list[Any]:
            if isinstance(arg, ParserList):
                return list(arg.elements)
            if isinstance(arg, Compound) and arg.functor == "," and len(arg.args) == 2:
                return _flatten_args(arg.args[0]) + _flatten_args(arg.args[1])
            return [arg]

        indicators: list[tuple[str, int]] = []
        for raw in _flatten_args(term):
            key = self._validate_predicate_indicator(raw, "table/1")
            indicators.append(key)
        return indicators

    def _parse_import_list(self, imports_term, context: str) -> set[tuple[str, int]]:
        """Parse import list for use_module/2."""
        imports = set()
        if isinstance(imports_term, ParserList):
            for elt in imports_term.elements:
                # Each elt should be Name/Arity (Compound "/")
                if isinstance(elt, Compound) and elt.functor == "/" and len(elt.args) == 2:
                    name_arg, arity_arg = elt.args
                    normalized_name = self._normalize_operator_in_indicator(name_arg)
                    if not isinstance(normalized_name, Atom) or not isinstance(arity_arg, Number):
                        error_term = PrologError.type_error("predicate_indicator", elt, context)
                        raise PrologThrow(error_term)
                    imports.add((normalized_name.name, int(arity_arg.value)))
                else:
                    error_term = PrologError.type_error("predicate_indicator", elt, context)
                    raise PrologThrow(error_term)
        else:
            error_term = PrologError.type_error("list", imports_term, context)
            raise PrologThrow(error_term)
        return imports

    def _flatten_library_term(self, term, context: str) -> list[str]:
        """Convert a library(Name/Sub) term into path components."""
        if isinstance(term, Atom):
            return [term.name]
        if isinstance(term, Compound) and term.functor == "/" and len(term.args) == 2:
            left = self._flatten_library_term(term.args[0], context)
            right = self._flatten_library_term(term.args[1], context)
            return left + right
        error_term = PrologError.type_error("atom", term, context)
        raise PrologThrow(error_term)

    def _resolve_module_file(self, file_term, context: str, base_path: Path | None = None) -> str:
        """Resolve module file path."""
        if isinstance(file_term, Atom):
            module_name = file_term.name
            # Check if it's an already loaded module
            if module_name in self.modules:
                # Return a special marker for already loaded modules
                return f"{LOADED_MODULE_PREFIX}{module_name}"
            # Direct file path
            direct_path = Path(module_name)
            if not direct_path.is_absolute() and base_path is not None:
                candidate = base_path / direct_path
                if candidate.exists():
                    return str(candidate)
            if not direct_path.exists():
                error_term = PrologError.existence_error("file", file_term, context)
                raise PrologThrow(error_term)
            return str(direct_path)
        elif isinstance(file_term, Compound) and file_term.functor == "library" and len(file_term.args) == 1:
            # library(Name) syntax
            lib_term = file_term.args[0]
            lib_parts = self._flatten_library_term(lib_term, context)
            relative_path = Path(*lib_parts)
            candidate_paths = []
            if relative_path.suffix == ".pl":
                candidate_paths.append(relative_path)
            else:
                candidate_paths.append(relative_path.with_suffix(".pl"))
                candidate_paths.append(relative_path)
            # Look in predefined library search paths, ignoring the caller's base path
            for root in LIBRARY_SEARCH_PATHS:
                for candidate_rel in candidate_paths:
                    candidate = root / candidate_rel
                    if candidate.exists():
                        return str(candidate)
            error_term = PrologError.existence_error("file", file_term, context)
            raise PrologThrow(error_term)
        else:
            error_term = PrologError.type_error("atom", file_term, context)
            raise PrologThrow(error_term)

    def _source_path_from_name(self, source_name: str) -> Path | None:
        """Extract filesystem path from a source name identifier.

        Args:
            source_name: Source identifier (e.g., "file:/path/to/file.pl#consult1")

        Returns:
            Path object if source_name starts with "file:", None otherwise
        """
        if source_name.startswith("file:"):
            raw_path = source_name[5:]
            if "#" in raw_path:
                raw_path = raw_path.split("#", maxsplit=1)[0]
            return Path(raw_path)
        return None

    def _resolve_import_for_operators(
        self, import_term: Any, base_path: Path | None
    ) -> tuple[str | None, Module | None]:
        """Resolve an import term to a file path or loaded module.

        Args:
            import_term: The module file specification (Atom or Compound)
            base_path: Base directory for resolving relative paths

        Returns:
            Tuple of (file_path, loaded_module) where:
            - file_path: Resolved file path string, or None if resolution failed
            - loaded_module: Module object if already loaded, None otherwise
        """
        try:
            resolved_import = self._resolve_module_file(
                import_term, "use_module/1,2", base_path=base_path
            )
        except PrologThrow:
            return (None, None)

        if resolved_import.startswith(LOADED_MODULE_PREFIX):
            loaded_name = resolved_import[len(LOADED_MODULE_PREFIX):]
            loaded_module = self.modules.get(loaded_name)
            if loaded_module is None:
                return (None, None)
            # Return the file path if available, otherwise None
            return (loaded_module.file, loaded_module)

        return (resolved_import, None)


    def _extract_import_terms(
        self, prolog_code: str, directive_ops: list[tuple[int, str, str]]
    ) -> list[tuple[Any, bool]]:
        """Extract import directives from Prolog source code.

        Scans the code for use_module/1,2, ensure_loaded/1, and consult/1 directives
        to identify which modules are imported and whether their operators should be included.

        Args:
            prolog_code: Prolog source code to scan
            directive_ops: Operator directives already found in the source

        Returns:
            List of (file_term, include_operators) tuples where:
            - file_term: The module file specification (Atom or Compound)
            - include_operators: True if operators should be imported (False for selective imports)
        """
        imports: list[tuple[Any, bool]] = []
        for chunk in tokenize_prolog_statements(prolog_code):
            # Strip comments to find the actual directive
            stripped = _strip_comments(chunk).strip()
            if not stripped.startswith(":-"):
                continue
            try:
                directives = self._import_scanner_parser.parse(
                    stripped,
                    "import_scan",
                    apply_char_conversions=False,
                    directive_ops=directive_ops,
                )
            except (ValueError, LarkError, PrologThrow):
                # If we cannot parse the directive with default operators, skip it
                continue
            for item in directives:
                if not isinstance(item, Directive):
                    continue
                goal = item.goal
                if not isinstance(goal, Compound):
                    continue
                if goal.functor in {"use_module", "ensure_loaded", "consult"}:
                    if len(goal.args) == 1:
                        imports.append((goal.args[0], True))
                    elif goal.functor == "use_module" and len(goal.args) == 2:
                        # Selective imports (with list) do not import operators
                        imports.append((goal.args[0], False))
        return imports

    def _collect_module_operators_from_file(
        self, filepath: Path, visited: set[str]
    ) -> list[tuple[int, str, str]]:
        """Recursively collect operator directives from a module file and its imports.

        Reads the specified file, extracts local operator directives, and recursively
        collects operators from all imported modules. Results are cached to avoid
        redundant file I/O. The visited set prevents infinite loops in circular imports.

        Args:
            filepath: Path to the module file to process
            visited: Set of already-visited file paths (for cycle detection)

        Returns:
            List of (precedence, associativity, name) tuples for all operators
            found in this file and its transitive imports

        Side effects:
            Updates self._import_operator_cache with results
        """
        raw_path = str(filepath)
        path_exists = filepath.exists()
        cache_key = str(filepath.resolve()) if path_exists else raw_path
        if cache_key in self._import_operator_cache:
            return list(self._import_operator_cache[cache_key])
        if cache_key in visited:
            return []

        visited.add(cache_key)

        try:
            if not path_exists:
                module_match = next(
                    (mod for mod in self.modules.values() if mod.file == raw_path),
                    None,
                )
                if module_match is not None:
                    exported = list(module_match.exported_operators)
                    self._import_operator_cache[cache_key] = exported
                    return exported
                return []

            with open(filepath, "r") as handle:
                module_source = handle.read()

            try:
                local_ops = extract_op_directives(module_source)
            except ValueError:
                self._import_operator_cache[cache_key] = []
                return []
            imports = self._extract_import_terms(module_source, local_ops)

            collected_ops: list[tuple[int, str, str]] = []
            for import_term, include_ops in imports:
                if not include_ops:
                    continue

                file_path, _ = self._resolve_import_for_operators(import_term, filepath.parent)
                if file_path is None:
                    continue

                collected_ops.extend(
                    self._collect_module_operators_from_file(Path(file_path), visited)
                )

            collected_ops.extend(local_ops)
            self._import_operator_cache[cache_key] = list(collected_ops)
            return collected_ops
        finally:
            visited.remove(cache_key)

    def _collect_imported_operators(
        self,
        prolog_code: str,
        source_name: str,
        local_directives: list[tuple[int, str, str]],
    ) -> list[tuple[int, str, str]]:
        """Collect operator directives from all modules imported by the source code.

        Scans the source code for import directives, resolves the imported module files,
        and recursively collects operator definitions from them. This allows the parser
        to recognize operators from imported modules before parsing the source.

        Args:
            prolog_code: Source code to scan for imports
            source_name: Source identifier (e.g., "file:/path/to/file.pl")
            local_directives: Operator directives found in the source itself

        Returns:
            List of (precedence, associativity, name) tuples for all operators
            from imported modules (not including local_directives)

        Side effects:
            Updates self._import_operator_cache via _collect_module_operators_from_file
        """
        base_path = self._source_path_from_name(source_name)
        visited: set[str] = set()
        collected_ops: list[tuple[int, str, str]] = []

        import_terms = self._extract_import_terms(prolog_code, local_directives)
        for import_term, include_ops in import_terms:
            if not include_ops:
                continue

            file_path, loaded_module = self._resolve_import_for_operators(import_term, base_path)

            # If it's a loaded module, use its exported operators directly
            if loaded_module is not None:
                collected_ops.extend(list(loaded_module.exported_operators))
                continue

            # Otherwise, recursively collect from file
            if file_path is not None:
                collected_ops.extend(
                    self._collect_module_operators_from_file(Path(file_path), visited)
                )

        return collected_ops

    def _load_module_from_file(self, filepath: str) -> str:
        """Load a module from file and return its name."""
        # Handle already loaded modules
        if filepath.startswith(LOADED_MODULE_PREFIX):
            module_name = filepath[len(LOADED_MODULE_PREFIX):]
            if module_name in self.modules:
                return module_name
            else:
                error_term = PrologError.existence_error("module", Atom(module_name), "use_module/1,2")
                raise PrologThrow(error_term)

        # Check if already loaded by checking modules
        for mod_name, mod in self.modules.items():
            if mod.file == filepath:
                return mod_name

        # Save current module context
        saved_current_module = self.current_module

        # Load the file
        self.consult(filepath)

        # Get the module name that was loaded
        loaded_module_name = self.current_module

        # Restore current module context
        self.current_module = saved_current_module

        return loaded_module_name

    def _handle_attribute_directive(self, attr_spec) -> None:
        """Handle :- attribute Name/Arity, ... directive.
        
        Declares attributes that can be used with put_atts/2 and get_atts/2
        in the current module.
        
        Note: Currently the declarations are stored but not validated against
        at runtime. The put_atts/2 and get_atts/2 built-ins accept any attribute
        names without checking if they were declared. This storage is provided
        for future validation, tooling, and introspection purposes (e.g., a
        linter or IDE could warn about undeclared attributes).
        """
        module_name = self.current_module
        
        # Store declarations for future validation/tooling - not currently enforced
        if not hasattr(self, '_declared_attributes'):
            self._declared_attributes: dict[str, set[tuple[str, int]]] = {}
        if module_name not in self._declared_attributes:
            self._declared_attributes[module_name] = set()
        
        # Parse the attribute specification (can be single or comma-separated list)
        attrs = self._parse_attribute_list(attr_spec)
        
        for name, arity in attrs:
            self._declared_attributes[module_name].add((name, arity))

    def _parse_attribute_list(self, attr_spec) -> list[tuple[str, int]]:
        """Parse an attribute specification into a list of (name, arity) tuples.
        
        The spec can be:
        - Name/Arity - single attribute
        - (Attr1, Attr2, ...) - multiple attributes
        """
        result = []
        
        if isinstance(attr_spec, Compound) and attr_spec.functor == "," and len(attr_spec.args) == 2:
            # Comma-separated list
            result.extend(self._parse_attribute_list(attr_spec.args[0]))
            result.extend(self._parse_attribute_list(attr_spec.args[1]))
        elif isinstance(attr_spec, Compound) and attr_spec.functor == "/" and len(attr_spec.args) == 2:
            # Single attribute Name/Arity
            name_term, arity_term = attr_spec.args
            if isinstance(name_term, Atom) and isinstance(arity_term, Number):
                result.append((name_term.name, int(arity_term.value)))
            else:
                error_term = PrologError.type_error("attribute_indicator", attr_spec, "attribute/1")
                raise PrologThrow(error_term)
        else:
            error_term = PrologError.type_error("attribute_indicator", attr_spec, "attribute/1")
            raise PrologThrow(error_term)
        
        return result

    def _handle_predicate_property_directive(
        self, directive: PredicatePropertyDirective, closed_predicates: set[tuple[str, int]]
    ) -> None:
        """Apply a predicate property directive."""

        context = f"{directive.property}/1"
        module_name = self.current_module
        
        for indicator in directive.indicators:
            key = self._validate_predicate_indicator(indicator, context)
            
            # Check global properties for built-in check
            global_properties = self.predicate_properties.get(key, set())
            if "built_in" in global_properties and directive.property == "dynamic":
                indicator_term = self._indicator_from_key(key)
                error_term = PrologError.permission_error(
                    "modify", "static_procedure", indicator_term, context
                )
                raise PrologThrow(error_term)

            # Get or create module-scoped properties
            properties = self._get_module_predicate_properties(module_name, key).copy()

            if directive.property == "dynamic":
                properties.discard("static")
                properties.add("dynamic")
            else:
                properties.add(directive.property)
                if "dynamic" not in properties:
                    properties.add("static")

            # Update module-scoped properties
            self._set_module_predicate_properties(module_name, key, properties)
            
            # Also update global for backward compatibility
            self.predicate_properties.setdefault(key, set()).update(properties)

            if directive.property == "discontiguous" and key in closed_predicates:
                closed_predicates.discard(key)

    def _validate_predicate_indicator(
        self, indicator: PredicateIndicator | Any, context: str
    ) -> tuple[str, int]:
        """Validate a predicate indicator term."""

        if isinstance(indicator, PredicateIndicator):
            name_term = indicator.name
            arity_term = indicator.arity
        elif (
            isinstance(indicator, Compound)
            and indicator.functor == "/"
            and len(indicator.args) == 2
        ):
            name_term, arity_term = indicator.args
        else:
            error_term = PrologError.type_error("predicate_indicator", indicator, context)
            raise PrologThrow(error_term)

        if isinstance(name_term, Variable) or isinstance(arity_term, Variable):
            error_term = PrologError.instantiation_error(context)
            raise PrologThrow(error_term)

        normalized_name = self._normalize_operator_in_indicator(name_term)
        if not isinstance(normalized_name, Atom):
            error_term = PrologError.type_error("atom", name_term, context)
            raise PrologThrow(error_term)

        if not isinstance(arity_term, Number):
            error_term = PrologError.type_error("integer", arity_term, context)
            raise PrologThrow(error_term)

        if not isinstance(arity_term.value, int):
            error_term = PrologError.type_error("integer", arity_term, context)
            raise PrologThrow(error_term)

        if arity_term.value < 0:
            error_term = PrologError.domain_error("not_less_than_zero", arity_term, context)
            raise PrologThrow(error_term)

        return normalized_name.name, int(arity_term.value)

    def _reconstruct_operator_name(self, term):
        """Reconstruct an operator name from its AST representation as a compound term."""
        return reconstruct_operator_name_from_term(term)

    def _normalize_operator_in_indicator(self, term):
        """Normalize operator applications in predicate indicators to atoms."""
        if isinstance(term, Atom):
            return term
        # Handle parenthesized operators like (,) which are parsed as compounds with no args
        if isinstance(term, Compound) and not term.args:
            return Atom(term.functor)
        reconstructed = self._reconstruct_operator_name(term)
        if reconstructed is not None:
            return Atom(reconstructed)
        return term


    def _indicator_from_key(self, key: tuple[str, int]) -> Compound:
        name, arity = key
        return Compound("/", (Atom(name), Number(arity)))

    def _get_module_predicate_properties(
        self, module_name: str, key: tuple[str, int]
    ) -> set[str]:
        """Get predicate properties for a specific module. Returns an empty set if not found."""
        # First check if it's a built-in (global)
        if key in self.predicate_properties:
            props = self.predicate_properties[key]
            if "built_in" in props:
                return props

        # Check module-specific properties
        module_props = self._module_predicate_properties.get(module_name, {})
        if key in module_props:
            return module_props[key]

        return set()

    def _set_module_predicate_properties(
        self, module_name: str, key: tuple[str, int], properties: set[str]
    ) -> None:
        """Set predicate properties for a specific module."""
        self._module_predicate_properties.setdefault(module_name, {})[key] = properties

    def _get_module_predicate_sources(
        self, module_name: str, key: tuple[str, int]
    ) -> set[str]:
        """Get predicate sources for a specific module."""
        module_sources = self._module_predicate_sources.setdefault(module_name, {})
        return module_sources.get(key, set())

    def _add_module_predicate_source(
        self, module_name: str, key: tuple[str, int], source: str
    ) -> None:
        """Add a source to a predicate's source set in a specific module."""
        module_sources = self._module_predicate_sources.setdefault(module_name, {})
        module_sources.setdefault(key, set()).add(source)

    def _extract_character(self, term: Any, context: str) -> str:
        """Extract a single character from a term for char_conversion.
        
        The term must be a single-character atom.
        Raises type_error(character, term) if invalid.
        """
        if isinstance(term, Atom):
            if len(term.name) == 1:
                return term.name
        # Not a single-character atom
        error_term = PrologError.type_error("character", term, context)
        raise PrologThrow(error_term)

    def _add_clause(
        self,
        clause: Clause,
        source_name: str,
        closed_predicates: set[tuple[str, int]],
        last_predicate: tuple[str, int] | None,
    ) -> tuple[str, int] | None:
        """Insert a clause while enforcing predicate properties.

        Supports module-qualified clause heads like `Module:Head :- Body`.
        When the head is a `:/2` compound, the clause is added to the
        specified module rather than the current module.
        """

        head = clause.head
        is_cross_module_definition = False  # Track if this is a module-qualified head

        # Handle module-qualified clause heads (Module:Head :- Body)
        if isinstance(head, Compound) and head.functor == ":" and len(head.args) == 2:
            module_term = head.args[0]
            actual_head = head.args[1]

            # Module must be an atom - raise appropriate errors for invalid types
            if isinstance(module_term, Variable):
                # Variable module specifier is an instantiation error
                raise PrologThrow(
                    PrologError.instantiation_error("consult/1")
                )
            elif not isinstance(module_term, Atom):
                # Non-atom module specifier (e.g., number, compound) is a type error
                raise PrologThrow(
                    PrologError.type_error("atom", module_term, "consult/1")
                )

            # Valid atom module specifier - modify clause in-place
            head = actual_head
            clause.head = actual_head
            clause.module = module_term.name
            is_cross_module_definition = True

        # Compute key from (possibly updated) head
        if isinstance(head, Compound):
            key = (head.functor, len(head.args))
        elif isinstance(head, Atom):
            key = (head.name, 0)
        else:
            return last_predicate

        # Get the module for this clause (set above for module-qualified heads,
        # or from _process_items, or fall back to current_module)
        module_name = getattr(clause, "module", self.current_module)

        # Check if module already exists before potentially creating it
        module_existed = module_name in self.modules

        # Register clause under module if present
        self.modules.setdefault(module_name, Module(module_name, set()))
        mod = self.modules[module_name]

        # If this is a cross-module definition (Module:Head) and the module was
        # just created, export the predicate so it can be called. However, if
        # the module already existed with a declared export list, respect that
        # interface and don't auto-export (the module author controls exports).
        if is_cross_module_definition and not module_existed:
            mod.exports.add(key)

        # Check if it's a built-in (global check)
        global_properties = self.predicate_properties.get(key, set())
        if "built_in" in global_properties:
            if self.builtin_conflict == "skip":
                # Silently skip the library definition and use the existing built-in
                return last_predicate
            elif self.builtin_conflict == "error":
                indicator = self._indicator_from_key(key)
                error_term = PrologError.permission_error(
                    "modify", "static_procedure", indicator, "consult/1"
                )
                raise PrologThrow(error_term)
            elif self.builtin_conflict == "shadow":
                # Allow the module to define a shadowing predicate
                # Mark it as shadowed in the module
                mod.shadowed_builtins.add(key)

        # Get module-scoped properties
        properties = self._get_module_predicate_properties(module_name, key)
        if not properties:
            # Initialize with default 'static' property for this module
            properties = {"static"}
            self._set_module_predicate_properties(module_name, key, properties)
        
        if "dynamic" in properties:
            properties.discard("static")

        # Check sources within the same module
        sources = self._get_module_predicate_sources(module_name, key)
        if "static" in properties and sources and source_name not in sources and "multifile" not in properties:
            indicator = self._indicator_from_key(key)
            error_term = PrologError.permission_error(
                "modify", "static_procedure", indicator, "consult/1"
            )
            raise PrologThrow(error_term)

        if (
            key in closed_predicates
            and "discontiguous" not in properties
        ):
            indicator = self._indicator_from_key(key)
            error_term = PrologError.permission_error(
                "modify", "static_procedure", indicator, "consult/1"
            )
            raise PrologThrow(error_term)

        if last_predicate is not None and last_predicate != key:
            last_properties = self._get_module_predicate_properties(module_name, last_predicate)
            if not last_properties:
                last_properties = {"static"}
            if "discontiguous" not in last_properties:
                closed_predicates.add(last_predicate)

        self.clauses.append(clause)
        self._add_module_predicate_source(module_name, key, source_name)
        
        # Also update global tracking for backward compatibility
        self.predicate_properties.setdefault(key, set()).update(properties)
        self._predicate_sources.setdefault(key, set()).add(source_name)

        mod.predicates.setdefault(key, []).append(clause)

        return key

    def _execute_initialization_goals(self):
        """Execute collected initialization goals."""
        try:
            for goal in self.initialization_goals:
                # Execute the goal, but ignore solutions since initialization is for side effects
                # Use _solve_goals directly to allow exceptions to propagate (query catches them)
                list(self.engine._solve_goals([goal], Substitution()))
        finally:
            self.initialization_goals.clear()  # Clear after execution

    def _consult_code(self, prolog_code: str, source_name: str):
        """
        Process Prolog code: parse, process items, create engine, and run initialization goals.

        Args:
            prolog_code: String containing Prolog code to process
        """
        # Reset module context to `user` at start of a consult
        self.current_module = "user"
        # Ensure a default user module exists
        self.modules.setdefault("user", Module("user", None))
        self._current_source_path = self._source_path_from_name(source_name)

        try:
            local_directives = extract_op_directives(prolog_code)
            imported_ops = self._collect_imported_operators(
                prolog_code, source_name, local_directives
            )
            directive_ops = imported_ops + local_directives
        except ValueError as exc:
            error_term = PrologError.syntax_error(str(exc), "consult/1")
            raise PrologThrow(error_term)

        # Parse and process incrementally to support char_conversion taking effect
        # between clauses/directives. We split by period-terminated statements.
        all_items = []
        try:
            chunks = self._split_clauses(prolog_code)
        except ValueError as exc:
            error_term = PrologError.syntax_error(str(exc), "consult/1")
            raise PrologThrow(error_term)
        
        # Keep closed_predicates across chunks to enforce discontiguous requirements
        closed_predicates: set[tuple[str, int]] = set()
        last_predicate: tuple[str, int] | None = None
        
        for chunk in chunks:
            chunk = chunk.strip()
            if not chunk:
                continue
            try:
                # char_conversion directives should not be affected by char conversions
                # since they need to be parsed as-is to set the conversions
                is_char_conversion = re.match(r"\s*:-\s*char_conversion\b", chunk)
                items = self.parser.parse(
                    chunk,
                    "consult/1",
                    apply_char_conversions=not is_char_conversion,
                    directive_ops=directive_ops,
                    module_name=self.current_module,
                )
            except (ValueError, LarkError) as exc:
                error_term = PrologError.syntax_error(str(exc), "consult/1")
                raise PrologThrow(error_term)
            # Process each item immediately so char_conversion directives
            # take effect before subsequent parsing
            last_predicate = self._process_items(items, source_name, closed_predicates, last_predicate)
            all_items.extend(items)

        # Check for unclosed conditional directives
        if self._conditional_stack:
            error_term = PrologError.syntax_error(
                f"unclosed if directive ({len(self._conditional_stack)} level(s) deep)",
                "consult/1"
            )
            # Clear the stack before raising to allow recovery
            self._conditional_stack.clear()
            raise PrologThrow(error_term)

        self.engine = PrologEngine(
            self.clauses,
            self.argv,
            self.predicate_properties,
            self._predicate_sources,
            self.predicate_docs,
            operator_table=self.operator_table,
            max_depth=self.max_recursion_depth,
            tabled_predicates=self._tabled_predicates,
        )
        # Expose interpreter to engine for module-aware resolution
        self.engine.interpreter = self
        self._execute_initialization_goals()
        self._current_source_path = None

    def _split_clauses(self, prolog_code: str) -> list[str]:
        """Split Prolog code into individual clause/directive strings.

        Handles quoted strings and comments to avoid splitting inside them.
        Each returned string ends with a period.
        """
        from vibeprolog.parser import tokenize_prolog_statements
        return tokenize_prolog_statements(prolog_code)

    def _resolve_consult_target(self, target: str | Path) -> Path:
        """Resolve consult/1 input which may be a filesystem path or library spec."""
        if isinstance(target, Path):
            return target
        if isinstance(target, str):
            trimmed = target.strip()
            if trimmed.startswith("library(") and trimmed.endswith(")"):
                term = self.parser.parse_term(trimmed, "consult/1")
                resolved = self._resolve_module_file(term, "consult/1")
                return Path(resolved)
            return Path(trimmed)
        return Path(target)

    def consult(self, filepath: str | Path):
        """Load Prolog clauses from a file."""
        resolved_path = self._resolve_consult_target(filepath)
        with open(resolved_path, "r") as f:
            content = f.read()
        self._consult_counter += 1
        source_name = f"file:{resolved_path}#{self._consult_counter}"
        self._consult_code(content, source_name)

    def consult_string(self, prolog_code: str):
        """Load Prolog clauses from a string."""
        self._consult_counter += 1
        source_name = f"string:{self._consult_counter}"
        self._consult_code(prolog_code, source_name)

    def query(
        self, query_str: str, limit: int | None = None, capture_output: bool = False
    ) -> list[dict[str, any]] | tuple[list[dict[str, any]], bool]:
        """
        Execute a Prolog query and return all solutions.

        Args:
            query_str: Prolog query string (without ?-)
            limit: Maximum number of solutions to return (None for all)
            capture_output: If True, return tuple of (solutions, had_output)

        Returns:
            List of dictionaries mapping variable names to their values
            If capture_output=True, returns (solutions, had_output) where had_output is True if stdout was produced
        """
        if self.engine is None:
            # Initialize empty engine for built-in predicates
            self.engine = PrologEngine(
                self.clauses,
                self.argv,
                self.predicate_properties,
                self._predicate_sources,
                self.predicate_docs,
                operator_table=self.operator_table,
                max_depth=self.max_recursion_depth,
                tabled_predicates=self._tabled_predicates,
            )
            self.engine.interpreter = self

        # Parse the query
        goals = self._parse_query(query_str)

        # Collect variable names from the query
        var_names = self._collect_variables(goals)

        # Capture stdout if requested
        if capture_output:
            old_stdout = sys.stdout
            captured_output = io.StringIO()
            sys.stdout = captured_output

        # Execute query
        solutions = []
        # Reset depth tracking for new query
        if self.engine is not None:
            self.engine.call_depth = 0
        # Preserve current module context for the engine
        if hasattr(self, "engine") and self.engine is not None:
            self.engine.current_module = self.current_module
        try:
            for i, subst in enumerate(self.engine.query(goals)):
                if limit is not None and i >= limit:
                    break

                # Build solution dictionary
                solution = {}
                for var_name in var_names:
                    var = Variable(var_name)
                    value = apply_substitution(var, subst)
                    solution[var_name] = self._term_to_python(value)

                solutions.append(solution)
        except CutException:
            # Cut can bubble up after yielding committed results; treat it as end-of-search
            pass
        finally:
            if capture_output:
                sys.stdout = old_stdout
                output_text = captured_output.getvalue()
                had_output = len(output_text) > 0
                # Print the captured output
                if had_output:
                    print(output_text, end="")

        if capture_output:
            return solutions, had_output
        return solutions

    def query_once(
        self, query_str: str, capture_output: bool = False
    ) -> dict[str, any] | None | tuple[dict[str, any] | None, bool]:
        """
        Execute a Prolog query and return first solution.

        Args:
            query_str: Prolog query string
            capture_output: If True, return tuple of (solution, had_output)

        Returns:
            None if no solution found.
            If capture_output=False: dict of variable bindings or None
            If capture_output=True: (dict or None, had_output)
        """
        if capture_output:
            solutions, had_output = self.query(query_str, limit=1, capture_output=True)
            return (solutions[0] if solutions else None, had_output)
        else:
            solutions = self.query(query_str, limit=1)
            return solutions[0] if solutions else None

    def has_solution(self, query_str: str) -> bool:
        """Return True if the query has at least one solution; False otherwise.
        If a PrologThrow is raised (e.g., domain_error for arg/3 with 0),
        treat it as "no solution" for compatibility with existing tests.
        """
        try:
            result = self.query_once(query_str)
            return result is not None
        except PrologThrow:
            # Do not propagate the error for has_solution; report no solution instead
            return False

    def _parse_query(self, query_str: str) -> list[Compound]:
        """Parse a query string into a list of goals."""
        # Add ?- and . to make it a valid query
        if not query_str.strip().endswith("."):
            query_str = query_str.strip() + "."

        # Parse as a fact and extract the goals
        # We'll use a dummy rule structure
        prolog_code = f"dummy :- {query_str}"
        try:
            # Don't apply char conversions to interactive queries
            # Use "user" module context for top-level queries - module-scoped
            # operators in other modules should not affect user queries
            clauses = self.parser.parse(
                prolog_code,
                "query/1",
                apply_char_conversions=False,
                module_name="user",
            )
        except (ValueError, LarkError) as exc:
            self._raise_syntax_error(exc, "query/1")

        if clauses and clauses[0].body:
            # The parser already returns body as a flattened list of goals
            return clauses[0].body

        # Single goal case
        prolog_code = query_str
        try:
            # Don't apply char conversions to interactive queries
            # Use "user" module context for top-level queries
            clauses = self.parser.parse(
                prolog_code,
                "query/1",
                apply_char_conversions=False,
                module_name="user",
            )
        except (ValueError, LarkError) as exc:
            self._raise_syntax_error(exc, "query/1")
        if clauses:
            return [clauses[0].head]

        raise ValueError(f"Failed to parse query: {query_str}")

    def _flatten_conjunction(self, term) -> list[Compound]:
        """Flatten a conjunction (,) into a list of goals."""
        if isinstance(term, Compound) and term.functor == ",":
            # Recursively flatten left and right
            left = self._flatten_conjunction(term.args[0])
            right = self._flatten_conjunction(term.args[1])
            return left + right
        else:
            return [term]

    def _collect_variables(self, goals: list) -> set[str]:
        """Collect all variable names from goals."""
        from vibeprolog.parser import List

        variables = set()

        def collect_from_term(term):
            if isinstance(term, Variable):
                if not term.name.startswith("_"):  # Skip anonymous variables
                    variables.add(term.name)
            elif isinstance(term, Compound):
                for arg in term.args:
                    collect_from_term(arg)
            elif isinstance(term, List):
                for elem in term.elements:
                    collect_from_term(elem)
                if term.tail is not None:
                    collect_from_term(term.tail)
            elif isinstance(term, list):
                for item in term:
                    collect_from_term(item)

        for goal in goals:
            collect_from_term(goal)

        return variables

    def _term_to_python(self, term) -> any:
        """Convert a Prolog term to a Python value."""
        from vibeprolog.parser import List
        from vibeprolog.terms import Atom, Number, Compound, Variable

        if isinstance(term, Atom):
            return term.name
        elif isinstance(term, Number):
            return term.value
        elif isinstance(term, Variable):
            return f"_{term.name}"  # Unbound variable
        elif isinstance(term, List):
            # Convert list to Python list
            result = [self._term_to_python(elem) for elem in term.elements]
            if term.tail is not None and not (
                isinstance(term.tail, List) and not term.tail.elements
            ):
                # List with tail
                tail_val = self._term_to_python(term.tail)
                if isinstance(tail_val, list):
                    result.extend(tail_val)
                else:
                    result.append(("|", tail_val))
            return result
        elif isinstance(term, Compound):
            # Convert compound to tuple or dict
            if not term.args:
                return term.functor
            args = [self._term_to_python(arg) for arg in term.args]
            return {term.functor: args}
        else:
            return str(term)
