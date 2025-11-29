"""Main Prolog interpreter interface."""

import io
import sys
from pathlib import Path
from typing import Any

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
)
from vibeprolog.operators import OperatorTable
from vibeprolog.terms import Atom, Compound, Number, Variable
from vibeprolog.unification import Substitution, apply_substitution
from vibeprolog.dcg import expand_dcg_clause


class Module:
    def __init__(self, name: str, exports: set[tuple[str, int]] | None):
        self.name = name
        self.exports = exports if exports is not None else set()
        self.predicates: dict[tuple[str, int], list] = {}
        self.file: str | None = None
        # Import table: (functor, arity) -> source_module_name
        self.imports: dict[tuple[str, int], str] = {}


class PrologInterpreter:
    """Main interface for the Prolog interpreter."""

    def __init__(self, argv: list[str] | None = None, max_recursion_depth: int = 400) -> None:
        self.operator_table = OperatorTable()
        self.parser = PrologParser(self.operator_table)
        self.clauses = []
        # Module system
        self.modules: dict[str, "Module"] = {}
        self.current_module: str = "user"

        self._argv: list[str] = argv or []
        self.max_recursion_depth = max_recursion_depth
        self.engine = None
        self.initialization_goals = []
        self.predicate_properties: dict[tuple[str, int], set[str]] = {}
        self._predicate_sources: dict[tuple[str, int], set[str]] = {}
        self.predicate_docs: dict[tuple[str, int], str] = {}
        self._consult_counter = 0
        self._builtins_seeded = False

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

    def _process_items(self, items: list, source_name: str):
        """Process parsed clauses and directives."""
        self._ensure_builtin_properties()
        closed_predicates: set[tuple[str, int]] = set()
        last_predicate: tuple[str, int] | None = None

        for item in items:
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

    def _handle_directive(
        self, directive: Directive, closed_predicates: set[tuple[str, int]], source_name=None
    ):
        """Handle a directive."""
        goal = directive.goal

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

            if isinstance(exports_term, ParserList):
                for elt in exports_term.elements:
                    # Each elt should be Name/Arity (Compound "/")
                    if isinstance(elt, Compound) and elt.functor == "/" and len(elt.args) == 2:
                        name_arg, arity_arg = elt.args
                        if not isinstance(name_arg, Atom) or not isinstance(arity_arg, Number):
                            error_term = PrologError.type_error("predicate_indicator", elt, "module/2")
                            raise PrologThrow(error_term)
                        exports.add((name_arg.name, int(arity_arg.value)))
                    else:
                        error_term = PrologError.type_error("predicate_indicator", elt, "module/2")
                        raise PrologThrow(error_term)
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

            # Resolve the module file
            module_file = self._resolve_module_file(file_term, "use_module/1,2")

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
            else:
                # Import specific predicates
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
            self.operator_table.define(prec_term, spec_term, name_term, "op/3")
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
        # Other directives can be added here

    def _parse_import_list(self, imports_term, context: str) -> set[tuple[str, int]]:
        """Parse import list for use_module/2."""
        imports = set()
        if isinstance(imports_term, ParserList):
            for elt in imports_term.elements:
                # Each elt should be Name/Arity (Compound "/")
                if isinstance(elt, Compound) and elt.functor == "/" and len(elt.args) == 2:
                    name_arg, arity_arg = elt.args
                    if not isinstance(name_arg, Atom) or not isinstance(arity_arg, Number):
                        error_term = PrologError.type_error("predicate_indicator", elt, context)
                        raise PrologThrow(error_term)
                    imports.add((name_arg.name, int(arity_arg.value)))
                else:
                    error_term = PrologError.type_error("predicate_indicator", elt, context)
                    raise PrologThrow(error_term)
        else:
            error_term = PrologError.type_error("list", imports_term, context)
            raise PrologThrow(error_term)
        return imports

    def _resolve_module_file(self, file_term, context: str) -> str:
        """Resolve module file path."""
        if isinstance(file_term, Atom):
            module_name = file_term.name
            # Direct file path
            if not Path(module_name).exists():
                error_term = PrologError.existence_error("file", file_term, context)
                raise PrologThrow(error_term)
            return module_name
        elif isinstance(file_term, Compound) and file_term.functor == "library" and len(file_term.args) == 1:
            # library(Name) syntax
            lib_term = file_term.args[0]
            if not isinstance(lib_term, Atom):
                error_term = PrologError.type_error("atom", lib_term, context)
                raise PrologThrow(error_term)
            lib_name = lib_term.name
            # Look in examples/modules/ first, then library/
            candidates = [
                f"examples/modules/{lib_name}.pl",
                f"library/{lib_name}.pl"
            ]
            for candidate in candidates:
                if Path(candidate).exists():
                    return candidate
            error_term = PrologError.existence_error("file", file_term, context)
            raise PrologThrow(error_term)
        else:
            error_term = PrologError.type_error("atom", file_term, context)
            raise PrologThrow(error_term)

    def _load_module_from_file(self, filepath: str) -> str:
        """Load a module from file and return its name."""
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

    def _handle_predicate_property_directive(
        self, directive: PredicatePropertyDirective, closed_predicates: set[tuple[str, int]]
    ) -> None:
        """Apply a predicate property directive."""

        context = f"{directive.property}/1"
        for indicator in directive.indicators:
            key = self._validate_predicate_indicator(indicator, context)
            properties = self.predicate_properties.setdefault(key, set())

            if "built_in" in properties and directive.property == "dynamic":
                indicator_term = self._indicator_from_key(key)
                error_term = PrologError.permission_error(
                    "modify", "static_procedure", indicator_term, context
                )
                raise PrologThrow(error_term)

            if directive.property == "dynamic":
                properties.discard("static")
                properties.add("dynamic")
            else:
                properties.add(directive.property)
                if "dynamic" not in properties:
                    properties.add("static")

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

        if not isinstance(name_term, Atom):
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

        return name_term.name, int(arity_term.value)

    def _indicator_from_key(self, key: tuple[str, int]) -> Compound:
        name, arity = key
        return Compound("/", (Atom(name), Number(arity)))

    def _add_clause(
        self,
        clause: Clause,
        source_name: str,
        closed_predicates: set[tuple[str, int]],
        last_predicate: tuple[str, int] | None,
    ) -> tuple[str, int] | None:
        """Insert a clause while enforcing predicate properties."""

        head = clause.head
        if isinstance(head, Compound):
            key = (head.functor, len(head.args))
        elif isinstance(head, Atom):
            key = (head.name, 0)
        else:
            return last_predicate

        properties = self.predicate_properties.setdefault(key, {"static"})
        if "dynamic" in properties:
            properties.discard("static")

        if "built_in" in properties:
            indicator = self._indicator_from_key(key)
            error_term = PrologError.permission_error(
                "modify", "static_procedure", indicator, "consult/1"
            )
            raise PrologThrow(error_term)

        sources = self._predicate_sources.setdefault(key, set())
        if "static" in properties and sources and source_name not in sources and "multifile" not in properties:
            indicator = self._indicator_from_key(key)
            error_term = PrologError.permission_error(
                "modify", "static_procedure", indicator, "consult/1"
            )
            raise PrologThrow(error_term)

        if (
            key in closed_predicates
            and "discontiguous" not in properties
            and source_name in sources
        ):
            indicator = self._indicator_from_key(key)
            error_term = PrologError.permission_error(
                "modify", "static_procedure", indicator, "consult/1"
            )
            raise PrologThrow(error_term)

        if last_predicate is not None and last_predicate != key:
            last_properties = self.predicate_properties.get(last_predicate, {"static"})
            if "discontiguous" not in last_properties:
                closed_predicates.add(last_predicate)

        self.clauses.append(clause)
        sources.add(source_name)

        # Register clause under module if present
        module_name = getattr(clause, "module", "user")
        self.modules.setdefault(module_name, Module(module_name, set()))

        mod = self.modules[module_name]
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
        try:
            items = self.parser.parse(prolog_code, "consult/1")
        except (ValueError, LarkError) as exc:
            error_term = PrologError.syntax_error(str(exc), "consult/1")
            raise PrologThrow(error_term)
        # Reset module context to `user` at start of a consult
        self.current_module = "user"
        # Ensure a default user module exists
        self.modules.setdefault("user", Module("user", None))

        self._process_items(items, source_name)
        self.engine = PrologEngine(
            self.clauses,
            self.argv,
            self.predicate_properties,
            self._predicate_sources,
            self.predicate_docs,
            operator_table=self.operator_table,
            max_depth=self.max_recursion_depth,
        )
        # Expose interpreter to engine for module-aware resolution
        self.engine.interpreter = self
        self._execute_initialization_goals()

    def consult(self, filepath: str | Path):
        """Load Prolog clauses from a file."""
        filepath = Path(filepath)
        with open(filepath, "r") as f:
            content = f.read()
        self._consult_counter += 1
        source_name = f"file:{filepath}#{self._consult_counter}"
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
            clauses = self.parser.parse(prolog_code, "query/1")
        except (ValueError, LarkError) as exc:
            self._raise_syntax_error(exc, "query/1")

        if clauses and clauses[0].body:
            # The parser already returns body as a flattened list of goals
            return clauses[0].body

        # Single goal case
        prolog_code = query_str
        try:
            clauses = self.parser.parse(prolog_code, "query/1")
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
