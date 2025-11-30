"""Database built-ins (clause/2, assert*/1, retract/1, abolish/1).

Provides ISO-style dynamic database manipulation predicates.
"""

from __future__ import annotations

from typing import Iterator

from vibeprolog.builtins import BuiltinRegistry, register_builtin
from vibeprolog.builtins.common import BuiltinArgs, EngineContext
from vibeprolog.parser import Clause
from vibeprolog.terms import Atom, Compound, Number
from vibeprolog.unification import Substitution, apply_substitution, deref, unify


class DatabaseBuiltins:
    """Built-ins for interacting with the clause database."""

    @staticmethod
    def register(registry: BuiltinRegistry, _engine: EngineContext | None) -> None:
        """Register database predicates into the registry."""
        register_builtin(registry, "clause", 2, DatabaseBuiltins._builtin_clause)
        register_builtin(registry, "asserta", 1, DatabaseBuiltins._builtin_asserta)
        register_builtin(registry, "assertz", 1, DatabaseBuiltins._builtin_assertz)
        register_builtin(registry, "assert", 1, DatabaseBuiltins._builtin_assertz)
        register_builtin(registry, "retract", 1, DatabaseBuiltins._builtin_retract)
        register_builtin(registry, "retractall", 1, DatabaseBuiltins._builtin_retractall)
        register_builtin(registry, "abolish", 1, DatabaseBuiltins._builtin_abolish)

    @staticmethod
    def _builtin_clause(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        head, body = args
        head = deref(head, subst)

        for clause in engine.clauses:
            renamed_clause = engine._rename_variables(clause)
            new_subst = unify(head, renamed_clause.head, subst)
            if new_subst is not None:
                if renamed_clause.is_fact():
                    body_term = Atom("true")
                else:
                    if len(renamed_clause.body) == 1:
                        body_term = renamed_clause.body[0]
                    else:
                        body_term = renamed_clause.body[0]
                        for goal in renamed_clause.body[1:]:
                            body_term = Compound(",", (body_term, goal))

                final_subst = unify(body, body_term, new_subst)
                if final_subst is not None:
                    yield final_subst

    @staticmethod
    def _builtin_asserta(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Substitution | None:
        return DatabaseBuiltins._builtin_assert(args, subst, engine, position="front")

    @staticmethod
    def _builtin_assertz(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Substitution | None:
        return DatabaseBuiltins._builtin_assert(args, subst, engine, position="back")

    @staticmethod
    def _builtin_assert(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext, position: str
    ) -> Substitution | None:
        clause_term = deref(args[0], subst)
        clause_term = apply_substitution(clause_term, subst)

        if (
            isinstance(clause_term, Compound)
            and clause_term.functor == ":-"
            and len(clause_term.args) == 2
        ):
            head = clause_term.args[0]
            body_term = clause_term.args[1]
            body = engine._flatten_conjunction(body_term)
            new_clause = Clause(head, body)
        else:
            new_clause = Clause(clause_term, None)

        # Set module for runtime asserted clauses
        new_clause.module = 'user'

        head = new_clause.head
        if isinstance(head, Compound):
            key = (head.functor, len(head.args))
        elif isinstance(head, Atom):
            key = (head.name, 0)
        else:
            return None

        context = "asserta/1" if position == "front" else "assertz/1"
        engine._ensure_dynamic_permission(key, context)

        if position == "front":
            engine.clauses.insert(0, new_clause)
            clause_index = 0
            # Shift existing indices in the first-arg index
            for index_list in engine._first_arg_index.values():
                for i in range(len(index_list)):
                    if index_list[i] >= 0:
                        index_list[i] += 1
            for index_list in engine._variable_first_arg_clauses.values():
                for i in range(len(index_list)):
                    if index_list[i] >= 0:
                        index_list[i] += 1
        else:
            engine.clauses.append(new_clause)
            clause_index = len(engine.clauses) - 1

        # Update the predicate index
        engine._add_predicate_to_index(new_clause)
        engine._record_predicate_source(key, "runtime")

        # Update the first-argument index
        engine._add_clause_to_index(new_clause, clause_index)

        # Add to user module predicates
        if hasattr(engine, 'interpreter') and engine.interpreter:
            user_mod = engine.interpreter.modules.get('user')
            if user_mod:
                pred_list = user_mod.predicates.setdefault(key, [])
                if position == "front":
                    pred_list.insert(0, new_clause)
                else:
                    pred_list.append(new_clause)

        return subst

    @staticmethod
    def _builtin_retract(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        clause_term = deref(args[0], subst)
        matches = []
        retracted_predicates = set()  # Track predicates that were retracted

        for i, clause in enumerate(engine.clauses):
            renamed_clause = engine._rename_variables(clause)

            if renamed_clause.is_fact():
                clause_as_term = renamed_clause.head
            else:
                if len(renamed_clause.body) == 1:
                    body_term = renamed_clause.body[0]
                else:
                    body_term = renamed_clause.body[0]
                    for goal in renamed_clause.body[1:]:
                        body_term = Compound(",", (body_term, goal))
                clause_as_term = Compound(":-", (renamed_clause.head, body_term))

            new_subst = unify(clause_term, clause_as_term, subst)
            if new_subst is not None:
                matches.append((i, new_subst))
                # Track which predicate is being retracted
                head = clause.head
                if isinstance(head, Compound):
                    key = (head.functor, len(head.args))
                elif isinstance(head, Atom):
                    key = (head.name, 0)
                else:
                    continue
                retracted_predicates.add(key)

        for key in retracted_predicates:
            engine._ensure_dynamic_permission(key, "retract/1")

        for i, new_subst in reversed(matches):
            clause = engine.clauses[i]
            # Remove from first-argument index before removing from clauses
            engine._remove_clause_from_index(clause, i)
            engine.clauses.pop(i)

            # Update indices for clauses that come after this one
            for index_list in engine._first_arg_index.values():
                for j in range(len(index_list)):
                    if index_list[j] > i:
                        index_list[j] -= 1
            for index_list in engine._variable_first_arg_clauses.values():
                for j in range(len(index_list)):
                    if index_list[j] > i:
                        index_list[j] -= 1

            # Also remove from module predicates
            if hasattr(engine, 'interpreter') and engine.interpreter:
                module_name = getattr(clause, 'module', 'user')
                mod = engine.interpreter.modules.get(module_name)
                head = clause.head
                clause_key = None
                if isinstance(head, Compound):
                    clause_key = (head.functor, len(head.args))
                elif isinstance(head, Atom):
                    clause_key = (head.name, 0)

                if mod and clause_key and clause_key in mod.predicates:
                    if clause in mod.predicates[clause_key]:
                        mod.predicates[clause_key].remove(clause)
            yield new_subst

        # Update the index for retracted predicates
        for functor, arity in retracted_predicates:
            engine._remove_predicate_from_index_if_empty(functor, arity)

    @staticmethod
    def _builtin_retractall(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Substitution | None:
        """Retract all clauses matching the given pattern.

        Unlike retract/1, this predicate:
        - Removes all matching clauses at once
        - Always succeeds (even if no clauses match)
        - Is deterministic (succeeds exactly once, no backtracking)
        - Matches only on the clause head (not the body)
        """
        clause_term = deref(args[0], subst)

        # Handle module-qualified patterns: Module:Head
        target_module = None
        if (
            isinstance(clause_term, Compound)
            and clause_term.functor == ":"
            and len(clause_term.args) == 2
        ):
            module_term = clause_term.args[0]
            if isinstance(module_term, Atom):
                target_module = module_term.name
            clause_term = clause_term.args[1]

        # Handle predicate indicator form: Name/Arity (e.g., retractall(foo/2))
        # Convert to a pattern with fresh variables
        if (
            isinstance(clause_term, Compound)
            and clause_term.functor == "/"
            and len(clause_term.args) == 2
        ):
            name_term = deref(clause_term.args[0], subst)
            arity_term = deref(clause_term.args[1], subst)
            if isinstance(name_term, Atom) and isinstance(arity_term, Number):
                functor_name = name_term.name
                arity = int(arity_term.value)
                if arity == 0:
                    # Zero-arity: just the atom
                    clause_term = Atom(functor_name)
                else:
                    # Create a pattern with fresh variables
                    from vibeprolog.terms import Variable
                    var_args = tuple(Variable(f"_G{i}") for i in range(arity))
                    clause_term = Compound(functor_name, var_args)

        # Extract the head pattern from the argument
        # retractall(Head) matches clauses whose head unifies with Head
        # retractall((Head :- Body)) matches full clause terms like retract/1
        if (
            isinstance(clause_term, Compound)
            and clause_term.functor == ":-"
            and len(clause_term.args) == 2
        ):
            # Full clause pattern: retractall((head :- body))
            head_pattern = clause_term.args[0]
            body_pattern = clause_term.args[1]
            match_body = True
        else:
            # Head-only pattern: retractall(head)
            head_pattern = clause_term
            body_pattern = None
            match_body = False

        matches = []
        retracted_predicates = set()

        for i, clause in enumerate(engine.clauses):
            # If a target module is specified, only match clauses from that module
            if target_module is not None:
                clause_module = getattr(clause, 'module', 'user')
                if clause_module != target_module:
                    continue

            renamed_clause = engine._rename_variables(clause)

            # Try to unify the head
            new_subst = unify(head_pattern, renamed_clause.head, subst)
            if new_subst is None:
                continue

            # If we have a body pattern, also need to match the body
            if match_body:
                if renamed_clause.is_fact():
                    clause_body = Atom("true")
                else:
                    if len(renamed_clause.body) == 1:
                        clause_body = renamed_clause.body[0]
                    else:
                        clause_body = renamed_clause.body[0]
                        for goal in renamed_clause.body[1:]:
                            clause_body = Compound(",", (clause_body, goal))

                new_subst = unify(body_pattern, clause_body, new_subst)
                if new_subst is None:
                    continue

            matches.append(i)
            # Track which predicate is being retracted
            head = clause.head
            if isinstance(head, Compound):
                key = (head.functor, len(head.args))
            elif isinstance(head, Atom):
                key = (head.name, 0)
            else:
                continue
            retracted_predicates.add(key)

        # Check dynamic permission for all matched predicates
        for key in retracted_predicates:
            engine._ensure_dynamic_permission(key, "retractall/1")

        # Remove all matching clauses in reverse order to preserve indices
        for i in reversed(matches):
            clause = engine.clauses[i]
            # Remove from first-argument index before removing from clauses
            engine._remove_clause_from_index(clause, i)
            engine.clauses.pop(i)

            # Update indices for clauses that come after this one
            for index_list in engine._first_arg_index.values():
                for j in range(len(index_list)):
                    if index_list[j] > i:
                        index_list[j] -= 1
            for index_list in engine._variable_first_arg_clauses.values():
                for j in range(len(index_list)):
                    if index_list[j] > i:
                        index_list[j] -= 1

            # Also remove from module predicates
            if hasattr(engine, 'interpreter') and engine.interpreter:
                module_name = getattr(clause, 'module', 'user')
                mod = engine.interpreter.modules.get(module_name)
                head = clause.head
                clause_key = None
                if isinstance(head, Compound):
                    clause_key = (head.functor, len(head.args))
                elif isinstance(head, Atom):
                    clause_key = (head.name, 0)

                if mod and clause_key and clause_key in mod.predicates:
                    if clause in mod.predicates[clause_key]:
                        mod.predicates[clause_key].remove(clause)

        # Update the index for retracted predicates
        for functor, arity in retracted_predicates:
            engine._remove_predicate_from_index_if_empty(functor, arity)

        # Always succeed, even if no clauses matched
        return subst

    @staticmethod
    def _builtin_abolish(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Substitution | None:
        indicator = deref(args[0], subst)

        if (
            not isinstance(indicator, Compound)
            or indicator.functor != "/"
            or len(indicator.args) != 2
        ):
            return None

        name_term, arity_term = indicator.args
        name_term = deref(name_term, subst)
        arity_term = deref(arity_term, subst)

        if not isinstance(name_term, Atom):
            return None
        if (
            not isinstance(arity_term, Number)
            or not isinstance(arity_term.value, int)
            or arity_term.value < 0
        ):
            return None
        arity = arity_term.value
        name = name_term.name

        engine._ensure_dynamic_permission((name, arity), "abolish/1")

        clauses_to_keep = []
        had_clauses = False  # Track if any clauses were removed
        for clause in engine.clauses:
            head = clause.head
            matches = False
            if isinstance(head, Compound):
                if head.functor == name and len(head.args) == arity:
                    matches = True
                    had_clauses = True
            elif isinstance(head, Atom):
                if head.name == name and arity == 0:
                    matches = True
                    had_clauses = True

            if not matches:
                clauses_to_keep.append(clause)
        engine.clauses = clauses_to_keep

        # Also remove from module predicates
        if hasattr(engine, 'interpreter') and engine.interpreter:
            for module in engine.interpreter.modules.values():
                if (name, arity) in module.predicates:
                    del module.predicates[(name, arity)]

        # Update the index if clauses were removed
        if had_clauses:
            engine._predicate_index.discard((name, arity))
            engine.predicate_sources.pop((name, arity), None)
            # Clear and rebuild first-argument index since clause indices have shifted
            engine._first_arg_index.clear()
            engine._variable_first_arg_clauses.clear()
            engine._build_first_arg_index()

        return subst


__all__ = ["DatabaseBuiltins"]
