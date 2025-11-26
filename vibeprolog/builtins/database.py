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

        if position == "front":
            engine.clauses.insert(0, new_clause)
        else:
            engine.clauses.append(new_clause)

        # Update the predicate index
        engine._add_predicate_to_index(new_clause)

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
                    retracted_predicates.add((head.functor, len(head.args)))
                elif isinstance(head, Atom):
                    retracted_predicates.add((head.name, 0))

        for i, new_subst in reversed(matches):
            engine.clauses.pop(i)
            yield new_subst

        # Update the index for retracted predicates
        for functor, arity in retracted_predicates:
            engine._remove_predicate_from_index_if_empty(functor, arity)

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

        # Update the index if clauses were removed
        if had_clauses:
            engine._predicate_index.discard((name, arity))

        return subst


__all__ = ["DatabaseBuiltins"]
