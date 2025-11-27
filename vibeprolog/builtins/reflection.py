"""Reflection built-ins (predicate_property/2, current_predicate/1).

Expose predicate metadata such as presence in the database or built-in registry.
"""

from __future__ import annotations

from typing import Iterator

from vibeprolog.builtins import BuiltinRegistry, register_builtin
from vibeprolog.builtins.common import BuiltinArgs, EngineContext
from vibeprolog.terms import Atom, Compound, Number, Variable
from vibeprolog.unification import Substitution, deref, unify
from vibeprolog.utils.list_utils import python_to_list


class ReflectionBuiltins:
    """Built-ins that expose predicate metadata."""

    @staticmethod
    def register(registry: BuiltinRegistry, engine: EngineContext | None) -> None:
        """Register reflection predicate handlers."""
        register_builtin(
            registry,
            "predicate_property",
            2,
            ReflectionBuiltins._builtin_predicate_property,
        )
        register_builtin(
            registry,
            "current_module",
            1,
            ReflectionBuiltins._builtin_current_module,
        )
        register_builtin(
            registry,
            "module_property",
            2,
            ReflectionBuiltins._builtin_module_property,
        )
        register_builtin(
            registry,
            "current_predicate",
            1,
            ReflectionBuiltins._builtin_current_predicate,
        )
        register_builtin(
            registry,
            "argv",
            1,
            ReflectionBuiltins._builtin_argv,
        )
        register_builtin(
            registry,
            "current_prolog_flag",
            2,
            ReflectionBuiltins._builtin_current_prolog_flag,
        )
        register_builtin(
            registry,
            "predicate_documentation",
            2,
            ReflectionBuiltins._builtin_predicate_documentation,
        )

    @staticmethod
    def _builtin_predicate_property(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        goal_term, property_term = args
        goal_term = deref(goal_term, subst)
        property_term = deref(property_term, subst)

        indicator_term: Compound | None = None
        key: tuple[str, int] | None = None
        if isinstance(goal_term, Compound) and goal_term.functor == "/" and len(goal_term.args) == 2:
            name_term, arity_term = goal_term.args
            if isinstance(name_term, Atom) and isinstance(arity_term, Number):
                indicator_term = goal_term
                key = (name_term.name, int(arity_term.value))
        elif isinstance(goal_term, Compound):
            indicator_term = Compound("/", (Atom(goal_term.functor), Number(len(goal_term.args))))
            key = (goal_term.functor, len(goal_term.args))
        elif isinstance(goal_term, Atom):
            indicator_term = Compound("/", (Atom(goal_term.name), Number(0)))
            key = (goal_term.name, 0)

        if indicator_term is None or key is None:
            return iter(())

        properties = engine._get_predicate_properties(key)

        property_terms = []
        if "built_in" in properties:
            property_terms.append(Atom("built_in"))
        if "dynamic" in properties:
            property_terms.append(Compound("dynamic", (indicator_term,)))
        if "multifile" in properties:
            property_terms.append(Compound("multifile", (indicator_term,)))
        if "discontiguous" in properties:
            property_terms.append(Compound("discontiguous", (indicator_term,)))
        if "static" in properties:
            property_terms.append(Compound("static", (indicator_term,)))

        for term in property_terms:
            result = unify(property_term, term, subst)
            if result is not None:
                yield result

    @staticmethod
    def _builtin_current_predicate(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        indicator = deref(args[0], subst)

        predicates = set(engine._builtin_registry.keys())
        predicates.add(("!", 0))

        for clause in engine.clauses:
            if isinstance(clause.head, Compound):
                predicates.add((clause.head.functor, len(clause.head.args)))
            elif isinstance(clause.head, Atom):
                predicates.add((clause.head.name, 0))

        sorted_predicates = sorted(predicates)

        for name, arity in sorted_predicates:
            pred_indicator = Compound("/", (Atom(name), Number(arity)))
            new_subst = unify(indicator, pred_indicator, subst)
            if new_subst is not None:
                yield new_subst

    @staticmethod
    def _builtin_current_module(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        module_arg = args[0]
        interp = getattr(engine, "interpreter", None)
        if interp is None:
            return iter(())

        for module_name in interp.modules.keys():
            result = unify(module_arg, Atom(module_name), subst)
            if result is not None:
                yield result

    @staticmethod
    def _builtin_module_property(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        module_term, property_term = args
        module_term = deref(module_term, subst)
        property_term = deref(property_term, subst)

        interp = getattr(engine, "interpreter", None)
        if interp is None:
            return iter(())

        # Helper to yield properties for a given module
        def _yield_for_module(module_name, mod, subst_base):
            exports_list = python_to_list([Compound("/", (Atom(n), Number(a))) for (n, a) in sorted(mod.exports)])
            exp_unify = unify(property_term, Compound("exports", (exports_list,)), subst_base)
            if exp_unify is not None:
                yield exp_unify
            if mod.file is not None:
                file_unify = unify(property_term, Compound("file", (Atom(str(mod.file)),)), subst_base)
                if file_unify is not None:
                    yield file_unify

        # If module_term is a concrete atom, answer for that module only
        if isinstance(module_term, Atom):
            module_name = module_term.name
            mod = getattr(interp, "modules", {}).get(module_name)
            if mod is None:
                return iter(())
            for res in _yield_for_module(module_name, mod, subst):
                yield res
        else:
            # module_term is a variable: enumerate across all loaded modules
            for module_name, mod in getattr(interp, "modules", {}).items():
                new_subst = unify(module_term, Atom(module_name), subst)
                if new_subst is None:
                    continue
                for res in _yield_for_module(module_name, mod, new_subst):
                    yield res
        return iter(())

    @staticmethod
    def _builtin_argv(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Substitution | None:
        """argv(Args) - Unify Args with the list of command-line arguments."""
        arg_list = deref(args[0], subst)

        # Convert argv to Prolog list of atoms
        prolog_argv = ReflectionBuiltins._get_prolog_argv(engine)

        return unify(arg_list, prolog_argv, subst)

    @staticmethod
    def _get_prolog_argv(engine: EngineContext) -> any:
        """Convert engine.argv to Prolog list of atoms."""
        return python_to_list([Atom(arg) for arg in engine.argv])

    @staticmethod
    def _builtin_current_prolog_flag(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        """current_prolog_flag(Flag, Value) - Get Prolog flag values."""
        flag_term, value_term = args
        flag_term = deref(flag_term, subst)
        value_term = deref(value_term, subst)

        supported_flags = {
            "argv": ReflectionBuiltins._get_prolog_argv(engine)
        }

        if isinstance(flag_term, Variable):
            # Enumerate supported flags
            for flag_name, flag_value in supported_flags.items():
                new_subst = unify(flag_term, Atom(flag_name), subst)
                if new_subst is not None:
                    final_subst = unify(value_term, flag_value, new_subst)
                    if final_subst is not None:
                        yield final_subst
        elif isinstance(flag_term, Atom) and flag_term.name in supported_flags:
            flag_value = supported_flags[flag_term.name]
            new_subst = unify(value_term, flag_value, subst)
            if new_subst is not None:
                yield new_subst

    @staticmethod
    def _builtin_predicate_documentation(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        """predicate_documentation(Indicator, Doc) - Get documentation for a predicate."""
        indicator_term, doc_term = args
        indicator_term = deref(indicator_term, subst)
        doc_term = deref(doc_term, subst)

        key: tuple[str, int] | None = None
        if isinstance(indicator_term, Compound) and indicator_term.functor == "/" and len(indicator_term.args) == 2:
            name_term, arity_term = indicator_term.args
            if isinstance(name_term, Atom) and isinstance(arity_term, Number):
                key = (name_term.name, int(arity_term.value))
        elif isinstance(indicator_term, Compound):
            key = (indicator_term.functor, len(indicator_term.args))
        elif isinstance(indicator_term, Atom):
            key = (indicator_term.name, 0)

        if key is None:
            return iter(())

        doc = engine.predicate_docs.get(key)
        if doc is None:
            return iter(())

        result = unify(doc_term, Atom(doc), subst)
        if result is not None:
            yield result


__all__ = ["ReflectionBuiltins"]
