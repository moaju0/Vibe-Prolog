"""Attributed variables built-ins (SICStus/Scryer style).

Implements the library(atts) interface for attributed variables:
- put_atts/2: Set/remove attributes on variables
- get_atts/2: Query attributes on variables
- attvar/1: Test if a variable has attributes

This module provides the foundation for constraint logic programming (CLP).
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Iterator

from vibeprolog.builtins import BuiltinRegistry, register_builtin
from vibeprolog.builtins.common import BuiltinArgs, EngineContext
from vibeprolog.exceptions import PrologError, PrologThrow
from vibeprolog.parser import List
from vibeprolog.terms import Atom, Compound, Number, Variable
from vibeprolog.unification import Substitution, deref, unify

if TYPE_CHECKING:
    from vibeprolog.engine import PrologEngine


class AttsBuiltins:
    """Built-ins for attributed variables (SICStus/Scryer style)."""

    @staticmethod
    def register(registry: BuiltinRegistry, engine: EngineContext | None) -> None:
        """Register attributed variable predicate handlers."""
        register_builtin(registry, "put_atts", 2, AttsBuiltins._builtin_put_atts)
        register_builtin(registry, "get_atts", 2, AttsBuiltins._builtin_get_atts)
        register_builtin(registry, "attvar", 1, AttsBuiltins._builtin_attvar)
        register_builtin(registry, "term_attvars", 2, AttsBuiltins._builtin_term_attvars)
        register_builtin(registry, "copy_term", 3, AttsBuiltins._builtin_copy_term_3)
        register_builtin(registry, "del_atts", 1, AttsBuiltins._builtin_del_atts)

    @staticmethod
    def _get_attribute_store(engine: EngineContext) -> dict:
        """Get or create the attribute store for the engine.
        
        Attributes are stored in a dictionary mapping variable names to
        a dict of {attribute_functor: attribute_value}.
        """
        if not hasattr(engine, '_attribute_store'):
            engine._attribute_store = {}
        return engine._attribute_store

    # Removed unused helper methods: _get_declared_attributes and _get_var_key

    @staticmethod
    def _builtin_put_atts(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        """put_atts(Var, Attributes) - Set attributes on a variable.
        
        Attributes can be:
        - +Attr: Add this attribute
        - -Attr: Remove this attribute (if present)
        - [Attr1, Attr2, ...]: Multiple attribute operations
        - Attr: Same as +Attr
        """
        var_term = deref(args[0], subst)
        attr_term = deref(args[1], subst)

        if not isinstance(var_term, Variable):
            return

        var_key = var_term.name
        store = AttsBuiltins._get_attribute_store(engine)

        if var_key not in store:
            store[var_key] = {}

        attr_operations = AttsBuiltins._parse_attr_spec(attr_term)
        if attr_operations is None:
            error_term = PrologError.type_error("attribute_specification", attr_term, "put_atts/2")
            raise PrologThrow(error_term)

        for op, attr in attr_operations:
            if isinstance(attr, Compound):
                attr_functor = attr.functor
            elif isinstance(attr, Atom):
                attr_functor = attr.name
            else:
                continue

            if op == '+':
                store[var_key][attr_functor] = attr
            elif op == '-':
                if attr_functor in store[var_key]:
                    del store[var_key][attr_functor]

        yield subst

    @staticmethod
    def _parse_attr_spec(attr_term) -> list[tuple[str, any]] | None:
        """Parse an attribute specification into a list of (operation, attribute) pairs.
        
        Returns:
            List of ('+', attr) or ('-', attr) tuples, or None if invalid.
        """
        result = []

        if isinstance(attr_term, List):
            for elem in attr_term.elements:
                parsed = AttsBuiltins._parse_single_attr_spec(elem)
                if parsed is None:
                    return None
                result.append(parsed)
            return result

        parsed = AttsBuiltins._parse_single_attr_spec(attr_term)
        if parsed is None:
            return None
        return [parsed]

    @staticmethod
    def _parse_single_attr_spec(attr_term) -> tuple[str, any] | None:
        """Parse a single attribute specification.
        
        Returns:
            ('+', attr) for add, ('-', attr) for remove, or None if invalid.
        """
        if isinstance(attr_term, Compound):
            if attr_term.functor == '+' and len(attr_term.args) == 1:
                return ('+', attr_term.args[0])
            elif attr_term.functor == '-' and len(attr_term.args) == 1:
                return ('-', attr_term.args[0])
            else:
                return ('+', attr_term)
        elif isinstance(attr_term, Atom):
            return ('+', attr_term)
        return None

    @staticmethod
    def _builtin_get_atts(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        """get_atts(Var, Attributes) - Query attributes on a variable.
        
        Modes:
        - get_atts(Var, Attr): Unify Attr with the attribute value
        - get_atts(Var, +Attr): Succeed if attribute is present
        - get_atts(Var, -Attr): Succeed if attribute is absent
        """
        var_term = deref(args[0], subst)
        attr_term = deref(args[1], subst)

        if not isinstance(var_term, Variable):
            return

        var_key = var_term.name
        store = AttsBuiltins._get_attribute_store(engine)

        var_attrs = store.get(var_key, {})

        if isinstance(attr_term, Compound):
            if attr_term.functor == '+' and len(attr_term.args) == 1:
                query_attr = attr_term.args[0]
                if isinstance(query_attr, Compound):
                    attr_functor = query_attr.functor
                elif isinstance(query_attr, Atom):
                    attr_functor = query_attr.name
                else:
                    return
                if attr_functor in var_attrs:
                    yield subst
                return

            elif attr_term.functor == '-' and len(attr_term.args) == 1:
                query_attr = attr_term.args[0]
                if isinstance(query_attr, Compound):
                    attr_functor = query_attr.functor
                elif isinstance(query_attr, Atom):
                    attr_functor = query_attr.name
                else:
                    return
                if attr_functor not in var_attrs:
                    yield subst
                return

            else:
                attr_functor = attr_term.functor
                if attr_functor in var_attrs:
                    stored_attr = var_attrs[attr_functor]
                    new_subst = unify(attr_term, stored_attr, subst)
                    if new_subst is not None:
                        yield new_subst
                return

        elif isinstance(attr_term, Atom):
            attr_functor = attr_term.name
            if attr_functor in var_attrs:
                stored_attr = var_attrs[attr_functor]
                new_subst = unify(attr_term, stored_attr, subst)
                if new_subst is not None:
                    yield new_subst
            return

        elif isinstance(attr_term, Variable):
            # Unify with a list of all attributes, per SICStus/Scryer standard.
            from vibeprolog.utils.list_utils import python_to_list
            attrs_list = python_to_list(list(var_attrs.values()))
            new_subst = unify(attr_term, attrs_list, subst)
            if new_subst is not None:
                yield new_subst
            return

    @staticmethod
    def _builtin_attvar(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Substitution | None:
        """attvar(X) - Succeeds if X is an attributed variable.
        
        An attributed variable is an unbound variable with at least one attribute.
        """
        term = deref(args[0], subst)

        if not isinstance(term, Variable):
            return None

        var_key = term.name
        store = AttsBuiltins._get_attribute_store(engine)

        var_attrs = store.get(var_key, {})
        if var_attrs:
            return subst
        return None

    @staticmethod
    def _builtin_term_attvars(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        """term_attvars(Term, AttVars) - Find all attributed variables in Term.
        
        AttVars is unified with a list of all attributed variables in Term.
        """
        term = deref(args[0], subst)
        attvars_term = args[1]

        attvars = AttsBuiltins._collect_attvars(term, subst, engine)

        from vibeprolog.utils.list_utils import python_to_list
        attvars_list = python_to_list(attvars)

        new_subst = unify(attvars_term, attvars_list, subst)
        if new_subst is not None:
            yield new_subst

    @staticmethod
    def _collect_attvars(term, subst: Substitution, engine: EngineContext) -> list:
        """Collect all attributed variables in a term."""
        term = deref(term, subst)
        store = AttsBuiltins._get_attribute_store(engine)
        result = []
        seen = set()

        def collect(t):
            t = deref(t, subst)
            if isinstance(t, Variable):
                if t.name not in seen and t.name in store and store[t.name]:
                    seen.add(t.name)
                    result.append(t)
            elif isinstance(t, Compound):
                for arg in t.args:
                    collect(arg)
            elif isinstance(t, List):
                for elem in t.elements:
                    collect(elem)
                if t.tail is not None:
                    collect(t.tail)

        collect(term)
        return result

    @staticmethod
    def _builtin_copy_term_3(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        """copy_term(Term, Copy, Goals) - Copy term with attributes as goals.

        Like copy_term/2, but the third argument is unified with a list of
        goals that represent the attributes of the copied variables.
        """
        from vibeprolog.utils.variable_utils import collect_vars
        from vibeprolog.utils.list_utils import python_to_list

        term = args[0]
        copy_term_arg = args[1]
        goals_term = args[2]

        # Collect all variables in the term and in attribute values
        store = AttsBuiltins._get_attribute_store(engine)
        var_set = set()

        def _collect_vars_from_term(t):
            t = deref(t, subst)
            if isinstance(t, Variable):
                var_set.add(t)
            elif isinstance(t, Compound):
                for a in t.args:
                    _collect_vars_from_term(a)
            elif isinstance(t, List):
                for e in t.elements:
                    _collect_vars_from_term(e)
                if t.tail is not None:
                    _collect_vars_from_term(t.tail)

        _collect_vars_from_term(term)

        # Collect vars inside attribute values
        def _collect_vars_from_attr(val):
            v = deref(val, subst)
            if isinstance(v, Variable):
                var_set.add(v)
            elif isinstance(v, Compound):
                for a in v.args:
                    _collect_vars_from_attr(a)
            elif isinstance(v, List):
                for e in v.elements:
                    _collect_vars_from_attr(e)
                if v.tail is not None:
                    _collect_vars_from_attr(v.tail)

        for attrs in store.values():
            for attr_val in attrs.values():
                _collect_vars_from_attr(attr_val)

        # Create fresh copies for all collected variables
        var_mapping = {v: engine._fresh_variable("Copy") for v in var_set}

        def copy(t):
            t = deref(t, subst)
            if isinstance(t, Variable):
                if t in var_mapping:
                    return var_mapping[t]
                return t
            elif isinstance(t, Compound):
                new_args = tuple(copy(arg) for arg in t.args)
                return Compound(t.functor, new_args)
            elif isinstance(t, List):
                new_elems = tuple(copy(e) for e in t.elements)
                new_tail = copy(t.tail) if t.tail is not None else None
                return List(new_elems, new_tail)

        copied = copy(term)
        new_subst = unify(copy_term_arg, copied, subst)
        if new_subst is None:
            return

        store = AttsBuiltins._get_attribute_store(engine)
        goals = []

        for old_var, new_var in var_mapping.items():
            old_name = old_var.name
            if old_name in store and store[old_name]:
                for functor, attr in store[old_name].items():
                    copied_attr = copy(attr)
                    goal = Compound("put_atts", (new_var, Compound("+", (copied_attr,))))
                    goals.append(goal)

        goals_list = python_to_list(goals) if goals else List(())
        final_subst = unify(goals_term, goals_list, new_subst)
        if final_subst is not None:
            yield final_subst

    @staticmethod
    def _builtin_del_atts(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Substitution | None:
        """del_atts(Var) - Delete all attributes from a variable."""
        var_term = deref(args[0], subst)

        if not isinstance(var_term, Variable):
            return None

        var_key = var_term.name
        store = AttsBuiltins._get_attribute_store(engine)

        if var_key in store:
            del store[var_key]

        return subst


def is_attvar(term, subst: Substitution, engine: EngineContext) -> bool:
    """Check if a term is an attributed variable.
    
    This is a utility function for use in other modules.
    """
    term = deref(term, subst)
    if not isinstance(term, Variable):
        return False
    store = AttsBuiltins._get_attribute_store(engine)
    var_attrs = store.get(term.name, {})
    return bool(var_attrs)


def get_var_attributes(var: Variable, engine: EngineContext) -> dict:
    """Get all attributes of a variable.
    
    Returns:
        Dictionary mapping attribute functors to attribute values.
    """
    store = AttsBuiltins._get_attribute_store(engine)
    return store.get(var.name, {})


def copy_attributes(from_var: Variable, to_var: Variable, engine: EngineContext) -> None:
    """Copy all attributes from one variable to another."""
    store = AttsBuiltins._get_attribute_store(engine)
    if from_var.name in store:
        store[to_var.name] = store[from_var.name].copy()


__all__ = [
    "AttsBuiltins",
    "is_attvar",
    "get_var_attributes",
    "copy_attributes",
]
