"""Residual goal projection for attributed variables.

Implements the library($project_atts) interface:
- term_residual_goals/2: Collect residual goals from attributed variables
- project_attributes/2: Project constraints onto query variables

This module provides support for displaying constraint answers and for
copy_term/3 with attributed variables.
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Iterator

from vibeprolog.builtins import BuiltinRegistry, register_builtin
from vibeprolog.builtins.atts import AttsBuiltins
from vibeprolog.builtins.common import BuiltinArgs, EngineContext
from vibeprolog.parser import List
from vibeprolog.terms import Atom, Compound, Variable
from vibeprolog.unification import Substitution, deref, unify

if TYPE_CHECKING:
    pass


class ProjectAttsBuiltins:
    """Built-ins for residual goal projection."""

    @staticmethod
    def register(registry: BuiltinRegistry, engine: EngineContext | None) -> None:
        """Register project_atts predicate handlers."""
        register_builtin(
            registry, "term_residual_goals", 2, ProjectAttsBuiltins._builtin_term_residual_goals
        )
        register_builtin(
            registry, "project_attributes", 2, ProjectAttsBuiltins._builtin_project_attributes
        )

    @staticmethod
    def _builtin_term_residual_goals(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Iterator[Substitution]:
        """term_residual_goals(Term, Goals) - Collect residual goals from attributed variables.

        Finds all attributed variables in Term and collects goals that represent
        their constraints. For each attributed variable, if the module defining
        the attribute exports attribute_goals//1, that hook is called. Otherwise,
        a default put_atts/2 goal is generated.

        Args:
            args[0]: Term to search for attributed variables
            args[1]: Output - list of goals representing the constraints

        Yields:
            Substitutions where args[1] is unified with the goals list
        """
        term = args[0]
        goals_arg = args[1]

        attvars = AttsBuiltins._collect_attvars(term, subst, engine)

        goals = ProjectAttsBuiltins._collect_residual_goals(attvars, subst, engine)

        from vibeprolog.utils.list_utils import python_to_list

        goals_list = python_to_list(goals) if goals else List(())

        new_subst = unify(goals_arg, goals_list, subst)
        if new_subst is not None:
            yield new_subst

    @staticmethod
    def _collect_residual_goals(
        attvars: list, subst: Substitution, engine: EngineContext
    ) -> list:
        """Collect residual goals from a list of attributed variables.

        For each attributed variable, attempts to call the defining module's
        attribute_goals//1 hook. If no hook is defined, generates default
        put_atts/2 goals.

        Args:
            attvars: List of attributed variables
            subst: Current substitution
            engine: Engine context

        Returns:
            List of goal terms representing the constraints
        """
        goals = []
        store = AttsBuiltins._get_attribute_store(engine)

        for var in attvars:
            var = deref(var, subst)
            if not isinstance(var, Variable):
                continue

            var_attrs = store.get(var.name, {})
            if not var_attrs:
                continue

            var_goals = ProjectAttsBuiltins._get_attribute_goals_for_var(
                var, var_attrs, subst, engine
            )
            goals.extend(var_goals)

        return goals

    @staticmethod
    def _get_attribute_goals_for_var(
        var: Variable, var_attrs: dict, subst: Substitution, engine: EngineContext
    ) -> list:
        """Get residual goals for a single attributed variable.

        Attempts to call attribute_goals//1 for each attribute's module.
        Falls back to generating put_atts/2 goals if no hook is defined.

        Args:
            var: The attributed variable
            var_attrs: Dictionary of {functor: attribute_value}
            subst: Current substitution
            engine: Engine context

        Returns:
            List of goal terms for this variable's attributes
        """
        goals = []
        tried_modules = set()

        for functor, attr in var_attrs.items():
            module_name = ProjectAttsBuiltins._infer_module_from_attribute(
                functor, engine
            )

            if module_name and module_name not in tried_modules:
                tried_modules.add(module_name)

                hook_goals = ProjectAttsBuiltins._try_attribute_goals_hook(
                    var, module_name, subst, engine
                )
                if hook_goals is not None:
                    goals.extend(hook_goals)
                    continue

            goal = Compound("put_atts", (var, Compound("+", (attr,))))
            goals.append(goal)

        return goals

    @staticmethod
    def _infer_module_from_attribute(functor: str, engine: EngineContext) -> str | None:
        """Infer which module defined an attribute based on its functor.

        This checks if any loaded module has declared attributes with this functor.

        Args:
            functor: The attribute functor name
            engine: Engine context

        Returns:
            Module name if found, None otherwise
        """
        interpreter = getattr(engine, "_interpreter", None)
        if interpreter is None:
            return None

        for module_name in interpreter.modules:
            module_attrs = getattr(engine, "_module_attributes", {})
            if module_name in module_attrs:
                if functor in module_attrs[module_name]:
                    return module_name

        return None

    @staticmethod
    def _try_attribute_goals_hook(
        var: Variable, module_name: str, subst: Substitution, engine: EngineContext
    ) -> list | None:
        """Try to call a module's attribute_goals//1 DCG hook.

        The hook should be defined as:
            attribute_goals(Var) --> Goals.

        Which is equivalent to:
            attribute_goals(Var, S0, S) :- ... 

        Args:
            var: The attributed variable
            module_name: Name of the module to try
            subst: Current substitution
            engine: Engine context

        Returns:
            List of goal terms if hook succeeded, None if hook not defined or failed
        """
        interpreter = getattr(engine, "_interpreter", None)
        if interpreter is None:
            return None

        if module_name not in interpreter.modules:
            return None

        module = interpreter.modules[module_name]

        if ("attribute_goals", 3) not in module.exports and (
            "attribute_goals", 3
        ) not in module.predicates:
            return None

        s0 = engine._fresh_variable("S0")
        s = engine._fresh_variable("S")
        goal = Compound(":", (Atom(module_name), Compound("attribute_goals", (var, s0, s))))

        try:
            result_subst = None
            for sol in engine._solve_goals([goal], subst, 0):
                result_subst = sol
                break

            if result_subst is None:
                return None

            goals_list = deref(s0, result_subst)
            remainder = deref(s, result_subst)

            return ProjectAttsBuiltins._extract_goals_from_dcg_result(
                goals_list, remainder, result_subst
            )

        except Exception:
            return None

    @staticmethod
    def _extract_goals_from_dcg_result(
        goals_list, remainder, subst: Substitution
    ) -> list:
        """Extract goal terms from a DCG difference list result.

        The DCG produces a difference list S0-S where the goals are the
        elements consumed from S0 until we reach S.

        Args:
            goals_list: The S0 term (start of difference list)
            remainder: The S term (end of difference list)
            subst: Current substitution

        Returns:
            List of goal terms extracted from the difference list
        """
        goals = []

        current = deref(goals_list, subst)
        end = deref(remainder, subst)

        while True:
            if isinstance(current, Variable) or current == end:
                break

            if isinstance(current, List):
                if current.elements:
                    goals.append(current.elements[0])
                    if len(current.elements) > 1:
                        from vibeprolog.utils.list_utils import python_to_list

                        current = python_to_list(list(current.elements[1:]), current.tail)
                    elif current.tail is not None:
                        current = deref(current.tail, subst)
                    else:
                        break
                elif current.tail is not None:
                    current = deref(current.tail, subst)
                else:
                    break
            elif isinstance(current, Compound) and current.functor == "." and len(current.args) == 2:
                goals.append(deref(current.args[0], subst))
                current = deref(current.args[1], subst)
            elif isinstance(current, Atom) and current.name == "[]":
                break
            else:
                break

        return goals

    @staticmethod
    def _builtin_project_attributes(
        args: BuiltinArgs, subst: Substitution, engine: EngineContext
    ) -> Substitution | None:
        """project_attributes(QueryVars, AttrVars) - Project constraints onto query variables.

        This predicate is called by the toplevel to project constraints from all
        attributed variables (AttrVars) onto the variables the user asked about
        (QueryVars). Variables not in QueryVars should be existentially quantified.

        For each module with attributes on variables in AttrVars, if the module
        exports project_attributes/2, it is called to perform module-specific
        projection.

        Args:
            args[0]: QueryVars - list of variables the user queried about
            args[1]: AttrVars - list of all attributed variables encountered

        Returns:
            The substitution if projection succeeds, None otherwise
        """
        query_vars = deref(args[0], subst)
        attr_vars = deref(args[1], subst)

        query_var_set = set()
        if isinstance(query_vars, List):
            for v in query_vars.elements:
                v = deref(v, subst)
                if isinstance(v, Variable):
                    query_var_set.add(v.name)
        elif isinstance(query_vars, Atom) and query_vars.name == "[]":
            pass
        elif isinstance(query_vars, Compound) and query_vars.functor == "." and len(query_vars.args) == 2:
            current = query_vars
            while isinstance(current, Compound) and current.functor == "." and len(current.args) == 2:
                v = deref(current.args[0], subst)
                if isinstance(v, Variable):
                    query_var_set.add(v.name)
                current = deref(current.args[1], subst)

        attr_var_list = []
        if isinstance(attr_vars, List):
            for v in attr_vars.elements:
                v = deref(v, subst)
                if isinstance(v, Variable):
                    attr_var_list.append(v)
        elif isinstance(attr_vars, Atom) and attr_vars.name == "[]":
            pass
        elif isinstance(attr_vars, Compound) and attr_vars.functor == "." and len(attr_vars.args) == 2:
            current = attr_vars
            while isinstance(current, Compound) and current.functor == "." and len(current.args) == 2:
                v = deref(current.args[0], subst)
                if isinstance(v, Variable):
                    attr_var_list.append(v)
                current = deref(current.args[1], subst)

        interpreter = getattr(engine, "_interpreter", None)
        if interpreter is None:
            return subst

        store = AttsBuiltins._get_attribute_store(engine)

        modules_with_attrs = set()
        for var in attr_var_list:
            var_attrs = store.get(var.name, {})
            for functor in var_attrs:
                module_name = ProjectAttsBuiltins._infer_module_from_attribute(
                    functor, engine
                )
                if module_name:
                    modules_with_attrs.add(module_name)

        from vibeprolog.utils.list_utils import python_to_list

        for module_name in modules_with_attrs:
            if module_name not in interpreter.modules:
                continue

            module = interpreter.modules[module_name]

            if ("project_attributes", 2) not in module.exports and (
                "project_attributes", 2
            ) not in module.predicates:
                continue

            query_vars_list = python_to_list(
                [Variable(name) for name in query_var_set]
            )
            attr_vars_list = python_to_list(attr_var_list)

            goal = Compound(
                ":",
                (
                    Atom(module_name),
                    Compound("project_attributes", (query_vars_list, attr_vars_list)),
                ),
            )

            try:
                for sol in engine._solve_goals([goal], subst, 0):
                    subst = sol
                    break
            except Exception:
                pass

        return subst


__all__ = ["ProjectAttsBuiltins"]
