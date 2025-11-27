"""Definite Clause Grammar (DCG) expansion logic."""

from typing import Any

from vibeprolog.parser import Atom, Compound, Cut, List, Variable


class DCGExpander:
    """Expands DCG rules into regular Prolog clauses with difference lists."""

    def __init__(self):
        self._var_counter = 0

    def _fresh_var(self, prefix: str = "S") -> Variable:
        """Generate a fresh variable for difference list threading."""
        var_name = f"{prefix}{self._var_counter}"
        self._var_counter += 1
        return Variable(var_name)

    def expand_dcg_clause(self, head: Compound, body: list[Any]) -> Compound:
        """
        Expand a DCG clause Head --> Body into Head(S0, S) :- ExpandedBody.

        Args:
            head: The DCG head (e.g., sentence)
            body: The DCG body as a list of goals

        Returns:
            Expanded Prolog clause with difference list arguments
        """
        # Reset variable counter for each clause
        self._var_counter = 0

        # Create input and output variables for difference list
        s0 = self._fresh_var("S")
        s = self._fresh_var("S")

        # Expand the DCG head: foo becomes foo(S0, S)
        expanded_head = self._expand_nonterminal(head, s0, s)

        # Expand the DCG body
        if not body:
            # Empty body: just unify S0 = S
            expanded_body = [Compound("=", (s0, s))]
        else:
            expanded_body = self._expand_dcg_body(body, s0, s)

        return Compound(":-", (expanded_head, expanded_body[0]))

    def _expand_dcg_body(self, body: list[Any], s0: Variable, s: Variable) -> list[Compound]:
        """
        Expand DCG body goals into regular Prolog goals with difference list threading.

        Args:
            body: List of DCG body elements
            s0: Input difference list variable
            s: Output difference list variable

        Returns:
            List of expanded Prolog goals
        """
        if not body:
            return [Compound("=", (s0, s))]

        goals = []
        current_s0 = s0

        for i, goal in enumerate(body):
            if i == len(body) - 1:
                # Last goal gets the final output variable
                current_s = s
            else:
                # Intermediate goals get fresh variables
                current_s = self._fresh_var("S")

            expanded = self._expand_dcg_goal(goal, current_s0, current_s)
            goals.extend(expanded)
            current_s0 = current_s

        # If we have multiple goals, combine them with conjunction
        if len(goals) > 1:
            # Build right-associative conjunction
            result = goals[-1]
            for goal in reversed(goals[:-1]):
                result = Compound(",", (goal, result))
            return [result]
        else:
            return goals

    def _expand_dcg_goal(self, goal: Any, s0: Variable, s: Variable) -> list[Compound]:
        """
        Expand a single DCG goal.

        Args:
            goal: The DCG goal to expand
            s0: Input difference list variable
            s: Output difference list variable

        Returns:
            List of expanded Prolog goals
        """
        if isinstance(goal, List):
            # Terminal: [X, Y, Z] becomes S0 = [X, Y, Z | S]
            if not goal.elements:
                # Empty list [] becomes S0 = S
                return [Compound("=", (s0, s))]
            else:
                # Non-empty list: create [Elements | S]
                tail = s
                for elem in reversed(goal.elements):
                    tail = List(elements=(elem,), tail=tail)
                return [Compound("=", (s0, tail))]

        elif isinstance(goal, Cut):
            # Cut: preserve as-is
            return [goal]

        elif isinstance(goal, Compound):
            if goal.functor == "{}" and len(goal.args) == 1:
                # Embedded goal: {Goal} unifies input/output difference lists and executes Goal
                return [Compound("=", (s0, s)), goal.args[0]]

            elif goal.functor == ",":
                # Sequence: expand recursively
                return self._expand_dcg_body([goal.args[0], goal.args[1]], s0, s)

            elif goal.functor == ";":
                # Alternative: expand both sides with same variables
                left_expanded = self._expand_dcg_goal(goal.args[0], s0, s)
                right_expanded = self._expand_dcg_goal(goal.args[1], s0, s)

                # Create disjunction
                if len(left_expanded) == 1 and len(right_expanded) == 1:
                    return [Compound(";", (left_expanded[0], right_expanded[0]))]
                else:
                    # For complex cases, wrap in conjunctions
                    left_conj = Compound(",", tuple(left_expanded)) if len(left_expanded) > 1 else left_expanded[0]
                    right_conj = Compound(",", tuple(right_expanded)) if len(right_expanded) > 1 else right_expanded[0]
                    return [Compound(";", (left_conj, right_conj))]

            elif goal.functor == "!":
                # Cut: preserve as-is
                return [goal]

            else:
                # Non-terminal: add difference list arguments
                return [self._expand_nonterminal(goal, s0, s)]

        elif isinstance(goal, Atom):
            # Non-terminal atom: add difference list arguments
            return [self._expand_nonterminal(goal, s0, s)]

        else:
            # Other terms (shouldn't happen in valid DCG)
            raise ValueError(f"Unsupported DCG goal: {goal}")

    def _expand_nonterminal(self, term: Any, s0: Variable, s: Variable) -> Compound:
        """
        Expand a non-terminal by adding difference list arguments.

        Args:
            term: The non-terminal (atom or compound)
            s0: Input difference list variable
            s: Output difference list variable

        Returns:
            Non-terminal with difference list arguments added
        """
        if isinstance(term, Atom):
            return Compound(term.name, (s0, s))
        elif isinstance(term, Compound):
            return Compound(term.functor, term.args + (s0, s))
        else:
            raise ValueError(f"Cannot expand non-terminal: {term}")


def expand_dcg_clause(head: Compound, body: list[Any]) -> Compound:
    """
    Convenience function to expand a DCG clause.

    Args:
        head: DCG head
        body: DCG body

    Returns:
        Expanded Prolog clause
    """
    expander = DCGExpander()
    return expander.expand_dcg_clause(head, body)