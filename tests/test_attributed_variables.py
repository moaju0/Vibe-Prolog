"""Tests for attributed variables (SICStus/Scryer style library(atts))."""

import pytest
from vibeprolog import PrologInterpreter


class TestBasicAttributeOperations:
    """Tests for basic put_atts/2 and get_atts/2 operations."""

    def test_put_atts_simple(self):
        """Setting a simple attribute on a variable."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("put_atts(X, +color(red))")

    def test_put_atts_multiple_attributes(self):
        """Setting multiple attributes on a variable."""
        prolog = PrologInterpreter()
        assert prolog.has_solution(
            "put_atts(X, +color(red)), put_atts(X, +size(large))"
        )

    def test_put_atts_list_syntax(self):
        """Setting multiple attributes using list syntax."""
        prolog = PrologInterpreter()
        assert prolog.has_solution(
            "put_atts(X, [+color(red), +size(large)])"
        )

    def test_get_atts_retrieves_value(self):
        """Getting an attribute value unifies correctly."""
        prolog = PrologInterpreter()
        result = prolog.query_once(
            "put_atts(X, +color(red)), get_atts(X, color(C))"
        )
        assert result is not None
        assert result["C"] == "red"

    def test_get_atts_check_present(self):
        """Checking if an attribute is present with +Attr."""
        prolog = PrologInterpreter()
        assert prolog.has_solution(
            "put_atts(X, +color(red)), get_atts(X, +color(_))"
        )

    def test_get_atts_check_absent(self):
        """Checking if an attribute is absent with -Attr."""
        prolog = PrologInterpreter()
        # Variable without the attribute
        assert prolog.has_solution("get_atts(X, -color(_))")

    def test_get_atts_fails_when_not_present(self):
        """get_atts fails when querying a non-existent attribute."""
        prolog = PrologInterpreter()
        assert not prolog.has_solution(
            "get_atts(X, color(C))"
        )

    def test_put_atts_remove_attribute(self):
        """Removing an attribute with -Attr."""
        prolog = PrologInterpreter()
        # Add then remove attribute
        assert prolog.has_solution(
            "put_atts(X, +color(red)), put_atts(X, -color(_)), get_atts(X, -color(_))"
        )

    def test_attribute_updates(self):
        """An attribute can be updated (replaced)."""
        prolog = PrologInterpreter()
        result = prolog.query_once(
            "put_atts(X, +color(red)), put_atts(X, +color(blue)), get_atts(X, color(C))"
        )
        assert result is not None
        assert result["C"] == "blue"


class TestAttvar:
    """Tests for attvar/1 type test."""

    def test_attvar_with_attributed_var(self):
        """attvar/1 succeeds for variables with attributes."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("put_atts(X, +color(red)), attvar(X)")

    def test_attvar_fails_for_regular_var(self):
        """attvar/1 fails for regular variables without attributes."""
        prolog = PrologInterpreter()
        assert not prolog.has_solution("attvar(X)")

    def test_attvar_fails_for_ground_term(self):
        """attvar/1 fails for ground terms."""
        prolog = PrologInterpreter()
        assert not prolog.has_solution("attvar(foo)")
        assert not prolog.has_solution("attvar(123)")

    def test_var_still_succeeds_for_attvar(self):
        """var/1 should still succeed for attributed variables."""
        prolog = PrologInterpreter()
        assert prolog.has_solution("put_atts(X, +color(red)), var(X)")


class TestTermAttvars:
    """Tests for term_attvars/2."""

    def test_term_attvars_finds_attvars(self):
        """term_attvars/2 finds attributed variables in a term."""
        prolog = PrologInterpreter()
        result = prolog.query_once(
            "put_atts(X, +color(red)), term_attvars(foo(X, Y, X), Vars)"
        )
        assert result is not None
        # Should contain X but not Y (Y has no attributes)
        assert len(result["Vars"]) == 1

    def test_term_attvars_empty_for_no_attvars(self):
        """term_attvars/2 returns empty list when no attributed variables."""
        prolog = PrologInterpreter()
        result = prolog.query_once(
            "term_attvars(foo(a, b, c), Vars)"
        )
        assert result is not None
        assert result["Vars"] == []


class TestDelAtts:
    """Tests for del_atts/1."""

    def test_del_atts_removes_all_attributes(self):
        """del_atts/1 removes all attributes from a variable."""
        prolog = PrologInterpreter()
        assert prolog.has_solution(
            "put_atts(X, +color(red)), put_atts(X, +size(large)), "
            "del_atts(X), \\+ attvar(X)"
        )


class TestAttributeDirective:
    """Tests for :- attribute directive."""

    def test_attribute_directive_accepted(self):
        """The :- attribute(Name/Arity) directive is accepted."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- attribute(color/1).
        """)
        # Should not raise an error
        assert True

    def test_attribute_directive_multiple(self):
        """Multiple attributes can be declared with comma syntax."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- attribute((color/1, size/1, weight/1)).
        """)
        # Should not raise an error
        assert True


class TestVerifyAttributes:
    """Tests for verify_attributes/3 hook during unification."""

    def test_verify_attributes_called_on_unification(self):
        """verify_attributes/3 is called when an attvar is unified."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- dynamic(hook_called/2).
            
            verify_attributes(Var, Value, []) :-
                get_atts(Var, +myattr(_)),
                assertz(hook_called(Var, Value)).
            verify_attributes(_, _, []).
        """)
        
        prolog.query_once("put_atts(X, +myattr(test)), X = hello")
        result = prolog.has_solution("hook_called(_, hello)")
        assert result

    def test_verify_attributes_can_fail_unification(self):
        """verify_attributes/3 failure causes unification to fail."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            verify_attributes(Var, Value, []) :-
                get_atts(Var, domain(Domain)),
                member(Value, Domain).
        """
        )
        
        # Unification with value in domain should succeed
        result = prolog.query_once(
            "put_atts(X, +domain([1, 2, 3])), X = 2"
        )
        assert result is not None

        # Unification with value outside domain should fail
        assert not prolog.has_solution(
            "put_atts(X, +domain([1, 2, 3])), X = 4"
        )

    def test_verify_attributes_returns_goals(self):
        """Goals returned by verify_attributes/3 are executed."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- dynamic(goal_executed/1).
            
            verify_attributes(Var, Value, [assertz(goal_executed(Value))]) :-
                get_atts(Var, +tracking(_)).
            verify_attributes(_, _, []).
        """)
        
        prolog.query_once("put_atts(X, +tracking(yes)), X = myvalue")
        result = prolog.has_solution("goal_executed(myvalue)")
        assert result

    def test_no_verify_attributes_normal_unification(self):
        """Without verify_attributes/3, unification works normally."""
        prolog = PrologInterpreter()
        # Just test that basic unification still works
        result = prolog.query_once(
            "put_atts(X, +color(red)), X = hello"
        )
        assert result is not None


class TestCopyTerm3:
    """Tests for copy_term/3 with attributed variables."""

    def test_copy_term_3_copies_attributes_as_goals(self):
        """copy_term/3 returns attribute restoration goals."""
        prolog = PrologInterpreter()
        result = prolog.query_once(
            "put_atts(X, +color(red)), copy_term(X, Y, Goals)"
        )
        assert result is not None
        # Goals should contain put_atts goal for the copy
        assert result["Goals"] != []


class TestIntegration:
    """Integration tests for attributed variables."""

    def test_simple_constraint_simulation(self):
        """Simple constraint: variable can only unify with integers."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            verify_attributes(Var, Value, []) :-
                get_atts(Var, +integer_only),
                !,
                integer(Value).
            verify_attributes(_, _, []).
        """)

        # Should succeed with integer
        result = prolog.query_once(
            "put_atts(X, +integer_only), X = 42"
        )
        assert result is not None
        assert result["X"] == 42
        # Should fail with non-integer
        assert not prolog.has_solution(
            "put_atts(X, +integer_only), X = abc"
        )
        
    def test_domain_constraint(self):
        """Domain constraint: variable restricted to specific values."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            verify_attributes(Var, Value, []) :-
                get_atts(Var, domain(Domain)),
                !,
                member(Value, Domain).
            verify_attributes(_, _, []).
        """)

        # Should succeed with value in domain
        result = prolog.query_once(
            "put_atts(X, +domain([a, b, c])), X = b"
        )
        assert result is not None
        assert result["X"] == "b"

        # Should fail with value outside domain
        assert not prolog.has_solution(
            "put_atts(X, +domain([a, b, c])), X = d"
        )


class TestLibraryAtts:
    """Tests for loading library(atts)."""

    def test_load_library_atts(self):
        """library(atts) can be loaded successfully."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- use_module(library(atts)).
        """)
        # Should not raise an error
        assert True

    def test_library_atts_predicates_available(self):
        """After loading library(atts), predicates are available."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- use_module(library(atts)).
        """)
        # Test that predicates work
        assert prolog.has_solution("put_atts(X, +test(1))")
        assert prolog.has_solution("put_atts(X, +test(1)), attvar(X)")


class TestEdgeCases:
    """Edge case tests for attributed variables."""

    def test_attribute_on_unified_variable(self):
        """Attributes persist through variable aliasing."""
        prolog = PrologInterpreter()
        # When X and Y are unified, they share attributes
        result = prolog.query_once(
            "X = Y, put_atts(X, +color(red)), get_atts(Y, color(C))"
        )
        # Note: This depends on implementation details of how aliasing works
        # In our implementation, attributes are keyed by variable name
        # so this might not work exactly as in some Prolog systems
        # For now, we just test that no error is raised
        assert result is not None and result.get("C") == "red"

    def test_nested_attvar_in_compound(self):
        """Attributed variables can appear in compound terms."""
        prolog = PrologInterpreter()
        result = prolog.query_once(
            "put_atts(X, +myattr(1)), term_attvars(foo(X, bar(X)), Vars)"
        )
        assert result is not None
        # X should appear once in the result (deduplicated)
        assert len(result["Vars"]) == 1
