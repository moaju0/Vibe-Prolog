"""Tests for conditional compilation directives (if/else/elif/endif)."""

import pytest
from vibeprolog import PrologInterpreter
from vibeprolog.exceptions import PrologThrow


class TestConditionalIfEndif:
    """Tests for basic :- if(Condition) ... :- endif. blocks."""

    def test_if_true_condition_includes_code(self):
        """Code inside if block is loaded when condition is true."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- if(true).
            included_fact.
            :- endif.
        """)
        assert prolog.has_solution("included_fact")

    def test_if_false_condition_skips_code(self):
        """Code inside if block is skipped when condition is false."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- if(fail).
            excluded_fact.
            :- endif.
        """)
        assert not prolog.has_solution("excluded_fact")

    def test_if_with_current_predicate_true(self):
        """If block with current_predicate succeeds for built-in predicate."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- if(current_predicate(member/2)).
            has_member.
            :- endif.
        """)
        assert prolog.has_solution("has_member")

    def test_if_with_current_predicate_false(self):
        """If block with current_predicate fails for non-existent predicate."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- if(current_predicate(nonexistent_predicate_xyz/3)).
            has_nonexistent.
            :- endif.
        """)
        assert not prolog.has_solution("has_nonexistent")


class TestConditionalIfElseEndif:
    """Tests for :- if(Condition) ... :- else ... :- endif. blocks."""

    def test_if_else_true_condition_includes_if_block(self):
        """When condition is true, if block is loaded and else block is skipped."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- if(true).
            from_if_block.
            :- else.
            from_else_block.
            :- endif.
        """)
        assert prolog.has_solution("from_if_block")
        assert not prolog.has_solution("from_else_block")

    def test_if_else_false_condition_includes_else_block(self):
        """When condition is false, if block is skipped and else block is loaded."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- if(fail).
            from_if_block.
            :- else.
            from_else_block.
            :- endif.
        """)
        assert not prolog.has_solution("from_if_block")
        assert prolog.has_solution("from_else_block")


class TestConditionalElif:
    """Tests for :- elif(Condition). directive (else-if)."""

    def test_elif_first_true(self):
        """When first if is true, elif and else blocks are skipped."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- if(true).
            from_if.
            :- elif(true).
            from_elif.
            :- else.
            from_else.
            :- endif.
        """)
        assert prolog.has_solution("from_if")
        assert not prolog.has_solution("from_elif")
        assert not prolog.has_solution("from_else")

    def test_elif_if_false_elif_true(self):
        """When if is false and elif is true, elif block is loaded."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- if(fail).
            from_if.
            :- elif(true).
            from_elif.
            :- else.
            from_else.
            :- endif.
        """)
        assert not prolog.has_solution("from_if")
        assert prolog.has_solution("from_elif")
        assert not prolog.has_solution("from_else")

    def test_elif_all_false_falls_to_else(self):
        """When if and all elifs are false, else block is loaded."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- if(fail).
            from_if.
            :- elif(fail).
            from_elif.
            :- else.
            from_else.
            :- endif.
        """)
        assert not prolog.has_solution("from_if")
        assert not prolog.has_solution("from_elif")
        assert prolog.has_solution("from_else")

    def test_multiple_elifs(self):
        """Multiple elif blocks work correctly."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- if(fail).
            from_if.
            :- elif(fail).
            from_elif1.
            :- elif(true).
            from_elif2.
            :- elif(true).
            from_elif3.
            :- else.
            from_else.
            :- endif.
        """)
        assert not prolog.has_solution("from_if")
        assert not prolog.has_solution("from_elif1")
        assert prolog.has_solution("from_elif2")
        assert not prolog.has_solution("from_elif3")
        assert not prolog.has_solution("from_else")


class TestNestedConditionals:
    """Tests for nested conditional blocks."""

    def test_nested_if_both_true(self):
        """Nested if blocks both true includes inner code."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- if(true).
            outer_fact.
            :- if(true).
            inner_fact.
            :- endif.
            :- endif.
        """)
        assert prolog.has_solution("outer_fact")
        assert prolog.has_solution("inner_fact")

    def test_nested_if_outer_false(self):
        """When outer if is false, inner is not evaluated and skipped."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- if(fail).
            outer_fact.
            :- if(true).
            inner_fact.
            :- endif.
            :- endif.
        """)
        assert not prolog.has_solution("outer_fact")
        assert not prolog.has_solution("inner_fact")

    def test_nested_if_inner_false(self):
        """When outer true but inner false, only outer included."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- if(true).
            outer_fact.
            :- if(fail).
            inner_fact.
            :- endif.
            :- endif.
        """)
        assert prolog.has_solution("outer_fact")
        assert not prolog.has_solution("inner_fact")

    def test_nested_if_else(self):
        """Nested if-else blocks work correctly."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- if(true).
            outer_if.
            :- if(fail).
            inner_if.
            :- else.
            inner_else.
            :- endif.
            :- else.
            outer_else.
            :- endif.
        """)
        assert prolog.has_solution("outer_if")
        assert not prolog.has_solution("inner_if")
        assert prolog.has_solution("inner_else")
        assert not prolog.has_solution("outer_else")

    def test_deeply_nested(self):
        """Three levels of nesting work correctly."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- if(true).
            level1.
            :- if(true).
            level2.
            :- if(true).
            level3.
            :- endif.
            :- endif.
            :- endif.
        """)
        assert prolog.has_solution("level1")
        assert prolog.has_solution("level2")
        assert prolog.has_solution("level3")


class TestConditionalErrors:
    """Tests for error handling in conditional compilation."""

    def test_error_else_without_if(self):
        """else without matching if raises syntax error."""
        prolog = PrologInterpreter()
        with pytest.raises(PrologThrow) as exc_info:
            prolog.consult_string("""
                :- else.
            """)
        assert "else without matching if" in str(exc_info.value)

    def test_error_endif_without_if(self):
        """endif without matching if raises syntax error."""
        prolog = PrologInterpreter()
        with pytest.raises(PrologThrow) as exc_info:
            prolog.consult_string("""
                :- endif.
            """)
        assert "endif without matching if" in str(exc_info.value)

    def test_error_elif_without_if(self):
        """elif without matching if raises syntax error."""
        prolog = PrologInterpreter()
        with pytest.raises(PrologThrow) as exc_info:
            prolog.consult_string("""
                :- elif(true).
            """)
        assert "elif without matching if" in str(exc_info.value)

    def test_error_multiple_else(self):
        """Multiple else clauses in one block raise syntax error."""
        prolog = PrologInterpreter()
        with pytest.raises(PrologThrow) as exc_info:
            prolog.consult_string("""
                :- if(true).
                :- else.
                :- else.
                :- endif.
            """)
        assert "multiple else" in str(exc_info.value)

    def test_error_elif_after_else(self):
        """elif after else raises syntax error."""
        prolog = PrologInterpreter()
        with pytest.raises(PrologThrow) as exc_info:
            prolog.consult_string("""
                :- if(true).
                :- else.
                :- elif(true).
                :- endif.
            """)
        assert "elif after else" in str(exc_info.value)

    def test_error_unclosed_if(self):
        """Unclosed if at end of file raises syntax error."""
        prolog = PrologInterpreter()
        with pytest.raises(PrologThrow) as exc_info:
            prolog.consult_string("""
                :- if(true).
                some_fact.
            """)
        assert "unclosed if" in str(exc_info.value)

    def test_error_unclosed_nested_if(self):
        """Unclosed nested if raises syntax error with correct depth."""
        prolog = PrologInterpreter()
        with pytest.raises(PrologThrow) as exc_info:
            prolog.consult_string("""
                :- if(true).
                :- if(true).
                some_fact.
                :- endif.
            """)
        assert "unclosed if" in str(exc_info.value)
        assert "1 level" in str(exc_info.value)


class TestConditionalWithRules:
    """Tests for conditionals with rules (not just facts)."""

    def test_conditional_rule_included(self):
        """Rules are properly included when condition is true."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            base_fact(a).
            base_fact(b).
            
            :- if(true).
            derived(X) :- base_fact(X).
            :- endif.
        """)
        results = prolog.query("derived(X)")
        assert len(results) == 2
        assert {"X": "a"} in results
        assert {"X": "b"} in results

    def test_conditional_rule_excluded(self):
        """Rules are properly excluded when condition is false."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            base_fact(a).
            
            :- if(fail).
            derived(X) :- base_fact(X).
            :- endif.
        """)
        assert not prolog.has_solution("derived(a)")

    def test_conditional_with_alternative_implementations(self):
        """Real-world use case: selecting between implementations."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- if(current_predicate(member/2)).
            my_member(X, L) :- member(X, L).
            :- else.
            my_member(X, [X|_]).
            my_member(X, [_|T]) :- my_member(X, T).
            :- endif.
        """)
        assert prolog.has_solution("my_member(b, [a, b, c])")


class TestConditionalWithDirectives:
    """Tests for conditionals containing other directives."""

    def test_conditional_dynamic_directive(self):
        """Dynamic directive inside conditional is processed when active."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- if(true).
            :- dynamic(dyn_pred/1).
            :- endif.
        """)
        # Should be able to assert to dyn_pred
        prolog.query("assertz(dyn_pred(x))")
        assert prolog.has_solution("dyn_pred(x)")

    def test_conditional_dynamic_directive_skipped(self):
        """Dynamic directive inside inactive conditional is skipped."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- if(fail).
            :- dynamic(dyn_pred2/1).
            :- endif.
            
            dyn_pred2(static).
        """)
        # dyn_pred2 should be static (not dynamic), so assert should fail
        assert prolog.has_solution("dyn_pred2(static)")
        # Trying to assert to a static predicate should fail
        with pytest.raises(PrologThrow):
            prolog.query("assertz(dyn_pred2(y))")


class TestConditionalPredicateConditions:
    """Tests using various predicates as conditions."""

    def test_condition_with_arithmetic(self):
        """Arithmetic conditions work correctly."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- if(1 < 2).
            one_less_than_two.
            :- endif.
            
            :- if(2 < 1).
            two_less_than_one.
            :- endif.
        """)
        assert prolog.has_solution("one_less_than_two")
        assert not prolog.has_solution("two_less_than_one")

    def test_condition_with_unification(self):
        """Unification conditions work correctly."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- if(foo = foo).
            unifies.
            :- endif.
            
            :- if(foo = bar).
            does_not_unify.
            :- endif.
        """)
        assert prolog.has_solution("unifies")
        assert not prolog.has_solution("does_not_unify")

    def test_condition_with_compound_goal(self):
        """Compound goals (conjunction) work as conditions."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- if((true, true)).
            both_true.
            :- endif.
            
            :- if((true, fail)).
            first_true_second_fail.
            :- endif.
        """)
        assert prolog.has_solution("both_true")
        assert not prolog.has_solution("first_true_second_fail")


class TestConditionalStackIsolation:
    """Tests that conditional stack is properly isolated between consult calls."""

    def test_stack_isolated_between_consults(self):
        """Each consult has its own conditional stack."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- if(true).
            first_file_fact.
            :- endif.
        """)
        prolog.consult_string("""
            :- if(true).
            second_file_fact.
            :- endif.
        """)
        assert prolog.has_solution("first_file_fact")
        assert prolog.has_solution("second_file_fact")

    def test_unclosed_if_in_first_consult_errors(self):
        """Unclosed if in first consult doesn't affect second consult (after error)."""
        prolog = PrologInterpreter()
        with pytest.raises(PrologThrow):
            prolog.consult_string("""
                :- if(true).
                incomplete.
            """)
        # Should be able to consult new code after error
        prolog.consult_string("""
            :- if(true).
            after_error.
            :- endif.
        """)
        assert prolog.has_solution("after_error")
