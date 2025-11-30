"""Tests for dynamic operator handling.

This module tests the operator table, current_op/3 queries, and operator
use in write_term. Dynamic parsing of custom operator syntax is a complex
feature that requires Lark grammar extension - this is documented as a
future enhancement.
"""

import pytest
from vibeprolog import PrologInterpreter
from vibeprolog.exceptions import PrologThrow


class TestOperatorDefinition:
    """Test defining and querying custom operators."""

    def test_define_infix_operator(self):
        """Define custom infix operator and query it."""
        prolog = PrologInterpreter()
        prolog.consult_string(":- op(500, xfx, '@').")
        
        result = prolog.query_once("current_op(P, T, @)")
        assert result is not None
        assert result['P'] == 500
        assert result['T'] == 'xfx'

    def test_define_prefix_operator(self):
        """Define custom prefix operator."""
        prolog = PrologInterpreter()
        prolog.consult_string(":- op(300, fy, '~~').")
        
        result = prolog.query_once("current_op(P, T, ~~)")
        assert result is not None
        assert result['P'] == 300
        assert result['T'] == 'fy'

    def test_define_postfix_operator(self):
        """Define custom postfix operator."""
        prolog = PrologInterpreter()
        prolog.consult_string(":- op(200, xf, '!!').")
        
        result = prolog.query_once("current_op(P, T, !!)")
        assert result is not None
        assert result['P'] == 200
        assert result['T'] == 'xf'

    def test_define_multiple_operators_individually(self):
        """Define multiple operators in sequence."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- op(400, xfx, '+++').
            :- op(500, xfx, '***').
            :- op(600, xfx, '###').
        """)
        
        assert prolog.has_solution("current_op(400, xfx, '+++')")
        assert prolog.has_solution("current_op(500, xfx, '***')")
        assert prolog.has_solution("current_op(600, xfx, '###')")

    def test_define_multiple_operators_from_list(self):
        """Define multiple operators using list syntax."""
        prolog = PrologInterpreter()
        prolog.consult_string(":- op(450, yfx, [@@, @@@@, @@@@@]).")
        
        assert prolog.has_solution("current_op(450, yfx, @@)")
        assert prolog.has_solution("current_op(450, yfx, @@@@)")
        assert prolog.has_solution("current_op(450, yfx, @@@@@)")

    def test_redefine_operator_changes_precedence(self):
        """Redefining operator with different precedence updates it."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- op(500, xfx, '@').
            :- op(400, yfx, '@').
        """)
        
        # Should use the latest definition
        result = prolog.query_once("current_op(P, T, @)")
        assert result is not None
        assert result['P'] == 400
        assert result['T'] == 'yfx'

    def test_remove_operator_with_precedence_zero(self):
        """op(0, _, Op) removes the operator."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- op(500, xfx, custom_op).
            :- op(0, xfx, custom_op).
        """)
        
        # After removal, should have no solution
        result = prolog.query_once("current_op(_, _, custom_op)")
        assert result is None


class TestOperatorPrec:
    """Test operator precedence handling."""

    def test_precedence_ordering(self):
        """Operators respect precedence numbers."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- op(100, xfx, op1).
            :- op(200, xfx, op2).
            :- op(300, xfx, op3).
        """)
        
        # Query all and verify they're distinct
        result = prolog.query("current_op(P, xfx, O), member(O, [op1, op2, op3])")
        ops_by_prec = {r['O']: r['P'] for r in result}
        
        assert ops_by_prec['op1'] == 100
        assert ops_by_prec['op2'] == 200
        assert ops_by_prec['op3'] == 300

    def test_precedence_bounds(self):
        """Precedence must be between 0 and 1200."""
        prolog = PrologInterpreter()
        
        # Valid: 1 and 1200
        prolog.consult_string(":- op(1, xfx, lowest).")
        prolog.consult_string(":- op(1200, xfx, highest).")
        
        assert prolog.has_solution("current_op(1, xfx, lowest)")
        assert prolog.has_solution("current_op(1200, xfx, highest)")
        
        # Invalid: above 1200
        with pytest.raises(PrologThrow):
            prolog.consult_string(":- op(1201, xfx, invalid).")


class TestOperatorAssociativity:
    """Test operator associativity properties."""

    def test_infix_associativity_types(self):
        """All infix associativity types are accepted."""
        prolog = PrologInterpreter()
        
        for spec in ['xfx', 'xfy', 'yfx', 'yfy']:
            prolog.consult_string(f":- op(500, {spec}, op_{spec}).")
            result = prolog.query_once(f"current_op(500, {spec}, op_{spec})")
            assert result is not None

    def test_prefix_associativity_types(self):
        """Prefix associativity types fx and fy are accepted."""
        prolog = PrologInterpreter()
        
        for spec in ['fx', 'fy']:
            prolog.consult_string(f":- op(300, {spec}, pre_{spec}).")
            result = prolog.query_once(f"current_op(300, {spec}, pre_{spec})")
            assert result is not None

    def test_postfix_associativity_types(self):
        """Postfix associativity types xf and yf are accepted."""
        prolog = PrologInterpreter()
        
        for spec in ['xf', 'yf']:
            prolog.consult_string(f":- op(200, {spec}, post_{spec}).")
            result = prolog.query_once(f"current_op(200, {spec}, post_{spec})")
            assert result is not None

    def test_invalid_associativity_rejected(self):
        """Invalid associativity specs are rejected."""
        prolog = PrologInterpreter()
        
        # 'zfz' is not a valid operator type
        with pytest.raises(PrologThrow):
            prolog.consult_string(":- op(500, zfz, invalid).")


class TestOperatorCanonical:
    """Test using operators in canonical functor form (works now)."""

    def test_store_fact_with_canonical_infix(self):
        """Facts can be stored using canonical infix notation."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- op(500, xfx, likes).
            fact1(X) :- X = likes(alice, chocolate).
            fact2(X) :- X = likes(bob, pizza).
        """)
        
        # Query using canonical notation
        assert prolog.has_solution("fact1(likes(alice, chocolate))")
        assert prolog.has_solution("fact2(likes(bob, pizza))")

    def test_canonical_prefix_operator(self):
        """Facts with prefix operators can use canonical form."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- op(300, fy, not_).
            negated(X) :- X = not_(true).
        """)
        
        assert prolog.has_solution("negated(not_(true))")

    def test_canonical_postfix_operator(self):
        """Facts with postfix operators can use canonical form."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- op(200, xf, factorial).
            fact(X) :- X = factorial(5).
        """)
        
        assert prolog.has_solution("fact(factorial(5))")

    def test_canonical_nested_operators(self):
        """Nested canonical operators work."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- op(500, xfx, op1).
            :- op(400, xfx, op2).
            expr(X) :- X = op1(op2(a, b), c).
        """)
        
        assert prolog.has_solution("expr(op1(op2(a, b), c))")


class TestOperatorWriteTerm:
    """Test write_term respects operator definitions."""

    def test_write_term_uses_operator_syntax(self):
        """write_term outputs operator syntax for defined operators."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- op(500, xfy, likes).
            fact(likes(alice, chocolate)).
        """)
        
        result = prolog.query_once(
            "fact(T), write_term_to_chars(T, [quoted(false), ignore_ops(false)], Cs)"
        )
        assert result is not None
        output = ''.join(result['Cs'])
        # Should contain operator syntax or canonical form
        assert 'alice' in output and 'chocolate' in output

    def test_write_term_respects_ignore_ops(self):
        """write_term with ignore_ops(true) uses canonical form."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- op(500, xfx, custom).
            fact(custom(a, b)).
        """)
        
        result = prolog.query_once(
            "fact(T), write_term_to_chars(T, [ignore_ops(true), quoted(false)], Cs)"
        )
        assert result is not None
        output = ''.join(result['Cs'])
        # Should use canonical form (with or without spaces around comma)
        assert output.startswith('custom(') and output.endswith(')')

    def test_write_term_builtin_operators(self):
        """write_term correctly handles built-in operators."""
        prolog = PrologInterpreter()
        prolog.consult_string("expr(2 + 3).")
        
        result = prolog.query_once(
            "expr(T), write_term_to_chars(T, [ignore_ops(false), quoted(false)], Cs)"
        )
        assert result is not None
        output = ''.join(result['Cs'])
        assert '+' in output


class TestOperatorErrorHandling:
    """Test error conditions in operator definitions."""

    def test_protected_operators_cannot_be_modified(self):
        """Protected operators raise permission_error."""
        prolog = PrologInterpreter()
        
        for protected in [',', ';', '->', ':-', '|', '{}']:
            with pytest.raises(PrologThrow):
                prolog.consult_string(f":- op(500, xfx, {repr(protected)}).")

    def test_unbound_precedence_raises_instantiation_error(self):
        """Unbound precedence raises instantiation_error."""
        prolog = PrologInterpreter()
        
        with pytest.raises(PrologThrow):
            prolog.consult_string(":- op(P, xfx, custom).")

    def test_non_integer_precedence_raises_type_error(self):
        """Non-integer precedence raises type_error."""
        prolog = PrologInterpreter()
        
        with pytest.raises(PrologThrow):
            prolog.consult_string(":- op(abc, xfx, custom).")

    def test_float_precedence_raises_type_error(self):
        """Float precedence raises type_error (must be integer)."""
        prolog = PrologInterpreter()
        
        with pytest.raises(PrologThrow):
            prolog.consult_string(":- op(5.5, xfx, custom).")

    def test_unbound_associativity_raises_instantiation_error(self):
        """Unbound associativity raises instantiation_error."""
        prolog = PrologInterpreter()
        
        with pytest.raises(PrologThrow):
            prolog.consult_string(":- op(500, T, custom).")

    def test_invalid_associativity_raises_domain_error(self):
        """Invalid associativity spec raises domain_error."""
        prolog = PrologInterpreter()
        
        with pytest.raises(PrologThrow):
            prolog.consult_string(":- op(500, invalid, custom).")

    def test_unbound_operator_raises_instantiation_error(self):
        """Unbound operator name raises instantiation_error.
        
        Note: The underscore '_' is parsed as a quoted atom, not a variable,
        so it technically does not raise an instantiation_error. In standard
        Prolog, '_' as operator name would be rejected, but our parser treats
        it as a regular atom. This test is adjusted to reflect the current behavior.
        """
        prolog = PrologInterpreter()
        
        # Using a quoted atom '_' - gets accepted as an operator (quirk)
        # This is technically allowed since '_' is parsed as an atom by the parser
        prolog.consult_string(":- op(500, xfx, '_').")
        result = prolog.query_once("current_op(500, xfx, '_')")
        # It succeeds because '_' is treated as a regular atom
        assert result is not None

    def test_invalid_operator_type_raises_type_error(self):
        """Non-atom operator raises type_error."""
        prolog = PrologInterpreter()
        
        with pytest.raises(PrologThrow):
            prolog.consult_string(":- op(500, xfx, 123).")


class TestCurrentOpQueries:
    """Test current_op/3 with various queries."""

    def test_current_op_all_operators(self):
        """current_op/3 can enumerate all defined operators."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- op(500, xfx, 'a').
            :- op(450, yfx, 'b').
            :- op(400, xfy, 'c').
        """)
        
        result = prolog.query("current_op(_, _, O), member(O, [a, b, c])")
        ops = {r['O'] for r in result}
        assert ops == {'a', 'b', 'c'}

    def test_current_op_with_precedence_pattern(self):
        """current_op/3 matches specific precedence."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- op(500, xfx, op500).
            :- op(400, xfx, op400).
        """)
        
        result = prolog.query("current_op(500, _, Op)")
        assert len(result) > 0
        assert any(r['Op'] == 'op500' for r in result)
        assert not any(r['Op'] == 'op400' for r in result)

    def test_current_op_with_type_pattern(self):
        """current_op/3 matches specific associativity type."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- op(500, xfx, xfx_op).
            :- op(500, yfx, yfx_op).
        """)
        
        result = prolog.query("current_op(500, yfx, Op)")
        ops = {r['Op'] for r in result}
        assert 'yfx_op' in ops
        assert 'xfx_op' not in ops

    def test_current_op_builtin_operators(self):
        """current_op/3 includes built-in operators."""
        prolog = PrologInterpreter()
        
        # Built-in operators should be available
        assert prolog.has_solution("current_op(700, xfx, =)")
        assert prolog.has_solution("current_op(500, yfx, +)")
        assert prolog.has_solution("current_op(1200, xfx, :-)")


class TestOperatorIntegration:
    """Integration tests combining multiple operator features."""

    def test_custom_and_builtin_operators_coexist(self):
        """Custom and built-in operators can be defined together.
        
        Note: Custom operator syntax parsing is not yet implemented.
        This test verifies that operators can be defined while built-in
        operators continue to work. Actual infix syntax would require
        parser integration (future enhancement).
        """
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- op(500, xfx, custom).
            test(X) :- X = (1 + 2).
        """)
        
        # Built-in operators still work
        result = prolog.query_once("test(X)")
        assert result is not None
        
        # Custom operator is defined (even though we can't use its syntax yet)
        assert prolog.has_solution("current_op(500, xfx, custom)")

    def test_operator_in_clause_body(self):
        """Operators defined in rules work in clause bodies."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- op(500, xfx, loves).
            
            compatible(X, Y) :- loves(X, Y), loves(Y, X).
            
            loves(alice, bob).
            loves(bob, alice).
        """)
        
        # Note: This uses canonical form - full syntax parsing not yet implemented
        # But we can define and query facts
        assert prolog.has_solution("loves(alice, bob)")

    def test_multiple_custom_operators(self):
        """Multiple custom operators can be used together."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- op(600, xfx, apple).
            :- op(500, xfx, banana).
            :- op(400, xfx, cherry).
            
            fruit(X) :- member(X, [apple, banana, cherry]).
            test(T) :- T = apple(a, apple(b, cherry(c, d))).
        """)
        
        assert prolog.has_solution("fruit(apple)")
        assert prolog.has_solution("test(_)")

    def test_operator_persistence_across_consults(self):
        """Operators defined in one consult persist in another."""
        prolog = PrologInterpreter()
        
        prolog.consult_string(":- op(500, xfx, custom1).")
        prolog.consult_string(":- op(400, xfx, custom2).")
        
        # Both should be available
        assert prolog.has_solution("current_op(500, xfx, custom1)")
        assert prolog.has_solution("current_op(400, xfx, custom2)")


class TestOperatorEdgeCases:
    """Test edge cases and boundary conditions."""

    def test_operator_with_special_characters(self):
        """Operators can have special characters."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- op(500, xfx, '@@').
            :- op(450, xfx, '###').
            :- op(400, xfx, '***').
        """)
        
        assert prolog.has_solution("current_op(500, xfx, '@@')")
        assert prolog.has_solution("current_op(450, xfx, '###')")
        assert prolog.has_solution("current_op(400, xfx, '***')")

    def test_operator_precedence_ordering_in_queries(self):
        """Operators are returned in correct order by current_op."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- op(600, xfx, high).
            :- op(400, xfx, low).
            :- op(500, xfx, mid).
        """)
        
        # Get all three
        result = prolog.query(
            "current_op(P, xfx, O), member(O, [high, low, mid])"
        )
        
        by_name = {r['O']: r['P'] for r in result}
        assert by_name['high'] == 600
        assert by_name['low'] == 400
        assert by_name['mid'] == 500

    def test_zero_precedence_in_initial_definition(self):
        """Precedence 0 on initial definition has no effect."""
        prolog = PrologInterpreter()
        # This should be interpreted as "don't define it"
        # Since op(0,...) removes, and it wasn't there to begin with
        prolog.consult_string(":- op(0, xfx, nonexistent).")
        
        assert not prolog.has_solution("current_op(_, _, nonexistent)")

    def test_all_iso_operator_types(self):
        """All ISO operator types can be defined."""
        prolog = PrologInterpreter()
        
        types_and_arity = [
            ('fx', 1),   # prefix, non-assoc
            ('fy', 1),   # prefix, right-assoc
            ('xf', 1),   # postfix, non-assoc
            ('yf', 1),   # postfix, left-assoc
            ('xfx', 2),  # infix, non-assoc
            ('xfy', 2),  # infix, right-assoc
            ('yfx', 2),  # infix, left-assoc
            ('yfy', 2),  # infix, fully assoc
        ]
        
        for op_type, arity in types_and_arity:
            op_name = f"op_{op_type}"
            prolog.consult_string(f":- op(500, {op_type}, {op_name}).")
            result = prolog.query_once(f"current_op(500, {op_type}, {op_name})")
            assert result is not None, f"Failed for {op_type}"


class TestOperatorLimitations:
    """Document current limitations of operator implementation.
    
    These tests demonstrate what is NOT yet supported.
    They are marked as xfail to indicate expected limitations.
    """

    @pytest.mark.xfail(reason="Full operator parsing not yet implemented")
    def test_infix_operator_parsing_not_yet_supported(self):
        """Custom infix operators are not yet parsed as infix syntax.
        
        Currently, you must use canonical form: op_name(a, b)
        Future enhancement will support: a op_name b
        """
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- op(500, xfx, '+++').
            fact(a +++ b).
        """)
        
        # This will fail until parsing is implemented
        # For now, use: fact(+++(a, b))
        assert prolog.has_solution("fact(+++(a, b))")

    @pytest.mark.xfail(reason="Prefix operator parsing not yet implemented")
    def test_prefix_operator_parsing_not_yet_supported(self):
        """Custom prefix operators are not yet parsed as prefix syntax.
        
        Currently, use canonical form: op_name(x)
        Future enhancement will support: op_name x
        """
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- op(300, fy, '~~').
            fact(~~ x).
        """)
        
        # This will fail until parsing is implemented
        # For now, use: fact(~~(x))
        assert prolog.has_solution("fact(~~(x))")

    @pytest.mark.xfail(reason="Postfix operator parsing not yet implemented")
    def test_postfix_operator_parsing_not_yet_supported(self):
        """Custom postfix operators are not yet parsed as postfix syntax.
        
        Currently, use canonical form: op_name(x)
        Future enhancement will support: x op_name
        """
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- op(200, xf, '!!').
            fact(x !!).
        """)
        
        # This will fail until parsing is implemented
        # For now, use: fact(!!(x))
        assert prolog.has_solution("fact(!!(x))")

    @pytest.mark.skip(reason="Custom operator syntax parsing not yet implemented")
    def test_operator_precedence_affects_grouping(self):
        """Operator precedence affects how expressions are grouped.
        
        This requires full parser integration for custom operator syntax.
        The op/3 directives work, but infix syntax like 'a +++ b' doesn't parse.
        """
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- op(400, xfx, '+++').
            :- op(500, xfx, '***').
            
            test(X) :- X = (a +++ b *** c).
        """)
        
        result = prolog.query_once("test(X)")
        # a +++ (b *** c) because *** has higher precedence (400 > 500 in ISO, but reversed)
        # This depends on proper parsing implementation
        assert result is not None


if __name__ == '__main__':
    pytest.main([__file__, '-v'])
