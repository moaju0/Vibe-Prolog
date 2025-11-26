"""Tests for ISO Prolog existence_error exception handling.

This module tests that existence_error is properly raised in various scenarios
where ISO Prolog requires it, such as:
- Calling undefined predicates via call/1
- Missing files (when file I/O is implemented)
- Missing streams (when stream I/O is implemented)
"""

import pytest

from vibeprolog import PrologInterpreter
from vibeprolog.terms import Atom, Compound, Number


def check_existence_error(error_dict, expected_procedure_name, expected_arity, expected_context="call/1"):
    """Helper function to check that an error dictionary represents a valid existence_error.

    Args:
        error_dict: The error dictionary from query results
        expected_procedure_name: The expected procedure name (e.g., "undefined_pred")
        expected_arity: The expected arity (e.g., 0 or 3)
        expected_context: The expected context predicate (default: "call/1")
    """
    assert isinstance(error_dict, dict)
    assert 'error' in error_dict

    error_parts = error_dict['error']
    assert isinstance(error_parts, list)
    assert len(error_parts) == 2

    # Check existence_error(procedure, name/arity)
    error_type = error_parts[0]
    assert isinstance(error_type, dict)
    assert 'existence_error' in error_type
    existence_parts = error_type['existence_error']
    assert isinstance(existence_parts, list)
    assert len(existence_parts) == 2
    assert existence_parts[0] == "procedure"

    # Check the procedure indicator
    indicator = existence_parts[1]
    assert isinstance(indicator, dict)
    assert '/' in indicator
    indicator_parts = indicator['/']
    assert indicator_parts[0] == expected_procedure_name
    assert indicator_parts[1] == expected_arity

    # Check the context
    context = error_parts[1]
    assert isinstance(context, dict)
    assert 'context' in context
    assert context['context'][0] == expected_context


def check_instantiation_error(error_dict, expected_context="call/1"):
    """Helper function to check that an error dictionary represents a valid instantiation_error.

    Args:
        error_dict: The error dictionary from query results
        expected_context: The expected context predicate (default: "call/1")
    """
    assert isinstance(error_dict, dict)
    assert 'error' in error_dict

    error_parts = error_dict['error']
    assert isinstance(error_parts, list)
    assert len(error_parts) == 2

    # Check instantiation_error
    error_type = error_parts[0]
    assert error_type == "instantiation_error"

    # Check the context
    context = error_parts[1]
    assert isinstance(context, dict)
    assert 'context' in context
    assert context['context'][0] == expected_context


def check_type_error(error_dict, expected_type, expected_culprit, expected_context="call/1"):
    """Helper function to check that an error dictionary represents a valid type_error.

    Args:
        error_dict: The error dictionary from query results
        expected_type: The expected type (e.g., "callable")
        expected_culprit: The expected culprit value
        expected_context: The expected context predicate (default: "call/1")
    """
    assert isinstance(error_dict, dict)
    assert 'error' in error_dict

    error_parts = error_dict['error']
    assert isinstance(error_parts, list)
    assert len(error_parts) == 2

    # Check type_error(type, culprit)
    error_type = error_parts[0]
    assert isinstance(error_type, dict)
    assert 'type_error' in error_type
    type_error_parts = error_type['type_error']
    assert isinstance(type_error_parts, list)
    assert len(type_error_parts) == 2
    assert type_error_parts[0] == expected_type
    assert type_error_parts[1] == expected_culprit

    # Check the context
    context = error_parts[1]
    assert isinstance(context, dict)
    assert 'context' in context
    assert context['context'][0] == expected_context


class TestExistenceError:
    """Tests for existence_error exception handling."""

    def test_call_undefined_predicate_zero_arity(self):
        """Test that call/1 raises existence_error for undefined zero-arity predicate."""
        prolog = PrologInterpreter()

        # Try to call an undefined predicate via call/1
        result = prolog.query_once("catch(call(undefined_pred), Error, true)")
        assert result is not None

        # Check that the error has the correct structure
        error = result.get('Error')
        check_existence_error(error, "undefined_pred", 0)

    def test_call_undefined_predicate_with_args(self):
        """Test that call/1 raises existence_error for undefined predicate with arguments."""
        prolog = PrologInterpreter()

        # Try to call an undefined predicate with arguments via call/1
        result = prolog.query_once("catch(call(undefined_pred(a, b, c)), Error, true)")
        assert result is not None

        # Check that the error has the correct structure
        error = result.get('Error')
        check_existence_error(error, "undefined_pred", 3)

    def test_call_defined_predicate_succeeds(self):
        """Test that call/1 succeeds for a defined predicate."""
        prolog = PrologInterpreter()
        prolog.consult_string("my_pred(hello).")

        # Call a defined predicate - should succeed without error
        result = prolog.query_once("call(my_pred(X))")
        assert result is not None
        assert result['X'] == "hello"

    def test_call_builtin_predicate_succeeds(self):
        """Test that call/1 succeeds for built-in predicates."""
        prolog = PrologInterpreter()

        # Call a built-in predicate - should succeed without error
        result = prolog.query_once("call(atom(hello))")
        assert result is not None

    def test_call_undefined_operator(self):
        """Test that call/1 raises existence_error for undefined operators.

        This is inspired by ISO conformity test case test_224.
        """
        prolog = PrologInterpreter()

        # Try to call an undefined atom that could be an operator (like \\)
        # We'll use a simpler example that doesn't involve string escaping issues
        result = prolog.query_once("catch(call(xor), Error, true)")
        assert result is not None

        # Check that the error has the correct structure
        error = result.get('Error')
        check_existence_error(error, "xor", 0)

    def test_existence_error_without_catch_fails(self):
        """Test that existence_error causes query to fail if not caught."""
        prolog = PrologInterpreter()

        # Without catch, the error should cause the query to fail
        result = prolog.query_once("call(undefined_pred)")
        assert result is None

    def test_call_with_variable_unbound(self):
        """Test that call/1 with unbound variable raises instantiation_error.

        According to ISO standard, call/1 should check for instantiation errors
        before checking for existence errors.
        """
        prolog = PrologInterpreter()

        # Try to call an unbound variable - should raise instantiation_error
        result = prolog.query_once("catch(call(X), Error, true)")
        assert result is not None

        error = result.get('Error')
        check_instantiation_error(error)

    def test_call_with_non_callable(self):
        """Test that call/1 with non-callable term raises type_error.

        According to ISO standard, call/1 should raise type_error(callable, Culprit)
        for non-callable terms like numbers or lists.
        """
        prolog = PrologInterpreter()

        # Try to call a number - should raise type_error
        result = prolog.query_once("catch(call(123), Error, true)")
        assert result is not None

        error = result.get('Error')
        check_type_error(error, "callable", 123)

    def test_direct_call_to_undefined_predicate_fails(self):
        """Test that directly calling an undefined predicate just fails (no error).

        This is traditional Prolog behavior - only call/1 raises existence_error.
        """
        prolog = PrologInterpreter()

        # Direct call to undefined predicate should just fail
        result = prolog.query_once("undefined_pred")
        assert result is None

        # With arguments
        result = prolog.query_once("undefined_pred(a, b, c)")
        assert result is None

    def test_existence_error_structure(self):
        """Test that existence_error has the correct structure."""
        prolog = PrologInterpreter()

        # Catch the error and examine its structure
        result = prolog.query_once("""
            catch(
                call(nonexistent),
                error(existence_error(ObjectType, Culprit), Context),
                true
            )
        """)
        assert result is not None

        # Check that the error was successfully matched and unified
        assert result['ObjectType'] == "procedure"
        assert isinstance(result['Culprit'], dict)
        assert '/' in result['Culprit']
        assert isinstance(result['Context'], dict)
        assert 'context' in result['Context']

    def test_multiple_undefined_predicates(self):
        """Test that each undefined predicate raises its own existence_error."""
        prolog = PrologInterpreter()

        # First undefined predicate
        result1 = prolog.query_once("catch(call(pred1), E1, true)")
        assert result1 is not None
        error1 = result1['E1']
        check_existence_error(error1, "pred1", 0)

        # Second undefined predicate
        result2 = prolog.query_once("catch(call(pred2), E2, true)")
        assert result2 is not None
        error2 = result2['E2']
        check_existence_error(error2, "pred2", 0)

    def test_existence_error_in_complex_goal(self):
        """Test that existence_error is properly raised in complex goal structures."""
        prolog = PrologInterpreter()
        prolog.consult_string("defined_pred(x).")

        # Calling a conjunction where the second part is undefined
        # Note: call/1 will check if the conjunction itself exists, not the sub-goals
        # So we need to call the goals separately
        result = prolog.query_once("catch((defined_pred(X), call(undefined_pred)), Error, true)")
        assert result is not None
        error = result['Error']
        check_existence_error(error, "undefined_pred", 0)

    def test_existence_error_with_nested_catch(self):
        """Test that existence_error can be caught by nested catch/3."""
        prolog = PrologInterpreter()

        # Nested catch - inner catch should handle the error
        result = prolog.query_once("""
            catch(
                catch(call(undefined), E1, throw(caught(E1))),
                caught(E2),
                true
            )
        """)
        assert result is not None

        # The outer catch should catch the rethrown error
        error = result['E2']
        assert isinstance(error, dict)
        assert 'error' in error
        check_existence_error(error, "undefined", 0)


class TestExistenceErrorEdgeCases:
    """Tests for edge cases in existence_error handling."""

    def test_existence_error_for_atom_predicate(self):
        """Test existence_error for atom predicates (zero-arity)."""
        prolog = PrologInterpreter()

        result = prolog.query_once("catch(call(undefined_atom), Error, true)")
        assert result is not None

        error = result['Error']
        check_existence_error(error, "undefined_atom", 0)

    def test_existence_error_for_compound_predicate(self):
        """Test existence_error for compound predicates."""
        prolog = PrologInterpreter()

        result = prolog.query_once("catch(call(foo(a, b)), Error, true)")
        assert result is not None

        error = result['Error']
        check_existence_error(error, "foo", 2)

    def test_predicate_exists_after_assert(self):
        """Test that predicate exists after being asserted."""
        prolog = PrologInterpreter()

        # First, the predicate doesn't exist
        result = prolog.query_once("catch(call(dynamic_pred), Error, true)")
        assert result is not None
        assert 'Error' in result

        # Assert the predicate
        prolog.query_once("assertz(dynamic_pred)")

        # Now it should exist and succeed
        result = prolog.query_once("call(dynamic_pred)")
        assert result is not None

    def test_predicate_does_not_exist_after_retract_all(self):
        """Test that existence_error is raised after retracting all clauses."""
        prolog = PrologInterpreter()
        prolog.consult_string("temp_pred.")

        # Predicate exists
        result = prolog.query_once("call(temp_pred)")
        assert result is not None

        # Retract all clauses
        prolog.query_once("retract(temp_pred)")

        # Now it should not exist
        result = prolog.query_once("catch(call(temp_pred), Error, true)")
        assert result is not None
        assert 'Error' in result


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
