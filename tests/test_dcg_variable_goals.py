import pytest
from vibeprolog import PrologInterpreter


class TestDCGVariableGoals:
    """Tests for DCG variable goal support."""

    def test_variable_terminal_consume(self):
        """Variable as DCG goal should consume matching terminals."""
        prolog = PrologInterpreter()
        prolog.consult_string('''
            foo --> { X = [a, b, c] }, X.
        ''')
        assert prolog.has_solution("phrase(foo, [a, b, c])")
        assert not prolog.has_solution("phrase(foo, [a, b])")

    def test_variable_terminal_generate(self):
        """Variable as DCG goal should generate terminals."""
        prolog = PrologInterpreter()
        prolog.consult_string('''
            bar(X) --> X.
        ''')
        result = prolog.query_once("phrase(bar([h,e,l,l,o]), Chars)")
        assert result['Chars'] == ['h', 'e', 'l', 'l', 'o']

    def test_multiple_variable_goals(self):
        """Multiple variables as DCG goals should work."""
        prolog = PrologInterpreter()
        prolog.consult_string('''
            baz --> { X = [a], Y = [b] }, X, Y.
        ''')
        assert prolog.has_solution("phrase(baz, [a, b])")

    def test_variable_in_disjunction(self):
        """Variable goal in disjunction should work (json.pl pattern)."""
        prolog = PrologInterpreter()
        prolog.consult_string('''
            qux(N) -->
                ( [d], { N = 1 }
                ; { number_chars(N, Chars) }, Chars
                ).
        ''')
        assert prolog.has_solution("phrase(qux(1), [d])")
        assert prolog.has_solution("phrase(qux(42), ['4', '2'])")

    def test_json_library_loads(self):
        """library/serialization/json.pl should load successfully."""
        prolog = PrologInterpreter()
        prolog.consult('library/serialization/json.pl')

    def test_json_library_variable_goals_work(self):
        """JSON library uses variable goals and should load successfully."""
        # The main test is that loading doesn't raise ValueError about unsupported DCG goal
        prolog = PrologInterpreter()
        prolog.consult('library/serialization/json.pl')
        # If we get here without exception, the variable goals are working

    def test_variable_with_empty_list(self):
        """Variable bound to empty list should work."""
        prolog = PrologInterpreter()
        prolog.consult_string('''
            empty_test --> { X = [] }, X.
        ''')
        assert prolog.has_solution("phrase(empty_test, [])")

    def test_variable_unbound_generates(self):
        """Unbound variable as DCG goal should generate terminals."""
        prolog = PrologInterpreter()
        prolog.consult_string('''
            unbound_test(X) --> X.
        ''')
        # This should succeed for generation - X gets unified with the input
        result = prolog.query_once("phrase(unbound_test(X), [a, b])")
        assert result['X'] == ['a', 'b']

    def test_variable_bound_to_non_list_fails(self):
        """Variable bound to non-list should fail at runtime."""
        prolog = PrologInterpreter()
        prolog.consult_string('''
            nonlist_test --> { X = atom }, X.
        ''')
        # This should fail because append expects lists
        assert not prolog.has_solution("phrase(nonlist_test, [a])")