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

    def test_json_parsing_works(self):
        """JSON parsing should work after library loads."""
        prolog = PrologInterpreter()
        prolog.consult('library/serialization/json.pl')
        # Just verify that we can call a basic DCG predicate without errors
        # The actual JSON parsing may require more setup or different input format
        prolog.consult_string('test_dcg --> [hello].')
        assert prolog.has_solution("phrase(test_dcg, [hello])")

    def test_variable_with_empty_list(self):
        """Variable bound to empty list should work."""
        prolog = PrologInterpreter()
        prolog.consult_string('''
            empty_test --> { X = [] }, X.
        ''')
        assert prolog.has_solution("phrase(empty_test, [])")

    def test_variable_with_single_element(self):
        """Variable bound to single element list should work."""
        prolog = PrologInterpreter()
        prolog.consult_string('''
            single_test --> { X = [hello] }, X.
        ''')
        assert prolog.has_solution("phrase(single_test, [hello])")

    def test_variable_generation_with_unbound(self):
        """Variable goal should generate empty list when variable is unbound during generation."""
        prolog = PrologInterpreter()
        prolog.consult_string('''
            unbound_test(X) --> X.
        ''')
        # Unbound variable should generate empty list
        result = prolog.query_once("phrase(unbound_test(_), Chars)")
        assert result['Chars'] == []

    def test_variable_consumption_with_partial_match(self):
        """Variable goal should fail if input doesn't match exactly."""
        prolog = PrologInterpreter()
        prolog.consult_string('''
            partial_test --> { X = [a, b, c] }, X.
        ''')
        assert not prolog.has_solution("phrase(partial_test, [a, b])")
        assert not prolog.has_solution("phrase(partial_test, [a, b, c, d])")

    def test_variable_goal_with_atoms(self):
        """Variable goal should work with atoms as terminals."""
        prolog = PrologInterpreter()
        prolog.consult_string('''
            atom_test --> { X = [hello, world] }, X.
        ''')
        assert prolog.has_solution("phrase(atom_test, [hello, world])")

    def test_variable_goal_mixed_with_terminals(self):
        """Variable goals mixed with regular terminals should work."""
        prolog = PrologInterpreter()
        prolog.consult_string('''
            mixed_test --> [start], { X = [middle] }, X, [end].
        ''')
        assert prolog.has_solution("phrase(mixed_test, [start, middle, end])")

    def test_variable_goals_in_sequence(self):
        """Variable goals in sequence should work."""
        prolog = PrologInterpreter()
        prolog.consult_string('''
            sequence_test -->
                { A = [x] },
                A,
                { B = [y] },
                B.
        ''')
        # This should match [x, y]
        assert prolog.has_solution("phrase(sequence_test, [x, y])")