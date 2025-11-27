"""Tests for Definite Clause Grammar (DCG) support."""

import pytest

from vibeprolog import PrologInterpreter


class TestDCGBasics:
    """Basic DCG functionality tests."""

    def test_simple_terminal(self):
        """Test simple terminal matching."""
        prolog = PrologInterpreter()
        prolog.consult_string("s --> [hello].")
        assert prolog.has_solution("phrase(s, [hello])")

    def test_terminal_sequence(self):
        """Test sequence of terminals."""
        prolog = PrologInterpreter()
        prolog.consult_string("s --> [a, b, c].")
        result = prolog.query_once("phrase(s, [a, b, c])")
        assert result is not None

    def test_terminal_failure(self):
        """Test terminal matching failure."""
        prolog = PrologInterpreter()
        prolog.consult_string("s --> [hello].")
        assert not prolog.has_solution("phrase(s, [world])")

    def test_empty_production(self):
        """Test empty production []."""
        prolog = PrologInterpreter()
        prolog.consult_string("optional --> [].")
        assert prolog.has_solution("phrase(optional, [])")


class TestDCGNonTerminals:
    """Tests for non-terminal calls in DCGs."""

    def test_simple_nonterminal(self):
        """Test simple non-terminal call."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            noun --> [cat].
            sentence --> noun.
        """)
        assert prolog.has_solution("phrase(sentence, [cat])")

    def test_nonterminal_sequence(self):
        """Test sequence of non-terminals."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            article --> [the].
            noun --> [cat].
            noun_phrase --> article, noun.
        """)
        assert prolog.has_solution("phrase(noun_phrase, [the, cat])")

    def test_recursive_nonterminal(self):
        """Test recursive non-terminal."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            digit --> [0].
            digit --> [1].
            digits --> digit.
            digits --> digit, digits.
        """)
        assert prolog.has_solution("phrase(digits, [1, 0, 1])")


class TestDCGEmbedded:
    """Tests for embedded Prolog goals {Goal} in DCGs."""

    def test_embedded_goal_success(self):
        """Test embedded goal that succeeds."""
        prolog = PrologInterpreter()
        prolog.consult_string("number(X) --> [X], {integer(X)}.")
        assert prolog.has_solution("phrase(number(42), [42])")

    def test_embedded_goal_failure(self):
        """Test embedded goal that fails."""
        prolog = PrologInterpreter()
        prolog.consult_string("number(X) --> [X], {integer(X)}.")
        assert not prolog.has_solution("phrase(number(cat), [cat])")

    def test_embedded_goal_with_variables(self):
        """Test embedded goal with variable binding."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            pair(A, B) --> [A, B], {A = B}.
        """)
        assert prolog.has_solution("phrase(pair(X, X), [hello, hello])")
        assert not prolog.has_solution("phrase(pair(X, Y), [hello, world])")


class TestDCGAlternatives:
    """Tests for alternatives (;) in DCGs."""

    def test_simple_alternative(self):
        """Test simple alternative."""
        prolog = PrologInterpreter()
        prolog.consult_string("animal --> [cat] ; [dog].")
        assert prolog.has_solution("phrase(animal, [cat])")
        assert prolog.has_solution("phrase(animal, [dog])")
        assert not prolog.has_solution("phrase(animal, [bird])")

    def test_alternative_with_sequence(self):
        """Test alternative with sequences."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            greeting --> [hello] ; [hi].
            name --> [world] ; [there].
            sentence --> greeting, name.
        """)
        assert prolog.has_solution("phrase(sentence, [hello, world])")
        assert prolog.has_solution("phrase(sentence, [hi, there])")


class TestDCGCut:
    """Tests for cut (!) in DCGs."""

    def test_cut_in_dcg(self):
        """Test cut operator in DCG."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            test --> [a], !, fail.
            test --> [a].
        """)
        # The first clause has cut and then fail, so it should not backtrack to the second clause
        assert not prolog.has_solution("phrase(test, [a])")


class TestDCGPhrase:
    """Tests for phrase/2 and phrase/3 builtins."""

    def test_phrase_2_success(self):
        """Test phrase/2 with successful match."""
        prolog = PrologInterpreter()
        prolog.consult_string("word --> [hello].")
        assert prolog.has_solution("phrase(word, [hello])")

    def test_phrase_2_failure(self):
        """Test phrase/2 with failed match."""
        prolog = PrologInterpreter()
        prolog.consult_string("word --> [hello].")
        assert not prolog.has_solution("phrase(word, [world])")

    def test_phrase_3_with_remainder(self):
        """Test phrase/3 with remainder."""
        prolog = PrologInterpreter()
        prolog.consult_string("word --> [hello].")
        result = prolog.query_once("phrase(word, [hello, world], Rest)")
        assert result is not None
        assert result["Rest"] == ["world"]

    def test_phrase_3_complete_consumption(self):
        """Test phrase/3 with complete consumption."""
        prolog = PrologInterpreter()
        prolog.consult_string("word --> [hello].")
        result = prolog.query_once("phrase(word, [hello], Rest)")
        assert result is not None
        assert result["Rest"] == []


class TestDCGComplex:
    """Complex DCG tests combining multiple features."""

    def test_arithmetic_expression_parser(self):
        """Test parsing arithmetic expressions."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            digit(X) --> [X], {integer(X), X >= 0, X =< 9}.
            number(X) --> digit(Y), {X is Y}.
            number(X) --> digit(Y), number(Z), {X is Y * 10 + Z}.
        """)
        assert prolog.has_solution("phrase(number(42), [4, 2])")

    def test_sentence_parser(self):
        """Test simple sentence parsing."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            article --> [the] ; [a].
            noun --> [cat] ; [dog] ; [house].
            verb --> [chases] ; [sees].
            sentence --> article, noun, verb, article, noun.
        """)
        assert prolog.has_solution("phrase(sentence, [the, cat, chases, a, dog])")

    def test_parameterized_dcg(self):
        """Test DCG rules with parameters."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            rep(0) --> [].
            rep(N) --> [x], rep(M), {N is M + 1}.
        """)
        assert prolog.has_solution("phrase(rep(3), [x, x, x])")
        assert prolog.has_solution("phrase(rep(0), [])")


class TestDCGIntegration:
    """Integration tests combining DCG with regular Prolog."""

    def test_dcg_with_regular_predicates(self):
        """Test DCG rules calling regular Prolog predicates."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            is_noun(cat).
            is_noun(dog).
            noun(X) --> [X], {is_noun(X)}.
            sentence --> noun(X), [is, nice].
        """)
        result = prolog.query_once("phrase(sentence, [cat, is, nice])")
        assert result is not None

    def test_multiple_dcg_rules(self):
        """Test multiple DCG rules working together."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            s --> np, vp.
            np --> det, n.
            vp --> v, np.
            det --> [the].
            n --> [cat].
            v --> [chases].
        """)
        assert prolog.has_solution("phrase(s, [the, cat, chases, the, cat])")


class TestDCGErrorCases:
    """Tests for error conditions in DCGs."""

    def test_phrase_undefined_predicate(self):
        """Test phrase/2 with undefined DCG predicate."""
        prolog = PrologInterpreter()
        # Should fail because 'undefined' is not defined
        assert not prolog.has_solution("phrase(undefined, [])")

    def test_malformed_dcg_syntax(self):
        """Test malformed DCG syntax."""
        prolog = PrologInterpreter()
        # This should raise a syntax error
        with pytest.raises(Exception):
            prolog.consult_string("invalid --> [missing, dot]")


class TestDCGExpansion:
    """Tests that verify DCG expansion produces correct Prolog code."""

    def test_dcg_expansion_basic(self):
        """Test that DCG rules are properly expanded."""
        prolog = PrologInterpreter()
        prolog.consult_string("test --> [hello].")

        # The expanded rule should be: test(S0, S) :- S0 = [hello | S]
        # So phrase(test, [hello]) should work
        assert prolog.has_solution("phrase(test, [hello])")

    def test_dcg_expansion_with_nonterminal(self):
        """Test DCG expansion with non-terminal calls."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            a --> [x].
            b --> a, [y].
        """)

        # Should expand to: b(S0, S) :- a(S0, S1), S1 = [y | S]
        assert prolog.has_solution("phrase(b, [x, y])")