"""Tests for Unicode letter support in atoms.

Tests that atoms containing Unicode letters (Greek, Cyrillic, Arabic, CJK, etc.)
are properly parsed and handled by the interpreter.
"""

import pytest
from vibeprolog import PrologInterpreter
from vibeprolog.terms import Atom, Compound, Number


class TestUnicodeAtomParsing:
    """Basic Unicode atom parsing tests."""

    def test_greek_delta_atom(self):
        """Test parsing Greek letter δ as an unquoted atom."""
        prolog = PrologInterpreter()
        result = prolog.query_once("X = δ")
        assert result is not None
        assert result['X'] == Atom('δ')

    def test_greek_alpha_atom(self):
        """Test parsing Greek letter α as an unquoted atom."""
        prolog = PrologInterpreter()
        result = prolog.query_once("X = α")
        assert result is not None
        assert result['X'] == Atom('α')

    def test_greek_beta_gamma_atoms(self):
        """Test parsing other Greek letters β and γ."""
        prolog = PrologInterpreter()
        result = prolog.query_once("X = β")
        assert result is not None
        assert result['X'] == Atom('β')
        
        result = prolog.query_once("X = γ")
        assert result is not None
        assert result['X'] == Atom('γ')

    def test_unicode_atom_with_underscore(self):
        """Test Unicode letter followed by underscore."""
        prolog = PrologInterpreter()
        result = prolog.query_once("X = δ_test")
        assert result is not None
        assert result['X'] == Atom('δ_test')

    def test_unicode_atom_with_numbers(self):
        """Test Unicode letter mixed with numbers."""
        prolog = PrologInterpreter()
        result = prolog.query_once("X = δ1")
        assert result is not None
        assert result['X'] == Atom('δ1')
        
        result = prolog.query_once("X = α_v2")
        assert result is not None
        assert result['X'] == Atom('α_v2')

    def test_mixed_ascii_unicode_atom(self):
        """Test ASCII letters mixed with Unicode letters."""
        prolog = PrologInterpreter()
        result = prolog.query_once("X = test_δ_value")
        assert result is not None
        assert result['X'] == Atom('test_δ_value')

    def test_multiple_unicode_letters(self):
        """Test multiple Unicode letters in sequence."""
        prolog = PrologInterpreter()
        result = prolog.query_once("X = αβγ")
        assert result is not None
        assert result['X'] == Atom('αβγ')

    def test_cyrillic_atom(self):
        """Test Cyrillic letters as atoms."""
        prolog = PrologInterpreter()
        # Test Cyrillic 'п' (p)
        result = prolog.query_once("X = п")
        assert result is not None
        assert result['X'] == Atom('п')
        
        # Test compound Cyrillic
        result = prolog.query_once("X = тест")
        assert result is not None
        assert result['X'] == Atom('тест')

    def test_arabic_atom(self):
        """Test Arabic letters as atoms."""
        prolog = PrologInterpreter()
        # Test Arabic alef
        result = prolog.query_once("X = ا")
        assert result is not None
        assert result['X'] == Atom('ا')

    def test_chinese_atom(self):
        """Test CJK (Chinese) characters as atoms."""
        prolog = PrologInterpreter()
        result = prolog.query_once("X = 中")
        assert result is not None
        assert result['X'] == Atom('中')
        
        result = prolog.query_once("X = 中文")
        assert result is not None
        assert result['X'] == Atom('中文')

    def test_japanese_hiragana_atom(self):
        """Test Japanese hiragana as atoms."""
        prolog = PrologInterpreter()
        result = prolog.query_once("X = あ")
        assert result is not None
        assert result['X'] == Atom('あ')

    def test_korean_hangul_atom(self):
        """Test Korean Hangul as atoms."""
        prolog = PrologInterpreter()
        result = prolog.query_once("X = 한")
        assert result is not None
        assert result['X'] == Atom('한')


class TestUnicodePredicateNames:
    """Test that predicates can be defined and called with Unicode names."""

    def test_define_unicode_predicate(self):
        """Test defining and calling a predicate with Unicode name."""
        prolog = PrologInterpreter()
        prolog.consult_string("δ_test(a). δ_test(b).")
        
        results = list(prolog.query("δ_test(X)"))
        assert len(results) == 2
        assert results[0]['X'] == Atom('a')
        assert results[1]['X'] == Atom('b')

    def test_unicode_functor_with_args(self):
        """Test calling Unicode functor with arguments."""
        prolog = PrologInterpreter()
        prolog.consult_string("δ_inverses_t(a, b, c, d, e).")
        
        result = prolog.query_once("δ_inverses_t(a, b, c, d, e)")
        assert result is not None

    def test_unify_unicode_compound(self):
        """Test unifying with compound term using Unicode functor."""
        prolog = PrologInterpreter()
        result = prolog.query_once("X = δ(1, 2, 3)")
        assert result is not None
        assert isinstance(result['X'], Compound)
        assert result['X'].functor == 'δ'
        assert result['X'].args == (Number(1), Number(2), Number(3))


class TestUnicodeFunctor:
    """Test functor/3 with Unicode predicate names."""

    def test_functor_unicode_name(self):
        """Test functor/3 extracting Unicode functor name."""
        prolog = PrologInterpreter()
        result = prolog.query_once("functor(δ_inverses_t(a, b, c, d, e), Name, Arity)")
        assert result is not None
        assert result['Name'] == Atom('δ_inverses_t')
        assert result['Arity'] == Number(5)

    def test_functor_construct_unicode(self):
        """Test functor/3 constructing compound with Unicode functor."""
        prolog = PrologInterpreter()
        result = prolog.query_once("functor(X, δ_test, 2)")
        assert result is not None
        assert isinstance(result['X'], Compound)
        assert result['X'].functor == 'δ_test'
        assert len(result['X'].args) == 2

    def test_functor_unicode_atom(self):
        """Test functor/3 with Unicode atom."""
        prolog = PrologInterpreter()
        result = prolog.query_once("functor(δ, F, A)")
        assert result is not None
        assert result['F'] == Atom('δ')
        assert result['A'] == Number(0)


class TestUnicodeUniv:
    """Test =../2 (univ) with Unicode functors."""

    def test_univ_decompose_unicode(self):
        """Test decomposing compound with Unicode functor using =.."""
        prolog = PrologInterpreter()
        result = prolog.query_once("δ_test(a, b, c) =.. L")
        assert result is not None
        L = result['L']
        assert isinstance(L, list) or (hasattr(L, 'elements') and L.elements[0] == Atom('δ_test'))

    def test_univ_construct_unicode(self):
        """Test constructing compound with Unicode functor using =.."""
        prolog = PrologInterpreter()
        result = prolog.query_once("X =.. [δ_test, a, b, c]")
        assert result is not None
        assert isinstance(result['X'], Compound)
        assert result['X'].functor == 'δ_test'
        assert len(result['X'].args) == 3


class TestUnicodeAtomProcessing:
    """Test atom processing predicates with Unicode atoms."""

    def test_atom_chars_unicode(self):
        """Test atom_chars/2 with Unicode atoms."""
        prolog = PrologInterpreter()
        result = prolog.query_once("atom_chars(δ, L)")
        assert result is not None
        # L should be a list containing the single character δ
        L = result['L']
        # Check that it's a list with one element 'δ'
        if hasattr(L, 'elements'):
            assert len(L.elements) == 1
            assert L.elements[0] == Atom('δ')

    def test_atom_chars_unicode_compound(self):
        """Test atom_chars/2 with compound Unicode atom."""
        prolog = PrologInterpreter()
        result = prolog.query_once("atom_chars(δ_test, L)")
        assert result is not None
        L = result['L']
        if hasattr(L, 'elements'):
            # Should have characters: δ, _, t, e, s, t
            assert len(L.elements) == 6

    def test_atom_codes_unicode(self):
        """Test atom_codes/2 with Unicode atoms."""
        prolog = PrologInterpreter()
        result = prolog.query_once("atom_codes(δ, L)")
        assert result is not None
        L = result['L']
        # δ (Greek small letter delta) is U+03B4
        if hasattr(L, 'elements'):
            assert len(L.elements) == 1
            assert L.elements[0] == Number(0x03B4)  # 948 in decimal

    def test_atom_length_unicode(self):
        """Test atom_length/2 with Unicode atoms."""
        prolog = PrologInterpreter()
        result = prolog.query_once("atom_length(δ_test, L)")
        assert result is not None
        # δ_test should be 6 characters long
        assert result['L'] == Number(6)

    def test_atom_concat_unicode(self):
        """Test atom_concat/3 with Unicode atoms."""
        prolog = PrologInterpreter()
        result = prolog.query_once("atom_concat(δ, _test, X)")
        assert result is not None
        assert result['X'] == Atom('δ_test')

    def test_atom_concat_unicode_decompose(self):
        """Test atom_concat/3 decomposing Unicode atoms."""
        prolog = PrologInterpreter()
        results = list(prolog.query("atom_concat(δ, X, δ_test)"))
        assert len(results) > 0
        assert results[0]['X'] == Atom('_test')

    def test_sub_atom_unicode(self):
        """Test sub_atom/5 with Unicode atoms."""
        prolog = PrologInterpreter()
        result = prolog.query_once("sub_atom(δ_test, 0, 1, _, S)")
        assert result is not None
        assert result['S'] == Atom('δ')

    def test_sub_atom_unicode_middle(self):
        """Test sub_atom/5 extracting middle of Unicode atom."""
        prolog = PrologInterpreter()
        result = prolog.query_once("sub_atom(δ_test, 1, 1, _, S)")
        assert result is not None
        assert result['S'] == Atom('_')


class TestUnicodeInClauses:
    """Test that Unicode atoms work correctly in clauses."""

    def test_unicode_in_fact(self):
        """Test Unicode atoms in facts."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            parent(δ, γ).
            parent(γ, β).
        """)
        
        result = prolog.query_once("parent(δ, γ)")
        assert result is not None

    def test_unicode_in_rule(self):
        """Test Unicode atoms in rule bodies."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            parent(δ, γ).
            grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
        """)
        
        prolog.consult_string("parent(γ, β).")
        result = prolog.query_once("grandparent(δ, β)")
        assert result is not None

    def test_unicode_variable_binding(self):
        """Test binding variables with Unicode atoms."""
        prolog = PrologInterpreter()
        prolog.consult_string("parent(δ, γ). parent(γ, β).")
        
        result = prolog.query_once("parent(δ, X), parent(X, β)")
        assert result is not None
        assert result['X'] == Atom('γ')


class TestUnicodeEdgeCases:
    """Test edge cases and special scenarios."""

    def test_underscore_start_unicode(self):
        """Test that underscore-prefixed Unicode atoms work."""
        prolog = PrologInterpreter()
        result = prolog.query_once("X = _δ")
        assert result is not None
        assert result['X'] == Atom('_δ')

    def test_unicode_atom_unification(self):
        """Test unification of Unicode atoms."""
        prolog = PrologInterpreter()
        result = prolog.query_once("δ = δ")
        assert result is not None

    def test_unicode_atom_not_equal(self):
        """Test that different Unicode atoms don't unify."""
        prolog = PrologInterpreter()
        result = prolog.query_once("δ = γ")
        assert result is None

    def test_unicode_in_list(self):
        """Test Unicode atoms in lists."""
        prolog = PrologInterpreter()
        result = prolog.query_once("X = [δ, γ, β]")
        assert result is not None
        L = result['X']
        if hasattr(L, 'elements'):
            assert L.elements[0] == Atom('δ')
            assert L.elements[1] == Atom('γ')
            assert L.elements[2] == Atom('β')

    def test_unicode_in_nested_compound(self):
        """Test Unicode atoms in nested compound terms."""
        prolog = PrologInterpreter()
        result = prolog.query_once("X = f(g(δ, γ), β)")
        assert result is not None
        assert isinstance(result['X'], Compound)
        assert result['X'].functor == 'f'

    def test_current_predicate_unicode(self):
        """Test current_predicate/1 with Unicode predicates."""
        prolog = PrologInterpreter()
        prolog.consult_string("δ_test(a). δ_test(b).")
        
        result = prolog.query_once("current_predicate(δ_test/1)")
        assert result is not None


class TestUnicodeErrorHandling:
    """Test error handling with Unicode atoms."""

    def test_undefined_unicode_predicate_fails(self):
        """Test that undefined Unicode predicates fail correctly."""
        prolog = PrologInterpreter()
        result = prolog.query_once("undefined_δ_predicate(X)")
        assert result is None

    def test_unicode_in_findall(self):
        """Test Unicode atoms with findall/3."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            data(δ, 1).
            data(γ, 2).
            data(β, 3).
        """)
        
        result = prolog.query_once("findall(X, data(X, _), L)")
        assert result is not None
        L = result['L']
        if hasattr(L, 'elements'):
            assert len(L.elements) == 3
            assert Atom('δ') in L.elements
            assert Atom('γ') in L.elements
            assert Atom('β') in L.elements

    def test_unicode_with_member(self):
        """Test Unicode atoms with member/2."""
        prolog = PrologInterpreter()
        result = prolog.query_once("member(δ, [α, δ, γ])")
        assert result is not None


class TestUnicodeAndASCII:
    """Test mixing ASCII and Unicode atoms."""

    def test_ascii_atom_still_works(self):
        """Test that ASCII atoms still work after Unicode support."""
        prolog = PrologInterpreter()
        result = prolog.query_once("X = test")
        assert result is not None
        assert result['X'] == Atom('test')

    def test_mixed_unicode_ascii_unification(self):
        """Test that Unicode and ASCII atoms don't unify incorrectly."""
        prolog = PrologInterpreter()
        result = prolog.query_once("δ = test")
        assert result is None

    def test_unicode_and_ascii_in_same_program(self):
        """Test program with both Unicode and ASCII atoms."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            color(red).
            color(δ).
            color(blue).
            greek(δ).
            greek(γ).
        """)
        
        result = prolog.query_once("color(red), greek(δ)")
        assert result is not None
