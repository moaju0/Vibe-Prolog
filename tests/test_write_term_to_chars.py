"""Tests for write_term_to_chars/3 and read_from_chars/2 predicates."""

from prolog import PrologInterpreter


class TestWriteTermToChars:
    """Tests for write_term_to_chars/3 predicate."""

    def test_atom_unquoted(self):
        """Test writing an atom without quotes."""
        prolog = PrologInterpreter()
        result = prolog.query_once("write_term_to_chars(hello, [quoted(false)], Chars).")
        assert result is not None
        assert result['Chars'] == ['h', 'e', 'l', 'l', 'o']

    def test_atom_needs_quoting(self):
        """Test writing an atom that needs quoting."""
        prolog = PrologInterpreter()
        result = prolog.query_once("write_term_to_chars('Hello World', [quoted(true)], Chars).")
        assert result is not None
        # Should be quoted because it starts with uppercase and has space
        chars = result['Chars']
        assert chars[0] == "'"
        assert chars[-1] == "'"

    def test_number_integer(self):
        """Test writing an integer."""
        prolog = PrologInterpreter()
        result = prolog.query_once("write_term_to_chars(42, [quoted(false)], Chars).")
        assert result is not None
        assert result['Chars'] == ['4', '2']

    def test_number_negative(self):
        """Test writing a negative number."""
        prolog = PrologInterpreter()
        result = prolog.query_once("write_term_to_chars(-42, [quoted(false)], Chars).")
        assert result is not None
        assert result['Chars'] == ['-', '4', '2']

    def test_number_float(self):
        """Test writing a float."""
        prolog = PrologInterpreter()
        result = prolog.query_once("write_term_to_chars(3.14, [quoted(false)], Chars).")
        assert result is not None
        chars_str = ''.join(result['Chars'])
        assert '3.14' in chars_str

    def test_variable(self):
        """Test writing an unbound variable."""
        prolog = PrologInterpreter()
        result = prolog.query_once("write_term_to_chars(X, [quoted(false)], Chars).")
        assert result is not None
        # Variables are written as _VarName
        chars_str = ''.join(result['Chars'])
        assert chars_str.startswith('_')

    def test_empty_list(self):
        """Test writing an empty list."""
        prolog = PrologInterpreter()
        result = prolog.query_once("write_term_to_chars([], [quoted(false)], Chars).")
        assert result is not None
        assert result['Chars'] == ['[', ']']

    def test_simple_list(self):
        """Test writing a simple list."""
        prolog = PrologInterpreter()
        result = prolog.query_once("write_term_to_chars([1,2,3], [quoted(false)], Chars).")
        assert result is not None
        assert result['Chars'] == ['[', '1', ',', '2', ',', '3', ']']

    def test_nested_list(self):
        """Test writing a nested list."""
        prolog = PrologInterpreter()
        result = prolog.query_once("write_term_to_chars([1,[2,3]], [quoted(false)], Chars).")
        assert result is not None
        chars_str = ''.join(result['Chars'])
        assert chars_str == '[1,[2,3]]'

    def test_list_with_tail(self):
        """Test writing a list with explicit tail."""
        prolog = PrologInterpreter()
        result = prolog.query_once("write_term_to_chars([1,2|X], [quoted(false)], Chars).")
        assert result is not None
        chars_str = ''.join(result['Chars'])
        assert '[1,2|' in chars_str

    def test_compound_simple(self):
        """Test writing a simple compound term."""
        prolog = PrologInterpreter()
        result = prolog.query_once("write_term_to_chars(foo(bar), [quoted(false)], Chars).")
        assert result is not None
        assert result['Chars'] == ['f', 'o', 'o', '(', 'b', 'a', 'r', ')']

    def test_compound_multiple_args(self):
        """Test writing a compound term with multiple arguments."""
        prolog = PrologInterpreter()
        result = prolog.query_once("write_term_to_chars(f(a,b,c), [quoted(false)], Chars).")
        assert result is not None
        chars_str = ''.join(result['Chars'])
        assert chars_str == 'f(a,b,c)'

    def test_compound_nested(self):
        """Test writing nested compound terms."""
        prolog = PrologInterpreter()
        result = prolog.query_once("write_term_to_chars(f(g(h)), [quoted(false)], Chars).")
        assert result is not None
        chars_str = ''.join(result['Chars'])
        assert chars_str == 'f(g(h))'

    def test_compound_ignore_ops_true(self):
        """Test writing compound term with ignore_ops(true) - canonical form."""
        prolog = PrologInterpreter()
        result = prolog.query_once("write_term_to_chars(f(a,b), [ignore_ops(true)], Chars).")
        assert result is not None
        chars_str = ''.join(result['Chars'])
        assert chars_str == 'f(a,b)'

    def test_compound_ignore_ops_false(self):
        """Non-operator compound terms should stay in canonical form regardless of ignore_ops."""
        prolog = PrologInterpreter()
        result = prolog.query_once("write_term_to_chars(f(a,b), [ignore_ops(false)], Chars).")
        assert result is not None
        chars_str = ''.join(result['Chars'])
        # Both should produce same result (canonical form) until operator handling is implemented
        assert chars_str == 'f(a,b)'

    def test_compound_ignore_ops_both_identical(self):
        """Test that ignore_ops(true) and ignore_ops(false) produce identical output (verifies refactoring)."""
        prolog = PrologInterpreter()
        result_true = prolog.query_once("write_term_to_chars(foo(bar,baz), [ignore_ops(true)], Chars1).")
        result_false = prolog.query_once("write_term_to_chars(foo(bar,baz), [ignore_ops(false)], Chars2).")

        assert result_true is not None
        assert result_false is not None
        assert result_true['Chars1'] == result_false['Chars2']

    def test_arithmetic_ignore_ops_true(self):
        """ignore_ops(true) should emit canonical operator form."""
        prolog = PrologInterpreter()
        result = prolog.query_once(
            "write_term_to_chars(1+2, [ignore_ops(true)], Chars)."
        )
        assert result is not None
        chars_str = ''.join(result['Chars'])
        assert chars_str == '+(1,2)'

    def test_arithmetic_ignore_ops_false(self):
        """ignore_ops(false) should emit infix operator form respecting precedence."""
        prolog = PrologInterpreter()
        result = prolog.query_once(
            "write_term_to_chars(1+2, [ignore_ops(false)], Chars)."
        )
        assert result is not None
        chars_str = ''.join(result['Chars'])
        assert chars_str == '1+2'

    def test_arithmetic_parentheses(self):
        """Ensure parentheses are emitted when needed for precedence."""
        prolog = PrologInterpreter()
        result = prolog.query_once(
            "write_term_to_chars((1+2)*3, [ignore_ops(false)], Chars)."
        )
        assert result is not None
        chars_str = ''.join(result['Chars'])
        assert chars_str == '(1+2)*3'

    def test_is_operator_spacing(self):
        """Alphabetic operators like is should include spaces for readability/lexing."""
        prolog = PrologInterpreter()
        result = prolog.query_once(
            "write_term_to_chars(X is 1+2, [ignore_ops(false)], Chars)."
        )
        assert result is not None
        chars_str = ''.join(result['Chars'])
        assert ' is ' in chars_str
        assert chars_str.endswith('1+2')

    def test_numbervars_simple(self):
        """Test numbervars option with $VAR(0)."""
        prolog = PrologInterpreter()
        # Use functor/3 to construct $VAR(0) to avoid parser issues
        prolog.consult_string("test_var(V) :- functor(V, '$VAR', 1), arg(1, V, 0).")
        result = prolog.query_once("test_var(V), write_term_to_chars(V, [numbervars(true)], Chars).")
        assert result is not None
        assert result['Chars'] == ['A']

    def test_numbervars_sequence(self):
        """Test numbervars option with sequence A, B, C..."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            make_var(N, V) :- functor(V, '$VAR', 1), arg(1, V, N).
        """)

        # Test $VAR(0) = A
        result = prolog.query_once("make_var(0, V), write_term_to_chars(V, [numbervars(true)], Chars).")
        assert result is not None
        assert result['Chars'] == ['A']

        # Test $VAR(1) = B
        result = prolog.query_once("make_var(1, V), write_term_to_chars(V, [numbervars(true)], Chars).")
        assert result is not None
        assert result['Chars'] == ['B']

        # Test $VAR(25) = Z
        result = prolog.query_once("make_var(25, V), write_term_to_chars(V, [numbervars(true)], Chars).")
        assert result is not None
        assert result['Chars'] == ['Z']

    def test_numbervars_wraparound(self):
        """Test numbervars option with $VAR(26) = A1."""
        prolog = PrologInterpreter()
        prolog.consult_string("make_var(N, V) :- functor(V, '$VAR', 1), arg(1, V, N).")
        result = prolog.query_once("make_var(26, V), write_term_to_chars(V, [numbervars(true)], Chars).")
        assert result is not None
        chars_str = ''.join(result['Chars'])
        assert chars_str == 'A1'

    def test_numbervars_false(self):
        """Test numbervars(false) doesn't convert $VAR terms."""
        prolog = PrologInterpreter()
        prolog.consult_string("make_var(N, V) :- functor(V, '$VAR', 1), arg(1, V, N).")
        result = prolog.query_once("make_var(0, V), write_term_to_chars(V, [numbervars(false)], Chars).")
        assert result is not None
        chars_str = ''.join(result['Chars'])
        # Should write as compound term $VAR(0), not as A
        assert '$VAR' in chars_str

    def test_quoted_false_simple_atom(self):
        """Test quoted(false) with simple atom."""
        prolog = PrologInterpreter()
        result = prolog.query_once("write_term_to_chars(abc, [quoted(false)], Chars).")
        assert result is not None
        chars_str = ''.join(result['Chars'])
        assert chars_str == 'abc'
        assert "'" not in chars_str

    def test_quoted_true_special_atom(self):
        """Test quoted(true) with atom that needs quoting."""
        prolog = PrologInterpreter()
        result = prolog.query_once("write_term_to_chars('Atom', [quoted(true)], Chars).")
        assert result is not None
        chars_str = ''.join(result['Chars'])
        # Should be quoted because it starts with uppercase
        assert chars_str.startswith("'")
        assert chars_str.endswith("'")

    def test_multiple_options(self):
        """Test combining multiple options."""
        prolog = PrologInterpreter()
        result = prolog.query_once(
            "write_term_to_chars(foo(bar), [quoted(false), ignore_ops(true)], Chars)."
        )
        assert result is not None
        chars_str = ''.join(result['Chars'])
        assert chars_str == 'foo(bar)'

    def test_empty_options(self):
        """Test with empty options list."""
        prolog = PrologInterpreter()
        result = prolog.query_once("write_term_to_chars(hello, [], Chars).")
        assert result is not None
        assert result['Chars'] == ['h', 'e', 'l', 'l', 'o']

    def test_complex_structure(self):
        """Test writing a complex nested structure."""
        prolog = PrologInterpreter()
        result = prolog.query_once(
            "write_term_to_chars(person(john, age(30), [child1, child2]), [quoted(false)], Chars)."
        )
        assert result is not None
        chars_str = ''.join(result['Chars'])
        assert 'person' in chars_str
        assert 'john' in chars_str
        assert 'age' in chars_str
        assert '30' in chars_str
        assert 'child1' in chars_str


class TestReadFromChars:
    """Tests for read_from_chars/2 predicate."""

    def test_read_atom(self):
        """Test reading an atom from character list."""
        prolog = PrologInterpreter()
        result = prolog.query_once("read_from_chars([h,e,l,l,o], Term).")
        assert result is not None
        # Term should be the atom 'hello'
        term_str = str(result['Term'])
        assert 'hello' in term_str

    def test_read_number(self):
        """Test reading a number from character list."""
        prolog = PrologInterpreter()
        result = prolog.query_once("read_from_chars(['4','2'], Term).")
        assert result is not None
        # Should parse as number 42
        assert result['Term'] == 42

    def test_read_from_string(self):
        """Test reading from an atom (string) instead of char list."""
        prolog = PrologInterpreter()
        result = prolog.query_once("read_from_chars('hello', Term).")
        assert result is not None
        term_str = str(result['Term'])
        assert 'hello' in term_str

    def test_read_compound(self):
        """Test reading a compound term."""
        prolog = PrologInterpreter()
        result = prolog.query_once("read_from_chars('foo(bar)', Term).")
        assert result is not None
        # Should be compound term foo(bar) - converted to dict by interpreter
        term = result['Term']
        assert isinstance(term, dict)
        assert 'foo' in term
        assert term['foo'] == ['bar']

    def test_read_list(self):
        """Test reading a list."""
        prolog = PrologInterpreter()
        result = prolog.query_once("read_from_chars('[1,2,3]', Term).")
        assert result is not None
        # Should be a list - converted to Python list by interpreter
        assert isinstance(result['Term'], list)
        assert result['Term'] == [1, 2, 3]

    def test_roundtrip_atom(self):
        """Test write then read roundtrip for atom."""
        prolog = PrologInterpreter()
        result = prolog.query_once(
            "write_term_to_chars(hello, [quoted(false)], Chars), "
            "read_from_chars(Chars, Term)."
        )
        assert result is not None
        term_str = str(result['Term'])
        assert 'hello' in term_str

    def test_roundtrip_compound(self):
        """Test write then read roundtrip for compound term."""
        prolog = PrologInterpreter()
        result = prolog.query_once(
            "write_term_to_chars(foo(bar,baz), [quoted(false)], Chars), "
            "read_from_chars(Chars, Term)."
        )
        assert result is not None
        term = result['Term']
        # Compound terms are converted to dicts by interpreter
        assert isinstance(term, dict)
        assert 'foo' in term
        assert term['foo'] == ['bar', 'baz']

    def test_roundtrip_list(self):
        """Test write then read roundtrip for list."""
        prolog = PrologInterpreter()
        result = prolog.query_once(
            "write_term_to_chars([1,2,3], [quoted(false)], Chars), "
            "read_from_chars(Chars, Term)."
        )
        assert result is not None
        # Lists are converted to Python lists by interpreter
        assert isinstance(result['Term'], list)
        assert result['Term'] == [1, 2, 3]

    def test_read_with_trailing_period(self):
        """Test reading term with trailing period."""
        prolog = PrologInterpreter()
        result = prolog.query_once("read_from_chars('hello.', Term).")
        assert result is not None
        term_str = str(result['Term'])
        assert 'hello' in term_str

    def test_read_invalid_syntax_raises(self):
        """Test reading invalid syntax surfaces a syntax_error that catch/3 can intercept."""
        prolog = PrologInterpreter()
        result = prolog.query_once(
            "catch(read_from_chars('foo(', Term), Error, true)."
        )
        assert result is not None
        error_term = result['Error']
        assert isinstance(error_term, dict)
        assert 'error' in error_term
        syntax_descriptor = error_term['error'][0]
        if isinstance(syntax_descriptor, dict):
            assert 'syntax_error' in syntax_descriptor
        else:
            assert syntax_descriptor == 'syntax_error'
        assert error_term['error'][1] == 'read_from_chars/2'


class TestEdgeCases:
    """Test edge cases and error conditions."""

    def test_write_invalid_options(self):
        """Test write_term_to_chars with invalid options."""
        prolog = PrologInterpreter()
        # Non-list options should fail
        result = prolog.query_once("write_term_to_chars(foo, invalid, Chars).")
        assert result is None

    def test_compound_no_args(self):
        """Test compound term with zero arguments (atom-like)."""
        prolog = PrologInterpreter()
        result = prolog.query_once("write_term_to_chars(foo, [quoted(false)], Chars).")
        assert result is not None
        assert result['Chars'] == ['f', 'o', 'o']

    def test_special_characters_in_atom(self):
        """Test atom with special characters that need quoting."""
        prolog = PrologInterpreter()
        result = prolog.query_once("write_term_to_chars('hello world', [quoted(true)], Chars).")
        assert result is not None
        chars_str = ''.join(result['Chars'])
        # Should be quoted
        assert chars_str.startswith("'")

    def test_escape_sequences(self):
        """Test atoms with escape sequences."""
        prolog = PrologInterpreter()
        result = prolog.query_once("write_term_to_chars('it\\'s', [quoted(true)], Chars).")
        assert result is not None
        chars_str = ''.join(result['Chars'])
        # Should contain escaped quote
        assert '\\' in chars_str
