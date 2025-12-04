"""Tests for parser handling of comments in various contexts."""

import pytest
from vibeprolog.parser import PrologParser


class TestParserComments:
    """Tests for comment handling in Prolog parser."""

    def setup_method(self):
        self.parser = PrologParser()

    def test_comment_after_semicolon_in_disjunction(self):
        """Test block comment after semicolon in disjunction."""
        code = """
        test(X) :-
            (X = a ;/* comment */ true).
        """
        items = self.parser.parse(code)
        assert len(items) == 1
        clause = items[0]
        assert clause.is_rule()
        # The body should parse correctly with the comment ignored

    def test_comment_after_comma_in_conjunction(self):
        """Test block comment after comma in conjunction."""
        code = """
        test(X) :-
            X = a,/* comment */ X = b.
        """
        items = self.parser.parse(code)
        assert len(items) == 1
        clause = items[0]
        assert clause.is_rule()

    def test_comment_after_if_then(self):
        """Test block comment after -> in if-then."""
        code = """
        test(X) :-
            (X = a ->/* comment */ true).
        """
        items = self.parser.parse(code)
        assert len(items) == 1
        clause = items[0]
        assert clause.is_rule()

    def test_comment_after_if_then_else(self):
        """Test block comment after ; in if-then-else."""
        code = """
        test(X) :-
            (X = a -> true ;/* comment */ false).
        """
        items = self.parser.parse(code)
        assert len(items) == 1
        clause = items[0]
        assert clause.is_rule()

    def test_line_comment_after_operators(self):
        """Test line comments after operators."""
        code = """
        test(X) :-
            (X = a ; % comment
             X = b).
        """
        items = self.parser.parse(code)
        assert len(items) == 1
        clause = items[0]
        assert clause.is_rule()

    def test_nested_comments_in_disjunction(self):
        """Test nested comments in disjunction."""
        code = """
        test(X) :-
            (X = a ;/* outer /* inner */ comment */ true).
        """
        items = self.parser.parse(code)
        assert len(items) == 1
        clause = items[0]
        assert clause.is_rule()

    def test_comment_at_end_of_clause_body(self):
        """Test comment at end of clause body."""
        code = """
        test(X) :-
            X = a/* comment */.
        """
        items = self.parser.parse(code)
        assert len(items) == 1
        clause = items[0]
        assert clause.is_rule()

    def test_multiple_comments_in_body(self):
        """Test multiple comments in clause body."""
        code = """
        test(X) :-
            (X = a ;/* comment1 */ X = b,/* comment2 */ X = c).
        """
        items = self.parser.parse(code)
        assert len(items) == 1
        clause = items[0]
        assert clause.is_rule()

    def test_comment_with_special_chars(self):
        """Test comment with special characters."""
        code = """
        test(X) :-
            (X = a ;/* R2 = (=), Item == X2 */ true).
        """
        items = self.parser.parse(code)
        assert len(items) == 1
        clause = items[0]
        assert clause.is_rule()

    def test_pldoc_comments_ignored(self):
        """Test that PlDoc comments are handled correctly."""
        code = """
        /** This is a PlDoc comment */
        test(X) :-
            X = a.
        """
        items = self.parser.parse(code)
        assert len(items) == 1
        clause = items[0]
        assert clause.is_fact() or clause.is_rule()