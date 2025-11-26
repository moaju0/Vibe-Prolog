"""Tests for PlDoc comment parsing and handling."""

import pytest

from vibeprolog import PrologInterpreter
from vibeprolog.exceptions import PrologThrow


class TestPlDoc:
    """Tests for PlDoc comment support."""

    def test_predicate_pldoc_line_comment(self):
        """Test %% line comments for predicates."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
%% This is a test predicate
test_predicate(a).
""")
        result = prolog.query_once("predicate_documentation(test_predicate/1, Doc)")
        assert result is not None
        assert result['Doc'] == " This is a test predicate"

    def test_predicate_pldoc_block_comment(self):
        """Test /** */ block comments for predicates."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
/** This is a test predicate with block comment */
test_predicate(b).
""")
        result = prolog.query_once("predicate_documentation(test_predicate/1, Doc)")
        assert result is not None
        assert result['Doc'] == " This is a test predicate with block comment "

    def test_predicate_pldoc_alt_block_comment(self):
        """Test /*! */ block comments for predicates."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
/*! This is a test predicate with alt block comment */
test_predicate(c).
""")
        result = prolog.query_once("predicate_documentation(test_predicate/1, Doc)")
        assert result is not None
        assert result['Doc'] == " This is a test predicate with alt block comment "

    def test_multiple_predicates_with_docs(self):
        """Test multiple predicates with different docs."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
%% First predicate
pred1.

/** Second predicate */
pred2(a).

/*! Third predicate */
pred3(a, b).
""")
        result1 = prolog.query_once("predicate_documentation(pred1/0, Doc)")
        assert result1 is not None
        assert result1['Doc'] == " First predicate"

        result2 = prolog.query_once("predicate_documentation(pred2/1, Doc)")
        assert result2 is not None
        assert result2['Doc'] == " Second predicate "

        result3 = prolog.query_once("predicate_documentation(pred3/2, Doc)")
        assert result3 is not None
        assert result3['Doc'] == " Third predicate "

    def test_predicate_without_doc(self):
        """Test predicate without documentation."""
        prolog = PrologInterpreter()
        prolog.consult_string("test_predicate(d).")
        result = prolog.query_once("predicate_documentation(test_predicate/1, Doc)")
        assert result is None

    def test_multiline_pldoc(self):
        """Test multiline PlDoc comments."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
/**
 * This is a multiline
 * PlDoc comment
 */
test_predicate(e).
""")
        result = prolog.query_once("predicate_documentation(test_predicate/1, Doc)")
        assert result is not None
        expected = "\n * This is a multiline\n * PlDoc comment\n "
        assert result['Doc'] == expected

    def test_pldoc_with_whitespace(self):
        """Test PlDoc comments with extra whitespace."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
%%   This has extra spaces
test_predicate(f).
""")
        result = prolog.query_once("predicate_documentation(test_predicate/1, Doc)")
        assert result is not None
        assert result['Doc'] == "   This has extra spaces"

    def test_malformed_pldoc_unterminated(self):
        """Test malformed PlDoc with unterminated block comment."""
        prolog = PrologInterpreter()
        with pytest.raises(PrologThrow):  # Should raise syntax error
            prolog.consult_string("""
/** Unterminated comment
test_predicate(g).
""")

    def test_regular_comments_ignored(self):
        """Test that regular /* */ comments are ignored for PlDoc."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
/* This is a regular comment */
test_predicate(h).
""")
        result = prolog.query_once("predicate_documentation(test_predicate/1, Doc)")
        assert result is None

    def test_pldoc_before_directive(self):
        """Test PlDoc before directives (should be ignored for now)."""
        prolog = PrologInterpreter()
        prolog.consult_string("""\n%% Module documentation\n:- dynamic(test_predicate/1).\ntest_predicate(a).\n""")
        # The doc should be associated with the directive, not the predicate.
        # Since directive docs are ignored for now, the predicate should have no doc.
        result = prolog.query_once("predicate_documentation(test_predicate/1, Doc)")
        assert result is None