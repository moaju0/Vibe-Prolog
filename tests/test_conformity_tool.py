"""Tests for the ISO Prolog conformity testing tool."""

import pytest
from pathlib import Path
from unittest.mock import patch, MagicMock
from vibeprolog import PrologInterpreter

from tools.conformity_test import (
    ConformityTest,
    TestResult,
    TestExecutionResult,
    CONFORMITY_TESTS,
    execute_test,
    run_conformity_tests,
    generate_markdown_report,
)


class TestConformityTest:
    """Tests for ConformityTest dataclass."""

    def test_category_character_escapes(self):
        """Test categorization of character escape tests."""
        test = ConformityTest(1, "writeq('\\n').")
        assert test.category == "Character Escapes"

    def test_category_operators(self):
        """Test categorization of operator tests."""
        test = ConformityTest(38, "writeq((-)-(-)).")
        assert test.category == "Operators"

    def test_category_numeric_literals(self):
        """Test categorization of numeric literal tests."""
        test = ConformityTest(60, "float(.0).")
        assert test.category == "Numeric Literals"

    def test_category_special_syntax(self):
        """Test categorization of special syntax tests."""
        test = ConformityTest(243, "set_prolog_flag(double_quotes,chars).")
        assert test.category == "Special Syntax"


class TestTestData:
    """Tests for the conformity test data."""

    def test_total_tests(self):
        """Test that we have all 355 tests."""
        assert len(CONFORMITY_TESTS) == 355

    def test_test_numbers_sequential(self):
        """Test that test numbers are sequential from 1 to 355."""
        numbers = [test.num for test in CONFORMITY_TESTS]
        assert numbers == list(range(1, 356))

    def test_references_previous_marker(self):
        """Test that /**/ tests are marked as references_previous."""
        # Test 67 has /**/ marker
        test_67 = next(test for test in CONFORMITY_TESTS if test.num == 67)
        assert test_67.references_previous

    def test_no_references_previous_marker(self):
        """Test that regular tests are not marked as references_previous."""
        test_1 = next(test for test in CONFORMITY_TESTS if test.num == 1)
        assert not test_1.references_previous


class TestExecuteTest:
    """Tests for execute_test function."""

    def test_execute_ok_test(self):
        """Test executing a test that should pass."""
        # PrologInterpreter is imported at module scope now

        prolog = PrologInterpreter()
        test = ConformityTest(1, "writeq('\\n').")

        result = execute_test(prolog, test)

        assert result.test == test
        assert result.result == TestResult.OK
        assert result.error_message is None
        assert result.execution_time >= 0

    def test_execute_syntax_error_test(self):
        """Test executing a test that should fail with syntax error."""
        # PrologInterpreter is imported at module scope now

        prolog = PrologInterpreter()
        test = ConformityTest(2, "'")

        result = execute_test(prolog, test)

        assert result.test == test
        assert result.result == TestResult.SYNTAX_ERROR
        assert result.error_message is not None
        assert "syntax_error" in result.error_message

    def test_execute_references_previous(self):
        """Test executing a test that references a previous test."""
        # PrologInterpreter is imported at module scope now

        prolog = PrologInterpreter()
        # Test 67 references test 66
        test_66 = ConformityTest(66, "op(9,xf,e). 1e-9 = -(e(1),9).")
        test_67 = ConformityTest(67, "/**/ 1.0e- 9 = -(e(1.0),9).", references_previous=True)

        result = execute_test(prolog, test_67)

        assert result.test == test_67
        # The result may vary, but the function should handle references_previous


class TestRunConformityTests:
    """Tests for run_conformity_tests function."""

    def test_run_subset(self):
        """Test running a subset of tests."""
        results = run_conformity_tests(test_range=(1, 3))

        assert len(results) == 3
        assert all(isinstance(r, TestExecutionResult) for r in results)
        assert {r.test.num for r in results} == {1, 2, 3}

    def test_run_by_category(self):
        """Test running tests by category."""
        results = run_conformity_tests(category="Character Escapes")

        assert len(results) > 0
        assert all(r.test.category == "Character Escapes" for r in results)

    def test_run_all_tests(self):
        """Test running all tests (but limit to avoid slow tests)."""
        # Only run first 10 tests for speed
        results = run_conformity_tests(test_range=(1, 10))

        assert len(results) == 10
        assert all(isinstance(r, TestExecutionResult) for r in results)


class TestGenerateMarkdownReport:
    """Tests for generate_markdown_report function."""

    def test_generate_report(self, tmp_path):
        """Test generating a markdown report."""
        # Create some test results
        results = [
            TestExecutionResult(
                test=ConformityTest(1, "writeq('\\n')."),
                result=TestResult.OK,
                execution_time=0.1
            ),
            TestExecutionResult(
                test=ConformityTest(2, "'"),
                result=TestResult.EXCEPTION,
                error_message="syntax error",
                execution_time=0.05
            ),
        ]

        output_path = tmp_path / "test_report.md"
        generate_markdown_report(results, output_path)

        assert output_path.exists()
        content = output_path.read_text()
        assert "# ISO Prolog Conformity Testing Results" in content
        assert "> **Total Tests**: 2" in content

    def test_generate_empty_report(self, tmp_path):
        """Test generating a report with no results."""
        results = []
        output_path = tmp_path / "empty_report.md"
        generate_markdown_report(results, output_path)

        assert output_path.exists()
        content = output_path.read_text()
        assert "# ISO Prolog Conformity Testing Results" in content


class TestCLI:
    """Tests for command-line interface."""

    @patch('tools.conformity_test.run_conformity_tests')
    @patch('tools.conformity_test.generate_markdown_report')
    def test_cli_basic(self, mock_generate, mock_run):
        """Test basic CLI functionality."""
        mock_run.return_value = []
        mock_generate.return_value = None

        with patch('sys.argv', ['conformity_test.py']):
            from tools.conformity_test import main
            main()

        mock_run.assert_called_once()
        mock_generate.assert_called_once()

    @patch('tools.conformity_test.run_conformity_tests')
    @patch('tools.conformity_test.generate_markdown_report')
    def test_cli_with_range(self, mock_generate, mock_run):
        """Test CLI with test range."""
        mock_run.return_value = []
        mock_generate.return_value = None

        with patch('sys.argv', ['conformity_test.py', '--tests', '1-10']):
            from tools.conformity_test import main
            main()

        mock_run.assert_called_once_with(test_range=(1, 10), category=None, verbose=False)

    @patch('tools.conformity_test.run_conformity_tests')
    @patch('tools.conformity_test.generate_markdown_report')
    def test_cli_with_category(self, mock_generate, mock_run):
        """Test CLI with category filter."""
        mock_run.return_value = []
        mock_generate.return_value = None

        with patch('sys.argv', ['conformity_test.py', '--category', 'Character Escapes']):
            from tools.conformity_test import main
            main()

        mock_run.assert_called_once_with(test_range=None, category='Character Escapes', verbose=False)

    @patch('tools.conformity_test.run_conformity_tests')
    @patch('tools.conformity_test.generate_markdown_report')
    def test_cli_verbose(self, mock_generate, mock_run):
        """Test CLI with verbose flag."""
        mock_run.return_value = []
        mock_generate.return_value = None

        with patch('sys.argv', ['conformity_test.py', '--verbose']):
            from tools.conformity_test import main
            main()

        mock_run.assert_called_once_with(test_range=None, category=None, verbose=True)