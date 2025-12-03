import pytest

from vibeprolog import PrologInterpreter
from vibeprolog.parser import extract_op_directives


@pytest.mark.slow
def test_clpz_operator_directives_loaded_before_parsing():
    prolog = PrologInterpreter()
    code = """
    :- use_module(library(clpz)).
    clpz_goal(X, Y) :- X in 0..1, Y #= X.
    """

    local_ops = extract_op_directives(code)
    imported_ops = prolog._collect_imported_operators(code, "string:1", local_ops)
    directive_ops = imported_ops + local_ops

    operator_names = {(spec, name) for _, spec, name in directive_ops}
    assert ("xfx", "#=") in operator_names
    assert any(name == ".." for _, name in operator_names)

    prolog.parser.parse(code, "consult/1", directive_ops=directive_ops)


def test_nested_imported_operator_directives_are_applied():
    prolog = PrologInterpreter()
    prolog.consult("tests/fixtures/operator_consumer.pl")

    operator_info = prolog.operator_table.lookup("<<<", "xfx")
    assert operator_info is not None

    consumer_module = prolog.modules.get("operator_consumer")
    assert consumer_module is not None
    assert ("uses_op", 2) in consumer_module.predicates


def test_circular_imports_do_not_cause_infinite_loop():
    """Test that circular imports are handled gracefully without infinite recursion."""
    prolog = PrologInterpreter()

    # This should not hang or raise a RecursionError
    prolog.consult("tests/fixtures/circular_a.pl")

    # Verify both modules loaded successfully
    assert "circular_a" in prolog.modules
    assert "circular_b" in prolog.modules

    # Verify operators from both modules are available
    op_a = prolog.operator_table.lookup("<<<", "xfx")
    op_b = prolog.operator_table.lookup(">>>", "xfy")
    assert op_a is not None, "Operator <<< from circular_a should be loaded"
    assert op_b is not None, "Operator >>> from circular_b should be loaded"

    # Verify predicates are accessible
    module_a = prolog.modules.get("circular_a")
    module_b = prolog.modules.get("circular_b")
    assert module_a is not None
    assert module_b is not None
    assert ("a_marker", 0) in module_a.predicates
    assert ("b_marker", 0) in module_b.predicates
