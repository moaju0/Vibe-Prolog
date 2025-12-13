"""Tests for parsed module caching in the interpreter."""

import os

from vibeprolog import PrologInterpreter


def test_reuses_parsed_modules_across_imports(tmp_path):
    """Ensure a dependency is parsed only once even when imported multiple times."""

    prolog = PrologInterpreter()
    parse_calls = 0

    def hook():
        nonlocal parse_calls
        parse_calls += 1

    prolog._parser_invocation_hook = hook

    dep_path = tmp_path / "dep.pl"
    dep_path.write_text(":- module(dep, [dep/1]).\n:- dynamic dep/1.\ndep(x).\n")

    dep_path_str = dep_path.as_posix()

    mid_path = tmp_path / "mid.pl"
    mid_path.write_text(
        f":- module(mid, [mid/1]).\n"
        ":- dynamic mid/1.\n"
        f":- use_module('{dep_path_str}').\n"
        "mid(X) :- dep(X).\n"
    )

    mid_path_str = mid_path.as_posix()

    main_path = tmp_path / "main.pl"
    main_path.write_text(
        f":- module(main, []).\n"
        ":- dynamic main/0.\n"
        f":- use_module('{dep_path_str}').\n"
        f":- use_module('{mid_path_str}').\n"
        "main :- dep(x), mid(x).\n"
    )

    prolog.consult(main_path)
    first_parse_calls = parse_calls

    prolog.consult(main_path)

    assert parse_calls == first_parse_calls


def test_parsed_module_cache_invalidated_on_mtime_change(tmp_path):
    """The parsed-module cache should refresh when the source file changes."""

    prolog = PrologInterpreter()
    parse_calls = 0

    def hook():
        nonlocal parse_calls
        parse_calls += 1

    prolog._parser_invocation_hook = hook

    module_path = tmp_path / "cached.pl"
    module_path.write_text(":- module(cached, [p/0]).\n:- dynamic p/0.\np.\n")

    prolog.consult(module_path)
    first_parse_calls = parse_calls

    stat_before = module_path.stat()
    module_path.write_text(":- module(cached, [p/0]).\n:- dynamic p/0.\np.\np.\n")
    os.utime(module_path, (stat_before.st_atime, stat_before.st_mtime + 1))

    prolog.consult(module_path)

    assert parse_calls > first_parse_calls
