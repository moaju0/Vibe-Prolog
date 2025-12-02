"""Shared default operator definitions for Prolog parsing and semantics."""

from __future__ import annotations

DEFAULT_OPERATORS: list[tuple[int, str, str]] = [
    (1200, "xfx", ":-"),
    (1100, "xfy", ";"),
    (1050, "xfy", "->"),
    (1000, "xfy", ","),
    (900, "fy", "\\+"),
    (700, "xfx", "=.."),
    (700, "xfx", "is"),
    (700, "xfx", "="),
    (700, "xfx", "\\="),
    (700, "xfx", "=:="),
    (700, "xfx", "=\\="),
    (700, "xfx", "<"),
    (700, "xfx", ">"),
    (700, "xfx", "=<"),
    (700, "xfx", ">="),
    (700, "xfx", "=="),
    (700, "xfx", "\\=="),
    (700, "xfx", "@<"),
    (700, "xfx", "@=<"),
    (700, "xfx", "@>"),
    (700, "xfx", "@>="),
    (600, "xfy", ":"),
    (500, "yfx", "+"),
    (500, "yfx", "-"),
    (400, "yfx", "*"),
    (400, "yfx", "/"),
    (400, "yfx", "//"),
    (400, "yfx", "mod"),
    (400, "yfx", "div"),
    (200, "xfy", "**"),
    (200, "fy", "+"),
    (200, "fy", "-"),
]

__all__ = ["DEFAULT_OPERATORS"]
