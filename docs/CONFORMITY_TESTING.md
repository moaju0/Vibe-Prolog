# ISO Prolog Conformity Testing Results

> **Last Updated**: 2025-12-08 20:54:28
> **Test Suite**: ISO/IEC JTC1 SC22 WG17
> **Source**: https://www.complang.tuwien.ac.at/ulrich/iso-prolog/conformity_testing
> **Total Tests**: 40

## Summary

- **Conforming (PASS)**: 23 (57.5%)
- **Non-conforming (FAIL)**: 17 (42.5%)

### Result Distribution

- **OK Results**: 29 (72.5%)
- **Syntax Errors**: 10 (25.0%)
- **Type Errors**: 0 (0.0%)
- **Other Errors**: 1 (2.5%)

## Results by Category

### Character Escapes (Tests 1-37)

- Conforming: 20/37 (54.1%)

### Operators (Tests 38-40)

- Conforming: 3/3 (100.0%)

## Detailed Results

| Test # | Query | Expected | Actual | Status | SWI | Scryer | Error |
|--------|-------|----------|--------|--------|-----|--------|-------|
| 1 | `writeq('\n').` | OK | OK | PASS | OK | OK |  |
| 2 | `'` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | WAITS | OK | error(syntax_error(Unterminated quote... |
| 3 | `)` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(No terminal matche... |
| 4 | `)'` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Unterminated quote... |
| 5 | `.` | SYNTAX_ERROR | EXCEPTION | FAIL | OK | OK | Failed to parse query: . |
| 6 | `writeq(' ').` | OK | OK | PASS | OK | OK |  |
| 7 | `0'\t=0' .` | SYNTAX_ERROR | OK | FAIL | OK | OK |  |
| 8 | `writeq('\n').` | OK | OK | PASS | OK | OK |  |
| 9 | `writeq('\\n').` | OK | OK | PASS | OK | OK |  |
| 10 | `writeq('\\na').` | OK | OK | PASS | OK | OK |  |
| 11 | `writeq('a\\nb').` | SYNTAX_ERROR | OK | FAIL | OK | OK |  |
| 12 | `writeq('a\\n b').` | OK | OK | PASS | OK | OK |  |
| 13 | `writeq('\\ ').` | SYNTAX_ERROR | OK | FAIL | OK | OK |  |
| 14 | `writeq('\\\n').` | OK | OK | PASS | OK | OK |  |
| 15 | `writeq('\\t').` | OK | OK | PASS | OK | OK |  |
| 16 | `writeq('\t').` | SYNTAX_ERROR | OK | FAIL | OK | OK |  |
| 17 | `writeq('\a').` | SYNTAX_ERROR | OK | FAIL | OK | OK |  |
| 18 | `writeq('\7\').` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated quote... |
| 19 | `writeq('\ca').` | OK | OK | PASS | OK | OK |  |
| 20 | `writeq('\d').` | OK | OK | PASS | OK | OK |  |
| 21 | `writeq('\e').` | SYNTAX_ERROR | OK | FAIL | OK | OK |  |
| 22 | `writeq('\033\').` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Unterminated quote... |
| 23 | `writeq('\0\').` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Unterminated quote... |
| 24 | `char_code('\e',C).` | SYNTAX_ERROR | OK | FAIL | WAITS | OK |  |
| 25 | `char_code('\d',C).` | SYNTAX_ERROR | OK | FAIL | OK | OK |  |
| 26 | `writeq('\u1').` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | WAITS | OK | error(syntax_error(Unterminated quote... |
| 27 | `writeq('\u0021').` | OK | OK | PASS | OK | OK |  |
| 28 | `put_code(0'\u0021).` | OK | OK | PASS | OK | OK |  |
| 29 | `writeq("\u0021").` | SYNTAX_ERROR | OK | FAIL | OK | OK |  |
| 30 | `writeq('\x21\').` | SYNTAX_ERROR | OK | FAIL | OK | OK |  |
| 31 | `writeq('\x0021\').` | OK | OK | PASS | OK | OK |  |
| 32 | `X = 0'\u1.` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(No terminal matche... |
| 33 | `writeq('\n').` | SYNTAX_ERROR | OK | FAIL | OK | OK |  |
| 34 | `writeq(.).` | SYNTAX_ERROR | OK | FAIL | OK | OK |  |
| 35 | `'\n''.` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated quote... |
| 36 | `X = 0'\. .` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(No terminal matche... |
| 37 | `writeq((-)-(-)).` | OK | OK | PASS | OK | OK |  |
| 38 | `writeq(((:-):-(:-))).` | OK | OK | PASS | OK | OK |  |
| 39 | `writeq((\\*)=(\\*)).` | OK | OK | PASS | OK | OK |  |
| 40 | `writeq([:-,-]).` | OK | OK | PASS | OK | OK |  |

## Known Issues

[Automatically populated list of failing tests with their error messages]

## Regenerating This Report

```bash
uv run python tools/conformity_test.py --output docs/CONFORMITY_TESTING.md
```
