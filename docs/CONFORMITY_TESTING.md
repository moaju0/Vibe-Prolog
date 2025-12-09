# ISO Prolog Conformity Testing Results

> **Last Updated**: 2025-12-08 12:29:53
> **Test Suite**: ISO/IEC JTC1 SC22 WG17
> **Source**: https://www.complang.tuwien.ac.at/ulrich/iso-prolog/conformity_testing
> **Total Tests**: 111

## Summary

- **Conforming (PASS)**: 12 (10.8%)
- **Non-conforming (FAIL)**: 99 (89.2%)

### Result Distribution

- **OK Results**: 0 (0.0%)
- **Syntax Errors**: 111 (100.0%)
- **Type Errors**: 0 (0.0%)
- **Other Errors**: 0 (0.0%)

## Results by Category

### Character Escapes (Tests 150-150)

- Conforming: 0/1 (0.0%)

### Numeric Literals (Tests 60-73)

- Conforming: 1/14 (7.1%)

### Operators (Tests 40-142)

- Conforming: 7/66 (10.6%)

### Special Syntax (Tests 74-149)

- Conforming: 4/30 (13.3%)

## Detailed Results

| Test # | Query | Expected | Actual | Status | SWI | Scryer | Error |
|--------|-------|----------|--------|--------|-----|--------|-------|
| 40 | `writeq([:-,-]).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 41 | `writeq(f(*)).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 42 | `writeq(a*(b+c)).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 43 | `writeq(f(;,'&#124;',';;')).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 44 | `writeq([.,.(.,.,.)]).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 45 | `writeq((a :- b,c)).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 46 | `write_canonical([a]).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 47 | `writeq('/*').` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 48 | `writeq(//*).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 49 | `writeq(//*./*/).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 50 | `writeq('/**').` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 51 | `writeq('*/').` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 52 | `"'\\`\"" = "'`"".` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 53 | `"'\\"" = "'"".` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 54 | `\\` = `'.` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 55 | `'\\`\"' = ''`'.` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 56 | `writeq(''\\`\"\"').` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 57 | `('\\\\') = (\\).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 58 | `op(1,xf,xf1). 1xf1 = xf1(1).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 59 | `X = 0X1.` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 60 | `float(.0).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 61 | `op(100,xfx,'.'). functor(3 .2,F,A).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 62 | `float(- .0).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 63 | `float(1E9).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 64 | `integer(1e).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | SYNTAX_ERROR | OK | error(syntax_error(Expecting a value,... |
| 65 | `op(9,xf,e9). 1e9 = e9(1).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 66 | `op(9,xf,e). 1e-9 = -(e(1),9).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 67 | `/**/ 1.0e- 9 = -(e(1.0),9).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 68 | `/**/ writeq(1e).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 69 | `/**/ writeq(1.0e).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 70 | `op(9,xfy,e). 1.2e 3 = e(X,Y).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 71 | `writeq(1.0e100).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 72 | `float(1.0ee9).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 73 | `(- (1)) = -(1).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 74 | `(- -1) = -(-1).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 75 | `(- 1^2) = ^(-1,2).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 76 | `integer(- 1).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 77 | `integer('-'1).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Expecting a value,... |
| 78 | `integer('-' 1).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Expecting a value,... |
| 79 | `integer(- /*.*/1).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 80 | `integer(-/*.*/1).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 81 | `integer('-'/*.*/1).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Expecting a value,... |
| 82 | `atom(-/**/-).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Expecting a value,... |
| 83 | `op(0,fy,-).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 84 | `/**/ integer(-1).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 85 | `/**/ integer(- 1).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 86 | `/**/ writeq(-(1)).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 87 | `/**/ writeq([-]).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 88 | `writeq(-(1)).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 89 | `writeq(-(-1)).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 90 | `writeq(-(1^2)).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 91 | `writeq(-(a^2)).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 92 | `writeq(-((a,b))).` | OK | SYNTAX_ERROR | FAIL | FAIL | OK | error(syntax_error(Expecting a value,... |
| 93 | `writeq(-(1*2)).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 94 | `writeq(-a).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Expecting a value,... |
| 95 | `writeq(-(-)).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 96 | `writeq(-[-]).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Expecting a value,... |
| 97 | `writeq(-p(c)).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Expecting a value,... |
| 98 | `writeq(-{}).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Expecting a value,... |
| 99 | `writeq(-{a}).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Expecting a value,... |
| 100 | `writeq(-(-a)).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Expecting a value,... |
| 101 | `writeq(-(-(-a))).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Expecting a value,... |
| 102 | `writeq(-(-(1))).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 103 | `op(100,yfx,~). writeq(-(1~2~3)).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 104 | `/**/ writeq(- (1~2)).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 105 | `/**/ writeq(1~2).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 106 | `op(9,xfy,.), writeq(-[1]).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 107 | `op(9,xf,'$VAR'), writeq(- '$VAR'(0)).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 108 | `/**/ writeq('$VAR'(0)).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 109 | `/**/ writeq('$VAR'(-1)).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 110 | `op(1,yf,yf1). {-1 yf1}={yf1(X)}.` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 111 | `compound(+1).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 112 | `compound(+ 1).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 113 | `writeq(+1^2).` | OK | SYNTAX_ERROR | FAIL | WAITS | OK | error(syntax_error(Expecting a value,... |
| 114 | `op(0,fy,+). compound(+1).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 115 | `[(:-)&#124;(:-)]=[:-&#124;:-].` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 116 | `X=[a&#124;b,c].` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 117 | `op(1000,xfy,','). p._e.(m., o.,',').` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 118 | `op(1001,xfy,','). p._e.(m., o.,','). or p._e.(c...` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 119 | `op(999,xfy,'&#124;'). p._e.(c., o.,'&#124;').` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 120 | `/**/ X=[a&#124;b].` | OK | SYNTAX_ERROR | FAIL | SYNTAX_ERROR | OK | error(syntax_error(Expecting a value,... |
| 121 | `/**/ X=[(a&#124;b)].` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 122 | `/**/ [a&#124;[]=[a].` | OK | SYNTAX_ERROR | FAIL | WAITS | OK | error(syntax_error(Expecting a value,... |
| 123 | `/**/ X=[a&#124;b&#124;c].` | OK | SYNTAX_ERROR | FAIL | SYNTAX_ERROR | OK | error(syntax_error(Expecting a value,... |
| 124 | `var(a:-b).` | OK | SYNTAX_ERROR | FAIL | WAITS | OK | error(syntax_error(Expecting a value,... |
| 125 | `:- = :- .` | OK | SYNTAX_ERROR | FAIL | WAITS | OK | error(syntax_error(Expecting a value,... |
| 126 | `\- = - .` | OK | SYNTAX_ERROR | FAIL | SYNTAX_ERROR | OK | error(syntax_error(Expecting a value,... |
| 127 | `* = * .` | OK | SYNTAX_ERROR | FAIL | TYPE_ERROR | OK | error(syntax_error(Expecting a value,... |
| 128 | `current_op(200,fy,-).` | OK | SYNTAX_ERROR | FAIL | SYNTAX_ERROR | OK | error(syntax_error(Expecting a value,... |
| 129 | `current_op(200,fy,+).` | OK | SYNTAX_ERROR | FAIL | WAITS | OK | error(syntax_error(Expecting a value,... |
| 130 | `{- - c}={-(-(c))}.` | OK | SYNTAX_ERROR | FAIL | TYPE_ERROR | OK | error(syntax_error(Expecting a value,... |
| 131 | `(- -) = -(-).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 132 | `(- - -) = -(-(-)).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 133 | `(- - - -) = -(-(-(-))).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 134 | `{- :- c} = {:-(:-,c)}.` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 135 | `{- = - 1}={(-(=)) - 1}.` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 136 | `write_canonical((- = - 1)).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 137 | `write_canonical((- = -1)).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 138 | `write_canonical((-;)).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 139 | `write_canonical((-;-)).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 140 | `write_canonical((:-;-)).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 141 | `[:- -c] = [(:- -c)].` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 142 | `writeq([a,b&#124;,]).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 143 | `X ={,}.` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 144 | `{1} = {}(1).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 145 | `write_canonical({1}).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 146 | `'[]'(1) = [ ](X).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 147 | `X = [] (1).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 148 | `op(100,yfy,op). d._e.(op._s., yfy).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 149 | `'''' = '\''.` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |
| 150 | `a = '\141\'.` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Expecting a value,... |

## Known Issues

[Automatically populated list of failing tests with their error messages]

## Regenerating This Report

```bash
uv run python tools/conformity_test.py --output docs/CONFORMITY_TESTING.md
```
