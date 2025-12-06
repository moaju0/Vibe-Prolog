# ISO Prolog Conformity Testing Results

> **Last Updated**: 2025-12-06 17:35:00
> **Test Suite**: ISO/IEC JTC1 SC22 WG17
> **Source**: https://www.complang.tuwien.ac.at/ulrich/iso-prolog/conformity_testing
> **Total Tests**: 355

## Summary

- **Conforming (PASS)**: 223 (62.8%)
- **Non-conforming (FAIL)**: 132 (37.2%)

### Result Distribution

- **OK Results**: 83 (23.4%)
- **Syntax Errors**: 270 (76.1%)
- **Type Errors**: 2 (0.6%)
- **Other Errors**: 0 (0.0%)

## Results by Category

### Character Escapes (Tests 1-309)

- Conforming: 49/72 (68.1%)

### Numeric Literals (Tests 60-308)

- Conforming: 19/26 (73.1%)

### Operators (Tests 38-320)

- Conforming: 73/131 (55.7%)

### Special Syntax (Tests 74-355)

- Conforming: 82/126 (65.1%)

## Detailed Results

| Test # | Query | Expected | Actual | Status | SWI | Scryer | Error |
|--------|-------|----------|--------|--------|-----|--------|-------|
| 1 | `writeq('\n').` | OK | OK | PASS | OK | OK |  |
| 2 | `'` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | WAITS | OK | error(syntax_error(No terminal matche... |
| 3 | `)` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(No terminal matche... |
| 4 | `)'` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(No terminal matche... |
| 5 | `.` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Unexpected end-of-... |
| 6 | `writeq(' ').` | OK | OK | PASS | OK | OK |  |
| 7 | `0'\t=0' .` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(No terminal matche... |
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
| 18 | `writeq('\7\').` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(No terminal matche... |
| 19 | `writeq('\ca').` | OK | OK | PASS | OK | OK |  |
| 20 | `writeq('\d').` | OK | OK | PASS | OK | OK |  |
| 21 | `writeq('\e').` | SYNTAX_ERROR | OK | FAIL | OK | OK |  |
| 22 | `writeq('\033\').` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(No terminal matche... |
| 23 | `writeq('\0\').` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(No terminal matche... |
| 24 | `char_code('\e',C).` | SYNTAX_ERROR | TYPE_ERROR | FAIL | WAITS | OK | error(type_error(character, \e), cont... |
| 25 | `char_code('\d',C).` | SYNTAX_ERROR | TYPE_ERROR | FAIL | OK | OK | error(type_error(character, \d), cont... |
| 26 | `writeq('\u1').` | SYNTAX_ERROR | OK | FAIL | WAITS | OK |  |
| 27 | `writeq('\u0021').` | OK | OK | PASS | OK | OK |  |
| 28 | `put_code(0'\u0021).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(No terminal matche... |
| 29 | `writeq("\u0021").` | SYNTAX_ERROR | OK | FAIL | OK | OK |  |
| 30 | `writeq('\x21\').` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(No terminal matche... |
| 31 | `writeq('\x0021\').` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(No terminal matche... |
| 32 | `X = 0'\u1.` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(No terminal matche... |
| 33 | `writeq('\n').` | SYNTAX_ERROR | OK | FAIL | OK | OK |  |
| 34 | `writeq(.).` | SYNTAX_ERROR | OK | FAIL | OK | OK |  |
| 35 | `'\n''.` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(No terminal matche... |
| 36 | `X = 0'\. .` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(No terminal matche... |
| 37 | `writeq((-)-(-)).` | OK | OK | PASS | OK | OK |  |
| 38 | `writeq(((:-):-(:-))).` | OK | OK | PASS | OK | OK |  |
| 39 | `writeq((\\*)=(\\*)).` | OK | OK | PASS | OK | OK |  |
| 40 | `writeq([:-,-]).` | OK | OK | PASS | OK | OK |  |
| 41 | `writeq(f(\\*)).` | OK | OK | PASS | OK | OK |  |
| 42 | `writeq(a\\*(b+c)).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(No terminal matche... |
| 43 | `writeq(f(;,'&#124;',';;')).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(No terminal matche... |
| 44 | `writeq([.,.(.,.,.)]).` | OK | OK | PASS | OK | OK |  |
| 45 | `writeq((a :- b,c)).` | OK | OK | PASS | OK | OK |  |
| 46 | `write_canonical([a]).` | OK | OK | PASS | OK | OK |  |
| 47 | `writeq('/*').` | OK | OK | PASS | OK | OK |  |
| 48 | `writeq(//*).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 49 | `writeq(//*./*/).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 50 | `writeq('/**').` | OK | OK | PASS | OK | OK |  |
| 51 | `writeq('*/').` | OK | OK | PASS | OK | OK |  |
| 52 | `"'\\`\"" = "'`"".` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(No terminal matche... |
| 53 | `"'\\"" = "'"".` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(No terminal matche... |
| 54 | `\\` = `'.` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(No terminal matche... |
| 55 | `'\\`\"' = ''`'.` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(No terminal matche... |
| 56 | `writeq(''\\`\"\"').` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(No terminal matche... |
| 57 | `('\\\\') = (\\).` | OK | OK | PASS | OK | OK |  |
| 58 | `op(1,xf,xf1). 1xf1 = xf1(1).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(No terminal matche... |
| 59 | `X = 0X1.` | OK | OK | PASS | OK | OK |  |
| 60 | `float(.0).` | OK | OK | PASS | OK | OK |  |
| 61 | `op(100,xfx,'.'). functor(3 .2,F,A).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(No terminal matche... |
| 62 | `float(- .0).` | OK | OK | PASS | OK | OK |  |
| 63 | `float(1E9).` | OK | OK | PASS | OK | OK |  |
| 64 | `integer(1e).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | SYNTAX_ERROR | OK | error(syntax_error(No terminal matche... |
| 65 | `op(9,xf,e9). 1e9 = e9(1).` | OK | OK | PASS | OK | OK |  |
| 66 | `op(9,xf,e). 1e-9 = -(e(1),9).` | OK | OK | PASS | OK | OK |  |
| 67 | `/**/ 1.0e- 9 = -(e(1.0),9).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 68 | `/**/ writeq(1e).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 69 | `/**/ writeq(1.0e).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 70 | `op(9,xfy,e). 1.2e 3 = e(X,Y).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(No terminal matche... |
| 71 | `writeq(1.0e100).` | OK | OK | PASS | OK | OK |  |
| 72 | `float(1.0ee9).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(No terminal matche... |
| 73 | `(- (1)) = -(1).` | OK | OK | PASS | OK | OK |  |
| 74 | `(- -1) = -(-1).` | OK | OK | PASS | OK | OK |  |
| 75 | `(- 1^2) = ^(-1,2).` | OK | OK | PASS | OK | OK |  |
| 76 | `integer(- 1).` | OK | OK | PASS | OK | OK |  |
| 77 | `integer('-'1).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(No terminal matche... |
| 78 | `integer('-' 1).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(No terminal matche... |
| 79 | `integer(- /*.*/1).` | OK | OK | PASS | OK | OK |  |
| 80 | `integer(-/*.*/1).` | OK | OK | PASS | OK | OK |  |
| 81 | `integer('-'/*.*/1).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(No terminal matche... |
| 82 | `atom(-/**/-).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Unterminated block... |
| 83 | `op(0,fy,-).` | OK | OK | PASS | OK | OK |  |
| 84 | `/**/ integer(-1).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 85 | `/**/ integer(- 1).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 86 | `/**/ writeq(-(1)).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 87 | `/**/ writeq([-]).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 88 | `writeq(-(1)).` | OK | OK | PASS | OK | OK |  |
| 89 | `writeq(-(-1)).` | OK | OK | PASS | OK | OK |  |
| 90 | `writeq(-(1^2)).` | OK | OK | PASS | OK | OK |  |
| 91 | `writeq(-(a^2)).` | OK | OK | PASS | OK | OK |  |
| 92 | `writeq(-((a,b))).` | OK | OK | PASS | FAIL | OK |  |
| 93 | `writeq(-(1\\*2)).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(No terminal matche... |
| 94 | `writeq(-a).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(No terminal matche... |
| 95 | `writeq(-(-)).` | OK | OK | PASS | OK | OK |  |
| 96 | `writeq(-[-]).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(No terminal matche... |
| 97 | `writeq(-p(c)).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(No terminal matche... |
| 98 | `writeq(-{}).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(No terminal matche... |
| 99 | `writeq(-{a}).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(No terminal matche... |
| 100 | `writeq(-(-a)).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(No terminal matche... |
| 101 | `writeq(-(-(-a))).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(No terminal matche... |
| 102 | `writeq(-(-(1))).` | OK | OK | PASS | OK | OK |  |
| 103 | `op(100,yfx,~). writeq(-(1~2~3)).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(No terminal matche... |
| 104 | `/**/ writeq(- (1~2)).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 105 | `/**/ writeq(1~2).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 106 | `op(9,xfy,.), writeq(-[1]).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(No terminal matche... |
| 107 | `op(9,xf,'$VAR'), writeq(- '$VAR'(0)).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(No terminal matche... |
| 108 | `/**/ writeq('$VAR'(0)).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 109 | `/**/ writeq('$VAR'(-1)).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 110 | `op(1,yf,yf1). {-1 yf1}={yf1(X)}.` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(No terminal matche... |
| 111 | `compound(+1).` | OK | OK | PASS | OK | OK |  |
| 112 | `compound(+ 1).` | OK | OK | PASS | OK | OK |  |
| 113 | `writeq(+1^2).` | OK | OK | PASS | WAITS | OK |  |
| 114 | `op(0,fy,+). compound(+1).` | OK | OK | PASS | OK | OK |  |
| 115 | `[(:-)&#124;(:-)]=[:-&#124;:-].` | OK | OK | PASS | OK | OK |  |
| 116 | `X=[a&#124;b,c].` | OK | OK | PASS | OK | OK |  |
| 117 | `op(1000,xfy,','). p._e.(m., o.,',').` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(No terminal matche... |
| 118 | `op(1001,xfy,','). p._e.(m., o.,','). or p._e.(c...` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(No terminal matche... |
| 119 | `op(999,xfy,'&#124;'). p._e.(c., o.,'&#124;').` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(No terminal matche... |
| 120 | `/**/ X=[a&#124;b].` | OK | SYNTAX_ERROR | FAIL | SYNTAX_ERROR | OK | error(syntax_error(Unterminated block... |
| 121 | `/**/ X=[(a&#124;b)].` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 122 | `/**/ [a&#124;[]=[a].` | OK | SYNTAX_ERROR | FAIL | WAITS | OK | error(syntax_error(Unterminated block... |
| 123 | `/**/ X=[a&#124;b&#124;c].` | OK | SYNTAX_ERROR | FAIL | SYNTAX_ERROR | OK | error(syntax_error(Unterminated block... |
| 124 | `var(a:-b).` | OK | OK | PASS | WAITS | OK |  |
| 125 | `:- = :- .` | OK | OK | PASS | WAITS | OK |  |
| 126 | `\- = - .` | OK | OK | PASS | SYNTAX_ERROR | OK |  |
| 127 | `\\* = \\* .` | OK | OK | PASS | TYPE_ERROR | OK |  |
| 128 | `current_op(200,fy,-).` | OK | OK | PASS | SYNTAX_ERROR | OK |  |
| 129 | `current_op(200,fy,+).` | OK | OK | PASS | WAITS | OK |  |
| 130 | `{- - c}={-(-(c))}.` | OK | OK | PASS | TYPE_ERROR | OK |  |
| 131 | `(- -) = -(-).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(No terminal matche... |
| 132 | `(- - -) = -(-(-)).` | OK | OK | PASS | OK | OK |  |
| 133 | `(- - - -) = -(-(-(-))).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(No terminal matche... |
| 134 | `{- :- c} = {:-(:-,c)}.` | OK | OK | PASS | OK | OK |  |
| 135 | `{- = - 1}={(-(=)) - 1}.` | OK | OK | PASS | OK | OK |  |
| 136 | `write_canonical((- = - 1)).` | OK | OK | PASS | OK | OK |  |
| 137 | `write_canonical((- = -1)).` | OK | OK | PASS | OK | OK |  |
| 138 | `write_canonical((-;)).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(No terminal matche... |
| 139 | `write_canonical((-;-)).` | OK | OK | PASS | OK | OK |  |
| 140 | `write_canonical((:-;-)).` | OK | OK | PASS | OK | OK |  |
| 141 | `[:- -c] = [(:- -c)].` | OK | OK | PASS | OK | OK |  |
| 142 | `writeq([a,b&#124;,]).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(No terminal matche... |
| 143 | `X ={,}.` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(No terminal matche... |
| 144 | `{1} = {}(1).` | OK | OK | PASS | OK | OK |  |
| 145 | `write_canonical({1}).` | OK | OK | PASS | OK | OK |  |
| 146 | `'[]'(1) = [ ](X).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(No terminal matche... |
| 147 | `X = [] (1).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(No terminal matche... |
| 148 | `op(100,yfy,op). d._e.(op._s., yfy).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(No terminal matche... |
| 149 | `'''' = '\''.` | OK | OK | PASS | OK | OK |  |
| 150 | `a = '\141\'.` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(No terminal matche... |
| 151 | `a = '\141'.` | OK | OK | PASS | OK | OK |  |
| 152 | `X = '\141\141'.` | OK | OK | PASS | OK | OK |  |
| 153 | `X = '\9'.` | OK | OK | PASS | OK | OK |  |
| 154 | `X = '\N'.` | OK | OK | PASS | SYNTAX_ERROR | OK |  |
| 155 | `X = '\\' .` | OK | OK | PASS | OK | OK |  |
| 156 | `X = '\77777777777\'.` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(No terminal matche... |
| 157 | `a = '\x61\'.` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(No terminal matche... |
| 158 | `atom_codes('\xG\',Cs).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(No terminal matche... |
| 159 | `atom_codes('\xG1\',Cs).` | OK | SYNTAX_ERROR | FAIL | SYNTAX_ERROR | OK | error(syntax_error(No terminal matche... |
| 160 | `atom(`).` | OK | SYNTAX_ERROR | FAIL | SYNTAX_ERROR | OK | error(syntax_error(No terminal matche... |
| 161 | `atom(`+).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(No terminal matche... |
| 162 | `atom(`\n`).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(No terminal matche... |
| 163 | `X = `a`.` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(No terminal matche... |
| 164 | `integer(0'\).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(No terminal matche... |
| 165 | `integer(0''').` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Error trying to pr... |
| 166 | `0''' = 0'\'.` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(No terminal matche... |
| 167 | `integer(0'').` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(No terminal matche... |
| 168 | `op(100,xf,'').` | OK | OK | PASS | OK | OK |  |
| 169 | `/**/ (0 '') = ''(X).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 170 | `/**/ writeq(0 '').` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 171 | `/**/ writeq(0'').` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 172 | `op(100,xfx,'').` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 173 | `/**/ functor(0 ''1, F, A).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 174 | `/**/ functor(0''1, F, A).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 175 | `op(100,xf,f). writeq(0'f\').` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 176 | `/**/ writeq(0'f'f').` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 177 | `/**/ writeq(0'ff).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 178 | `/**/ writeq(0f).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 179 | `op(100,xf,'f '). writeq(0 'f ').` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 180 | `X = 2'1.` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 181 | `op(100,xfx,'1'). functor(2'1'y, F, A).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | SYNTAX_ERROR | OK | error(syntax_error(Empty terminals ar... |
| 182 | `/**/ functor(2 '1'y, F, A).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 183 | `X =0'\x41\ .` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 184 | `X =0'\x41\.` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 185 | `X =0'\x1\.` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 186 | `writeq(0'\x\).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | WAITS | OK | error(syntax_error(Empty terminals ar... |
| 187 | `X is 16'mod'2.` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | WAITS | OK | error(syntax_error(Empty terminals ar... |
| 188 | `X is 37'mod'2.` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 189 | `X is 0'mod'1.` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 190 | `X is 1'+'1.` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 191 | `X is 1'\n+'1.` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 192 | `X is 0'\n+'1.` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 193 | `X = 0'\n+'/*'. %*/1.` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 194 | `X = 0'\na.` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 195 | `X is 0'\n` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 196 | `X = 0'\n.\` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 197 | `op(100,fx,' op').` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 198 | `/**/ writeq(' op' '1').` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 199 | `/**/ writeq(' op'[]).` | OK | SYNTAX_ERROR | FAIL | FAIL | OK | error(syntax_error(Unterminated block... |
| 200 | `op(1,xf,xf1). writeq({- =xf1}).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 201 | `writeq(- (a\\*b)).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 202 | `writeq(\\ (a\\*b)).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 203 | `current_op(P,xfy,.).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 204 | `op(100,xfy,.). writeq(1 .2).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | FAIL | OK | error(syntax_error(Empty terminals ar... |
| 205 | `/**/ writeq([1]).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 206 | `/**/ writeq(-[1]).` | OK | SYNTAX_ERROR | FAIL | WAITS | OK | error(syntax_error(Unterminated block... |
| 207 | `/**/ X = 1.e.` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 208 | `write_canonical('$VAR'(0)).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 209 | `write_term('$VAR'(0),[]).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 210 | `writeq('$VAR'(0)).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 211 | `writeq('$VAR'(-1)).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 212 | `writeq('$VAR'(-2)).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 213 | `writeq('$VAR'(x)).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | WAITS | OK | error(syntax_error(Empty terminals ar... |
| 214 | `writeq('$VAR'('A')).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 215 | `op(9,fy,fy),op(9,yf,yf). write_canonical(fy 1 yf).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 216 | `/**/ write_canonical(fy yf).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 217 | `/**/ writeq(fy(yf(1))).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 218 | `/**/ writeq(yf(fy(1))).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 219 | `/**/ writeq(yf(fy(yf(fy(1))))).` | OK | SYNTAX_ERROR | FAIL | FAIL | OK | error(syntax_error(Unterminated block... |
| 220 | `op(9,fy,fy),op(9,yfx,yfx). write_canonical(fy 1...` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 221 | `/**/ writeq(fy(yfx(1,2))).` | OK | SYNTAX_ERROR | FAIL | SYNTAX_ERROR | OK | error(syntax_error(Unterminated block... |
| 222 | `/**/ writeq(yfx(fy(1),2)).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 223 | `op(9,yf,yf),op(9,xfy,xfy). write_canonical(1 xf...` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 224 | `/**/ writeq(xfy(1,yf(2))).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 225 | `/**/ writeq(yf(xfy(1,2))).` | OK | SYNTAX_ERROR | FAIL | ERROR | OK | error(syntax_error(Unterminated block... |
| 226 | `op(0,xfy,:-). current_op(P,xfx,:-).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 227 | `op(0,xfy,','). p._e.(m., o.,',').` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 228 | `op(9,fy,p),op(9,yf,p). write_canonical(p p 0).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 229 | `/**/ writeq(p(p(0))).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 230 | `/**/ write_canonical(p 0 p).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 231 | `/**/ write_canonical(0 p p).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 232 | `/**/ write_canonical(p p).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 233 | `op(9,fy,p),op(9,yfx,p). write_canonical(1 p p p...` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 234 | `op(9,fy,p),op(9,xfy,p). write_canonical(1 p p p...` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 235 | `op(7,fy,p),op(9,yfx,p). write_canonical(1 p p p...` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 236 | `atom('.'.'.').` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 237 | `op(0,xfy,'&#124;').` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 238 | `/**/ writeq((a&#124;b)).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 239 | `op(0,xfy,.),op(9,yf,.).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | SYNTAX_ERROR | OK | error(syntax_error(Empty terminals ar... |
| 240 | `/**/ writeq(.(.)).` | OK | SYNTAX_ERROR | FAIL | SYNTAX_ERROR | OK | error(syntax_error(Unterminated block... |
| 241 | `op(0,xfy,.),writeq((.)+(.))).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 242 | `set_prolog_flag(double_quotes,chars).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | SYNTAX_ERROR | OK | error(syntax_error(Empty terminals ar... |
| 243 | `/**/ writeq("a").` | OK | SYNTAX_ERROR | FAIL | SYNTAX_ERROR | OK | error(syntax_error(Unterminated block... |
| 244 | `/**/ writeq("\z").` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 245 | `/**/ writeq("\0\").` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 246 | `X is 10.0** -323, writeq(X).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 247 | `1.0e-323=:=10.0** -323.` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 248 | `\-1 = -0x1.` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 249 | `T = t(0b1,0o1,0x1).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 250 | `X is 0b1mod 2.` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | SYNTAX_ERROR | OK | error(syntax_error(Empty terminals ar... |
| 251 | `op(1105,xfy,'&#124;').` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | SYNTAX_ERROR | OK | error(syntax_error(Empty terminals ar... |
| 252 | `/**/ writeq((a-->b,c&#124;d)).` | OK | SYNTAX_ERROR | FAIL | SYNTAX_ERROR | OK | error(syntax_error(Unterminated block... |
| 253 | `/**/ write_canonical((a&#124;b;c)).` | OK | SYNTAX_ERROR | FAIL | SYNTAX_ERROR | OK | error(syntax_error(Unterminated block... |
| 254 | `/**/ write_canonical((a;b&#124;c)).` | OK | SYNTAX_ERROR | FAIL | SYNTAX_ERROR | OK | error(syntax_error(Unterminated block... |
| 255 | `/**/ write_canonical([a;b&#124;c]).` | OK | SYNTAX_ERROR | FAIL | SYNTAX_ERROR | OK | error(syntax_error(Unterminated block... |
| 256 | `/**/ writeq([(a&#124;b)]).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 257 | `X/* /*/=7.` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 258 | `X/*/*/=7.` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 259 | `atom($-).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | SYNTAX_ERROR | OK | error(syntax_error(Empty terminals ar... |
| 260 | `atom(-$).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 261 | `op(900, fy, [$]). write_canonical($a+b).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 262 | `\ .` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 263 | `char_code(C,0), writeq(C).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 264 | `writeq('\0\').` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 265 | `write_canonical(_+_).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 266 | `write_canonical(B+B).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | SYNTAX_ERROR | OK | error(syntax_error(Empty terminals ar... |
| 267 | `writeq(0'\z).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | SYNTAX_ERROR | OK | error(syntax_error(Empty terminals ar... |
| 268 | `char_code('\^',X).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 269 | `writeq(0'\c).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 270 | `writeq(0'\ ).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 271 | `writeq(nop (1)).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 272 | `op(400,fx,f). writeq(f/*.*/(1,2)).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 273 | `/**/ writeq(1 = f).` | OK | SYNTAX_ERROR | FAIL | N/A | N/A | error(syntax_error(Unterminated block... |
| 274 | `write_canonical(a- - -b).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 275 | `op(699,xf,>.).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 276 | `/**/ writeq(>(>.(a),b)).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 277 | `/**/ write_canonical(a>. >b).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 278 | `/**/ write_canonical(a>. =b).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 279 | `/**/ write_canonical((a>.,b)).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 280 | `/**/ write_canonical(a>.).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 281 | `op(699,xf,>).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 282 | `/**/ writeq(>(>(a),b)).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 283 | `/**/ write_canonical(a> >b).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 284 | `/**/ write_canonical(a> =b).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 285 | `/**/ write_canonical((a>,b)).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 286 | `/**/ write_canonical(a>).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 287 | `/**/ write_canonical(a>b).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 288 | `op(9,yfx,[bop,bo,b,op,xor]). writeq(0bop 2).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | FAIL | OK | error(syntax_error(Empty terminals ar... |
| 289 | `/**/ writeq(0 bop 2).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 290 | `/**/ writeq(0bo 2).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 291 | `/**/ writeq(0b 2).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 292 | `/**/ writeq(0op 2).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 293 | `/**/ writeq(0xor 2).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 294 | `writeq('^\`').` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 295 | `op(9,yf,[b2,o8]).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 296 | `/**/ writeq(0b2).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 297 | `/**/ writeq(0o8).` | OK | SYNTAX_ERROR | FAIL | ERROR | OK | error(syntax_error(Unterminated block... |
| 298 | `op(500, xfy, {}). p._e.(c.,o.,{}).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 299 | `writeq('\b\r\f\t\n').` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 300 | `get_char(C).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 301 | `get_char(C).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 302 | `writeq(0B1).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 303 | `op(20,fx,--),writeq(--(a)).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 304 | `/**/ op(0,fy,--),writeq(--(a)).` | OK | SYNTAX_ERROR | FAIL | OK | OK | error(syntax_error(Unterminated block... |
| 305 | `writeq(0xamod 2).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 306 | `writeq(00'+'1).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 307 | `writeq(00'a).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 308 | `writeq('\^J').` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 309 | `writeq([(a,b)]).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 310 | `writeq(1= \\).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 311 | `writeq((,)).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 312 | `writeq({[}).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 313 | `writeq({(}).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 314 | `writeq([a,b&#124;c]).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 315 | `(\+ (a,b)) = \\+(T).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 316 | `[] = '[]'.` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 317 | `op(300,fy,~). writeq(~ (a=b)).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 318 | `writeq(\\ (a=b)).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 319 | `writeq(+ (a=b)).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 320 | `writeq([/*.*/]).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 321 | `writeq(.+).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 322 | `writeq({a,b}).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 323 | `writeq({\+ (}).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 324 | `writeq(\\+ (())).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 325 | `writeq(+((1\\*2)^3)).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 326 | `writeq(-((1\\*2)^3)).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 327 | `writeq([a&#124;\\ +2]).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 328 | `writeq((a)(b)).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 329 | `writeq('%').` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 330 | `writeq({[y]}).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 331 | `(>)(1,2).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 332 | `write_canonical(;(a)).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 333 | `write_canonical(';'(a)).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 334 | `write_canonical(;(a,b)).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 335 | `writeq(1 is _).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | SYNTAX_ERROR | OK | error(syntax_error(Empty terminals ar... |
| 336 | `op(9,fx,.),writeq(.(' ')).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | SYNTAX_ERROR | OK | error(syntax_error(Empty terminals ar... |
| 337 | `op(9,yf,.>).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 338 | `write_canonical(a.> .>).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 339 | `writeq(((a).>).>).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | WAITS | OK | error(syntax_error(Empty terminals ar... |
| 340 | `writeq([.&#124;1]).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 341 | `writeq(-'-'2).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 342 | `write_canonical((-,(1))).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 343 | `writeq(-[]1).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 344 | `writeq(-{}1).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 345 | `op(1000,xfx,~>). writeq([a~>b]).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 346 | `op(1000,yfx,~>). writeq([a~>b]).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | SYNTAX_ERROR | OK | error(syntax_error(Empty terminals ar... |
| 347 | `op(1000,xfy,~>). writeq([a~>b]).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 348 | `writeq({&#124;}).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 349 | `writeq(([)).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 350 | `writeq(((>)(1)).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 351 | `writeq({a:-b}).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 352 | `op(1105,xfy,'&#124;'). writeq((&#124;)).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 353 | `op(0,xfy,'&#124;'). writeq((&#124;)).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 354 | `Finis (()).` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |
| 355 | `Finis ().` | SYNTAX_ERROR | SYNTAX_ERROR | PASS | OK | OK | error(syntax_error(Empty terminals ar... |

## Known Issues

[Automatically populated list of failing tests with their error messages]

## Regenerating This Report

```bash
uv run python tools/conformity_test.py --output docs/CONFORMITY_TESTING.md
```
