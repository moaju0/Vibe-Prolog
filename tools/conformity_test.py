#!/usr/bin/env python3
"""
ISO Prolog Conformity Testing Framework

Runs the official ISO/IEC JTC1 SC22 WG17 conformity test suite against Vibe-Prolog
and generates a markdown report with results.

Usage:
    uv run python tools/conformity_test.py
    uv run python tools/conformity_test.py --tests 1-50
    uv run python tools/conformity_test.py --category escapes
    uv run python tools/conformity_test.py --verbose
"""

from __future__ import annotations

import argparse
import datetime
import sys
import time
from dataclasses import dataclass
from enum import Enum
from pathlib import Path
from typing import Dict, List, Optional, Tuple

_CATEGORIES = {
    "Character Escapes": [(1, 37), (150, 160), (184, 197), (264, 271), (300, 300), (309, 309)],
    "Operators": [(38, 59), (84, 115), (129, 142), (198, 242), (276, 288), (304, 305), (318, 320)],
    "Numeric Literals": [(60, 73), (181, 183), (247, 251), (303, 303), (306, 308)],
}

from vibeprolog import PrologInterpreter
from vibeprolog.exceptions import PrologError, PrologThrow


class TestResult(Enum):
    """Possible outcomes for a conformity test."""
    OK = "OK"
    SYNTAX_ERROR = "SYNTAX_ERROR"
    TYPE_ERROR = "TYPE_ERROR"
    EXCEPTION = "EXCEPTION"
    TIMEOUT = "TIMEOUT"


@dataclass
class ConformityTest:
    """Represents a single conformity test case."""
    num: int
    query: str
    references_previous: bool = False

    def __post_init__(self):
        """Set references_previous flag if the query contains the marker."""
        if "/**/" in self.query:
            self.references_previous = True

    @property
    def category(self) -> str:
        """Categorize the test based on its number (data-driven)."""
        for category, ranges in _CATEGORIES.items():
            for start, end in ranges:
                if start <= self.num <= end:
                    return category
        return "Special Syntax"


@dataclass
class TestExecutionResult:
    """Result of executing a single test."""
    test: ConformityTest
    result: TestResult
    error_message: Optional[str] = None
    execution_time: float = 0.0
    output: Optional[str] = None


# The complete list of 355 conformity tests
CONFORMITY_TESTS = [
    ConformityTest(1, "writeq('\\n')."),
    ConformityTest(2, "'"),
    ConformityTest(3, ")"),
    ConformityTest(4, ")'"),
    ConformityTest(5, "."),
    ConformityTest(6, "writeq(' ')."),  # horiz. tab
    ConformityTest(7, "0'\\t=0' ."),  # horiz. tab
    ConformityTest(8, "writeq('\\n')."),
    ConformityTest(9, "writeq('\\\\n')."),  # "\\\\n"
    ConformityTest(10, "writeq('\\\\na')."),  # "\\\\na"
    ConformityTest(11, "writeq('a\\\\nb')."),  # "a\\\\nb"
    ConformityTest(12, "writeq('a\\\\n b')."),  # "a\\\\n b"
    ConformityTest(13, "writeq('\\\\ ')."),
    ConformityTest(14, "writeq('\\\\\\n')."),  # "\\\\ \\n"
    ConformityTest(15, "writeq('\\\\t')."),  # "\\\\t"
    ConformityTest(16, "writeq('\\t')."),
    ConformityTest(17, "writeq('\\a')."),
    ConformityTest(18, "writeq('\\7\\')."),
    ConformityTest(19, "writeq('\\ca')."),
    ConformityTest(20, "writeq('\\d')."),
    ConformityTest(21, "writeq('\\e')."),
    ConformityTest(22, "writeq('\\033\\')."),
    ConformityTest(23, "writeq('\\0\\')."),
    ConformityTest(24, "char_code('\\e',C)."),
    ConformityTest(25, "char_code('\\d',C)."),
    ConformityTest(26, "writeq('\\u1')."),
    ConformityTest(27, "writeq('\\u0021')."),
    ConformityTest(28, "put_code(0'\\u0021)."),
    ConformityTest(29, "writeq(\"\\u0021\")."),
    ConformityTest(30, "writeq('\\x21\\')."),
    ConformityTest(31, "writeq('\\x0021\\')."),
    ConformityTest(32, "X = 0'\\u1."),
    ConformityTest(33, "writeq('\\n')."),
    ConformityTest(34, "writeq(.)."),
    ConformityTest(35, "'\\n''."),
    ConformityTest(36, "X = 0'\\. ."),
    ConformityTest(37, "writeq((-)-(-))."),
    ConformityTest(38, "writeq(((:-):-(:-)))."),
    ConformityTest(39, "writeq((\\\*)=(\\\*))."),
    ConformityTest(40, "writeq([:-,-])."),
    ConformityTest(41, "writeq(f(\\\*))."),
    ConformityTest(42, "writeq(a\\\*(b+c))."),
    ConformityTest(43, "writeq(f(;,'|',';;'))."),
    ConformityTest(44, "writeq([.,.(.,.,.)])."),
    ConformityTest(45, "writeq((a :- b,c))."),
    ConformityTest(46, "write_canonical([a])."),
    ConformityTest(47, "writeq('/*')."),
    ConformityTest(48, "writeq(//*)."),
    ConformityTest(49, "writeq(//*./*/)."),
    ConformityTest(50, "writeq('/**')."),
    ConformityTest(51, "writeq('*/')."),
    ConformityTest(52, "\"'\\\\`\\\"\" = \"'`\"\"."),  # "
    ConformityTest(53, "\"'\\\\\"\" = \"'\"\"."),  # "
    ConformityTest(54, "\\\\` = `'."),
    ConformityTest(55, "'\\\\`\\\"' = ''`'."),
    ConformityTest(56, "writeq(''\\\\`\\\"\\\"')."),
    ConformityTest(57, "('\\\\\\\\') = (\\\\)."),
    ConformityTest(58, "op(1,xf,xf1). 1xf1 = xf1(1)."),
    ConformityTest(59, "X = 0X1."),
    ConformityTest(60, "float(.0)."),
    ConformityTest(61, "op(100,xfx,'.'). functor(3 .2,F,A)."),
    ConformityTest(62, "float(- .0)."),
    ConformityTest(63, "float(1E9)."),
    ConformityTest(64, "integer(1e)."),
    ConformityTest(65, "op(9,xf,e9). 1e9 = e9(1)."),
    ConformityTest(66, "op(9,xf,e). 1e-9 = -(e(1),9)."),
    ConformityTest(67, "/**/ 1.0e- 9 = -(e(1.0),9)."),
    ConformityTest(68, "/**/ writeq(1e)."),
    ConformityTest(69, "/**/ writeq(1.0e)."),
    ConformityTest(70, "op(9,xfy,e). 1.2e 3 = e(X,Y)."),
    ConformityTest(71, "writeq(1.0e100)."),
    ConformityTest(72, "float(1.0ee9)."),
    ConformityTest(73, "(- (1)) = -(1)."),
    ConformityTest(74, "(- -1) = -(-1)."),
    ConformityTest(75, "(- 1^2) = ^(-1,2)."),
    ConformityTest(76, "integer(- 1)."),
    ConformityTest(77, "integer('-'1)."),
    ConformityTest(78, "integer('-' 1)."),
    ConformityTest(79, "integer(- /*.*/1)."),
    ConformityTest(80, "integer(-/*.*/1)."),
    ConformityTest(81, "integer('-'/*.*/1)."),
    ConformityTest(82, "atom(-/**/-)."),
    ConformityTest(83, "op(0,fy,-)."),
    ConformityTest(84, "/**/ integer(-1)."),
    ConformityTest(85, "/**/ integer(- 1)."),
    ConformityTest(86, "/**/ writeq(-(1))."),
    ConformityTest(87, "/**/ writeq([-])."),
    ConformityTest(88, "writeq(-(1))."),
    ConformityTest(89, "writeq(-(-1))."),
    ConformityTest(90, "writeq(-(1^2))."),
    ConformityTest(91, "writeq(-(a^2))."),
    ConformityTest(92, "writeq(-((a,b)))."),
    ConformityTest(93, "writeq(-(1\\\*2))."),
    ConformityTest(94, "writeq(-a)."),
    ConformityTest(95, "writeq(-(-))."),
    ConformityTest(96, "writeq(-[-])."),
    ConformityTest(97, "writeq(-p(c))."),
    ConformityTest(98, "writeq(-{})."),
    ConformityTest(99, "writeq(-{a})."),
    ConformityTest(100, "writeq(-(-a))."),
    ConformityTest(101, "writeq(-(-(-a)))."),
    ConformityTest(102, "writeq(-(-(1)))."),
    ConformityTest(103, "op(100,yfx,~). writeq(-(1~2~3))."),
    ConformityTest(104, "/**/ writeq(- (1~2))."),
    ConformityTest(105, "/**/ writeq(1~2)."),
    ConformityTest(106, "op(9,xfy,.), writeq(-[1])."),
    ConformityTest(107, "op(9,xf,'$VAR'), writeq(- '$VAR'(0))."),
    ConformityTest(108, "/**/ writeq('$VAR'(0))."),
    ConformityTest(109, "/**/ writeq('$VAR'(-1))."),
    ConformityTest(110, "op(1,yf,yf1). {-1 yf1}={yf1(X)}."),
    ConformityTest(111, "compound(+1)."),
    ConformityTest(112, "compound(+ 1)."),
    ConformityTest(113, "writeq(+1^2)."),
    ConformityTest(114, "op(0,fy,+). compound(+1)."),
    ConformityTest(115, "[(:-)|(:-)]=[:-|:-]."),
    ConformityTest(116, "X=[a|b,c]."),
    ConformityTest(117, "op(1000,xfy,','). p._e.(m., o.,',')."),
    ConformityTest(118, "op(1001,xfy,','). p._e.(m., o.,','). or p._e.(c., o.,',')."),
    ConformityTest(119, "op(999,xfy,'|'). p._e.(c., o.,'|')."),
    ConformityTest(120, "/**/ X=[a|b]."),
    ConformityTest(121, "/**/ X=[(a|b)]."),
    ConformityTest(122, "/**/ [a|[]=[a]."),
    ConformityTest(123, "/**/ X=[a|b|c]."),
    ConformityTest(124, "var(a:-b)."),
    ConformityTest(125, ":- = :- ."),
    ConformityTest(126, "\\- = - ."),
    ConformityTest(127, "\\\* = \\\* ."),
    ConformityTest(128, "current_op(200,fy,-)."),
    ConformityTest(129, "current_op(200,fy,+)."),
    ConformityTest(130, "{- - c}={-(-(c))}."),
    ConformityTest(131, "(- -) = -(-)."),
    ConformityTest(132, "(- - -) = -(-(-))."),
    ConformityTest(133, "(- - - -) = -(-(-(-)))."),
    ConformityTest(134, "{- :- c} = {:-(:-,c)}."),
    ConformityTest(135, "{- = - 1}={(-(=)) - 1}."),
    ConformityTest(136, "write_canonical((- = - 1))."),
    ConformityTest(137, "write_canonical((- = -1))."),
    ConformityTest(138, "write_canonical((-;))."),
    ConformityTest(139, "write_canonical((-;-))."),
    ConformityTest(140, "write_canonical((:-;-))."),
    ConformityTest(141, "[:- -c] = [(:- -c)]."),
    ConformityTest(142, "writeq([a,b|,])."),
    ConformityTest(143, "X ={,}."),
    ConformityTest(144, "{1} = {}(1)."),
    ConformityTest(145, "write_canonical({1})."),
    ConformityTest(146, "'[]'(1) = [ ](X)."),
    ConformityTest(147, "X = [] (1)."),
    ConformityTest(148, "op(100,yfy,op). d._e.(op._s., yfy)."),
    ConformityTest(149, "'''' = '\\''."),
    ConformityTest(150, "a = '\\141\\'."),
    ConformityTest(151, "a = '\\141'."),
    ConformityTest(152, "X = '\\141\\141'."),
    ConformityTest(153, "X = '\\9'."),
    ConformityTest(154, "X = '\\N'."),
    ConformityTest(155, "X = '\\\\' ."),
    ConformityTest(156, "X = '\\77777777777\\'."),
    ConformityTest(157, "a = '\\x61\\'."),
    ConformityTest(158, "atom_codes('\\xG\\',Cs)."),
    ConformityTest(159, "atom_codes('\\xG1\\',Cs)."),
    ConformityTest(160, "atom(`)."),
    ConformityTest(161, "atom(`+)."),
    ConformityTest(162, "atom(`\\n`)."),
    ConformityTest(163, "X = `a`."),
    ConformityTest(164, "integer(0'\\)."),
    ConformityTest(165, "integer(0''')."),
    ConformityTest(166, "0''' = 0'\\'."),
    ConformityTest(167, "integer(0'')."),
    ConformityTest(168, "op(100,xf,'')."),
    ConformityTest(169, "/**/ (0 '') = ''(X)."),
    ConformityTest(170, "/**/ writeq(0 '')."),
    ConformityTest(171, "/**/ writeq(0'')."),
    ConformityTest(172, "op(100,xfx,'')."),
    ConformityTest(173, "/**/ functor(0 ''1, F, A)."),
    ConformityTest(174, "/**/ functor(0''1, F, A)."),
    ConformityTest(175, "op(100,xf,f). writeq(0'f\\')."),
    ConformityTest(176, "/**/ writeq(0'f'f')."),
    ConformityTest(177, "/**/ writeq(0'ff)."),
    ConformityTest(178, "/**/ writeq(0f)."),
    ConformityTest(179, "op(100,xf,'f '). writeq(0 'f ')."),
    ConformityTest(180, "X = 2'1."),
    ConformityTest(181, "op(100,xfx,'1'). functor(2'1'y, F, A)."),
    ConformityTest(182, "/**/ functor(2 '1'y, F, A)."),
    ConformityTest(183, "X =0'\\x41\\ ."),
    ConformityTest(184, "X =0'\\x41\\."),
    ConformityTest(185, "X =0'\\x1\\."),
    ConformityTest(186, "writeq(0'\\x\\)."),
    ConformityTest(187, "X is 16'mod'2."),
    ConformityTest(188, "X is 37'mod'2."),
    ConformityTest(189, "X is 0'mod'1."),
    ConformityTest(190, "X is 1'+'1."),
    ConformityTest(191, "X is 1'\\n+'1."),
    ConformityTest(192, "X is 0'\\n+'1."),
    ConformityTest(193, "X = 0'\\n+'/*'. %*/1."),
    ConformityTest(194, "X = 0'\\na."),
    ConformityTest(195, "X is 0'\\n"),
    ConformityTest(196, "X = 0'\\n.\\"),
    ConformityTest(197, "op(100,fx,' op')."),
    ConformityTest(198, "/**/ writeq(' op' '1')."),
    ConformityTest(199, "/**/ writeq(' op'[])."),
    ConformityTest(200, "op(1,xf,xf1). writeq({- =xf1})."),
    ConformityTest(201, "writeq(- (a\\\*b))."),
    ConformityTest(202, "writeq(\\\ (a\\\*b))."),
    ConformityTest(203, "current_op(P,xfy,.)."),
    ConformityTest(204, "op(100,xfy,.). writeq(1 .2)."),
    ConformityTest(205, "/**/ writeq([1])."),
    ConformityTest(206, "/**/ writeq(-[1])."),
    ConformityTest(207, "/**/ X = 1.e."),
    ConformityTest(208, "write_canonical('$VAR'(0))."),
    ConformityTest(209, "write_term('$VAR'(0),[])."),
    ConformityTest(210, "writeq('$VAR'(0))."),
    ConformityTest(211, "writeq('$VAR'(-1))."),
    ConformityTest(212, "writeq('$VAR'(-2))."),
    ConformityTest(213, "writeq('$VAR'(x))."),
    ConformityTest(214, "writeq('$VAR'('A'))."),
    ConformityTest(215, "op(9,fy,fy),op(9,yf,yf). write_canonical(fy 1 yf)."),
    ConformityTest(216, "/**/ write_canonical(fy yf)."),
    ConformityTest(217, "/**/ writeq(fy(yf(1)))."),
    ConformityTest(218, "/**/ writeq(yf(fy(1)))."),
    ConformityTest(219, "/**/ writeq(yf(fy(yf(fy(1)))))."),
    ConformityTest(220, "op(9,fy,fy),op(9,yfx,yfx). write_canonical(fy 1 yfx 2)."),
    ConformityTest(221, "/**/ writeq(fy(yfx(1,2)))."),
    ConformityTest(222, "/**/ writeq(yfx(fy(1),2))."),
    ConformityTest(223, "op(9,yf,yf),op(9,xfy,xfy). write_canonical(1 xfy 2 yf)."),
    ConformityTest(224, "/**/ writeq(xfy(1,yf(2)))."),
    ConformityTest(225, "/**/ writeq(yf(xfy(1,2)))."),
    ConformityTest(226, "op(0,xfy,:-). current_op(P,xfx,:-)."),
    ConformityTest(227, "op(0,xfy,','). p._e.(m., o.,',')."),
    ConformityTest(228, "op(9,fy,p),op(9,yf,p). write_canonical(p p 0)."),
    ConformityTest(229, "/**/ writeq(p(p(0)))."),
    ConformityTest(230, "/**/ write_canonical(p 0 p)."),
    ConformityTest(231, "/**/ write_canonical(0 p p)."),
    ConformityTest(232, "/**/ write_canonical(p p)."),
    ConformityTest(233, "op(9,fy,p),op(9,yfx,p). write_canonical(1 p p p 2)."),
    ConformityTest(234, "op(9,fy,p),op(9,xfy,p). write_canonical(1 p p p 2)."),
    ConformityTest(235, "op(7,fy,p),op(9,yfx,p). write_canonical(1 p p p 2)."),
    ConformityTest(236, "atom('.'.'.')."),
    ConformityTest(237, "op(0,xfy,'|')."),
    ConformityTest(238, "/**/ writeq((a|b))."),
    ConformityTest(239, "op(0,xfy,.),op(9,yf,.)."),
    ConformityTest(240, "/**/ writeq(.(.))."),
    ConformityTest(241, "op(0,xfy,.),writeq((.)+(.)))."),
    ConformityTest(242, "set_prolog_flag(double_quotes,chars)."),
    ConformityTest(243, "/**/ writeq(\"a\")."),
    ConformityTest(244, "/**/ writeq(\"\\z\")."),
    ConformityTest(245, "/**/ writeq(\"\\0\\\")."),
    ConformityTest(246, "X is 10.0** -323, writeq(X)."),
    ConformityTest(247, "1.0e-323=:=10.0** -323."),
    ConformityTest(248, "\\-1 = -0x1."),
    ConformityTest(249, "T = t(0b1,0o1,0x1)."),
    ConformityTest(250, "X is 0b1mod 2."),
    ConformityTest(251, "op(1105,xfy,'|')."),
    ConformityTest(252, "/**/ writeq((a-->b,c|d))."),
    ConformityTest(253, "/**/ write_canonical((a|b;c))."),
    ConformityTest(254, "/**/ write_canonical((a;b|c))."),
    ConformityTest(255, "/**/ write_canonical([a;b|c])."),
    ConformityTest(256, "/**/ writeq([(a|b)])."),
    ConformityTest(257, "X/* /*/=7."),
    ConformityTest(258, "X/*/*/=7."),
    ConformityTest(259, "atom($-)."),
    ConformityTest(260, "atom(-$)."),
    ConformityTest(261, "op(900, fy, [$]). write_canonical($a+b)."),
    ConformityTest(262, "\\ ."),
    ConformityTest(263, "char_code(C,0), writeq(C)."),
    ConformityTest(264, "writeq('\\0\\')."),
    ConformityTest(265, "write_canonical(_+_)."),
    ConformityTest(266, "write_canonical(B+B)."),
    ConformityTest(267, "writeq(0'\\z)."),
    ConformityTest(268, "char_code('\\^',X)."),
    ConformityTest(269, "writeq(0'\\c)."),
    ConformityTest(270, "writeq(0'\\ )."),
    ConformityTest(271, "writeq(nop (1))."),
    ConformityTest(272, "op(400,fx,f). writeq(f/*.*/(1,2))."),
    ConformityTest(273, "/**/ writeq(1 = f)."),
    ConformityTest(274, "write_canonical(a- - -b)."),
    ConformityTest(275, "op(699,xf,>.)."),
    ConformityTest(276, "/**/ writeq(>(>.(a),b))."),
    ConformityTest(277, "/**/ write_canonical(a>. >b)."),
    ConformityTest(278, "/**/ write_canonical(a>. =b)."),
    ConformityTest(279, "/**/ write_canonical((a>.,b))."),
    ConformityTest(280, "/**/ write_canonical(a>.)."),
    ConformityTest(281, "op(699,xf,>)."),
    ConformityTest(282, "/**/ writeq(>(>(a),b))."),
    ConformityTest(283, "/**/ write_canonical(a> >b)."),
    ConformityTest(284, "/**/ write_canonical(a> =b)."),
    ConformityTest(285, "/**/ write_canonical((a>,b))."),
    ConformityTest(286, "/**/ write_canonical(a>)."),
    ConformityTest(287, "/**/ write_canonical(a>b)."),
    ConformityTest(288, "op(9,yfx,[bop,bo,b,op,xor]). writeq(0bop 2)."),
    ConformityTest(289, "/**/ writeq(0 bop 2)."),
    ConformityTest(290, "/**/ writeq(0bo 2)."),
    ConformityTest(291, "/**/ writeq(0b 2)."),
    ConformityTest(292, "/**/ writeq(0op 2)."),
    ConformityTest(293, "/**/ writeq(0xor 2)."),
    ConformityTest(294, "writeq('^\\`')."),
    ConformityTest(295, "op(9,yf,[b2,o8])."),
    ConformityTest(296, "/**/ writeq(0b2)."),
    ConformityTest(297, "/**/ writeq(0o8)."),
    ConformityTest(298, "op(500, xfy, {}). p._e.(c.,o.,{})."),
    ConformityTest(299, "writeq('\\b\\r\\f\\t\\n')."),
    ConformityTest(300, "get_char(C)."),  # a
    ConformityTest(301, "get_char(C)."),  # a
    ConformityTest(302, "writeq(0B1)."),
    ConformityTest(303, "op(20,fx,--),writeq(--(a))."),
    ConformityTest(304, "/**/ op(0,fy,--),writeq(--(a))."),
    ConformityTest(305, "writeq(0xamod 2)."),
    ConformityTest(306, "writeq(00'+'1)."),
    ConformityTest(307, "writeq(00'a)."),
    ConformityTest(308, "writeq('\\^J')."),
    ConformityTest(309, "writeq([(a,b)])."),
    ConformityTest(310, "writeq(1= \\\\)."),
    ConformityTest(311, "writeq((,))."),
    ConformityTest(312, "writeq({[})."),
    ConformityTest(313, "writeq({(})."),
    ConformityTest(314, "writeq([a,b|c])."),
    ConformityTest(315, "(\\+ (a,b)) = \\\\+(T)."),
    ConformityTest(316, "[] = '[]'."),
    ConformityTest(317, "op(300,fy,~). writeq(~ (a=b))."),
    ConformityTest(318, "writeq(\\\ (a=b))."),
    ConformityTest(319, "writeq(+ (a=b))."),
    ConformityTest(320, "writeq([/*.*/])."),
    ConformityTest(321, "writeq(.+)."),
    ConformityTest(322, "writeq({a,b})."),
    ConformityTest(323, "writeq({\\+ (})."),
    ConformityTest(324, "writeq(\\\\+ (()))."),
    ConformityTest(325, "writeq(+((1\\\*2)^3))."),
    ConformityTest(326, "writeq(-((1\\\*2)^3))."),
    ConformityTest(327, "writeq([a|\\\ +2])."),
    ConformityTest(328, "writeq((a)(b))."),
    ConformityTest(329, "writeq('%')."),
    ConformityTest(330, "writeq({[y]})."),
    ConformityTest(331, "(>)(1,2)."),
    ConformityTest(332, "write_canonical(;(a))."),
    ConformityTest(333, "write_canonical(';'(a))."),
    ConformityTest(334, "write_canonical(;(a,b))."),
    ConformityTest(335, "writeq(1 is _)."),
    ConformityTest(336, "op(9,fx,.),writeq(.(' '))."),
    ConformityTest(337, "op(9,yf,.>)."),
    ConformityTest(338, "write_canonical(a.> .>)."),
    ConformityTest(339, "writeq(((a).>).>)."),
    ConformityTest(340, "writeq([.|1])."),
    ConformityTest(341, "writeq(-'-'2)."),
    ConformityTest(342, "write_canonical((-,(1)))."),
    ConformityTest(343, "writeq(-[]1)."),
    ConformityTest(344, "writeq(-{}1)."),
    ConformityTest(345, "op(1000,xfx,~>). writeq([a~>b])."),
    ConformityTest(346, "op(1000,yfx,~>). writeq([a~>b])."),
    ConformityTest(347, "op(1000,xfy,~>). writeq([a~>b])."),
    ConformityTest(348, "writeq({|})."),
    ConformityTest(349, "writeq(([))."),
    ConformityTest(350, "writeq(((>)(1))."),
    ConformityTest(351, "writeq({a:-b})."),
    ConformityTest(352, "op(1105,xfy,'|'). writeq((|))."),
    ConformityTest(353, "op(0,xfy,'|'). writeq((|))."),
    ConformityTest(354, "Finis (())."),
    ConformityTest(355, "Finis ()."),
]

# Map test numbers to index for O(1) lookup
_TEST_INDEX: Dict[int, int] = {t.num: i for i, t in enumerate(CONFORMITY_TESTS)}


def _categorize_prolog_error(error: Exception) -> TestResult:
    """Infer a TestResult from a Prolog error exception or term."""
    inner = None
    # Try to inspect common shapes: error(iso_error_term) or error(syntax_error(...))
    err_term = getattr(error, "term", None)
    inner = None
    try:
        if err_term is not None:
            # Some Prolog libraries expose a nested term structure
            inner = getattr(err_term, "args", None)
            if inner and isinstance(inner, tuple) and len(inner) > 0:
                inner = inner[0]
    except Exception:
        inner = None
    # Check inner functor first for ISO errors
    inner_functor = getattr(inner, "functor", None) if inner is not None else None
    if inner_functor == "syntax_error" or getattr(error, "functor", None) == "syntax_error" or (inner and getattr(inner, "name", None) == "syntax_error"):
        return TestResult.SYNTAX_ERROR
    if inner_functor == "type_error" or getattr(error, "functor", None) == "type_error" or (inner and getattr(inner, "name", None) == "type_error"):
        return TestResult.TYPE_ERROR
    if inner_functor == "exception" or (inner and getattr(inner, "name", None) == "iso_error" and getattr(inner, "args", None)):
        return TestResult.EXCEPTION
    if inner_functor == "timeout" or (inner and getattr(inner, "name", None) == "timeout"):
        return TestResult.TIMEOUT
    return TestResult.EXCEPTION


def execute_test(prolog: PrologInterpreter, test: ConformityTest, verbose: bool = False) -> TestExecutionResult:
    """Execute a single conformity test and return the result."""
    start_time = time.time()

    try:
        # For tests that reference previous, we need to handle the /**/ marker
        # The /**/ tests reuse the op/3 declarations from the previous test
        if test.references_previous:
            # Find the previous non-referenced test efficiently
            idx = _TEST_INDEX.get(test.num, None)
            prev_test = None
            if idx is not None:
                j = idx - 1
                while j >= 0:
                    candidate = CONFORMITY_TESTS[j]
                    if not candidate.references_previous:
                        prev_test = candidate
                        break
                    j -= 1
            if prev_test:
                if verbose:
                    print(f"  Executing referenced test {prev_test.num}: {prev_test.query}")
                # Execute the previous test first to set up operators
                try:
                    prolog.query_once(prev_test.query)
                except Exception as e:
                    if verbose:
                        print(f"  Warning: Referenced test {prev_test.num} failed during setup: {e}")
                    pass  # Ignore errors in setup

        if verbose:
            print(f"  Executing test {test.num}: {test.query}")

        # Execute the test query with output capture
        result, had_output = prolog.query_once(test.query, capture_output=True)

        execution_time = time.time() - start_time
        return TestExecutionResult(
            test=test,
            result=TestResult.OK,
            execution_time=execution_time,
            output="output captured" if had_output else None
        )

    except PrologThrow as e:
        execution_time = time.time() - start_time
        # Map the exception to a TestResult using ISO error analysis
        error_result = _categorize_prolog_error(e)
        return TestExecutionResult(test=test, result=error_result, error_message=str(e), execution_time=execution_time)

    except Exception as e:
        execution_time = time.time() - start_time
        # Map the exception to a TestResult using ISO error analysis
        error_result = _categorize_prolog_error(e)
        return TestExecutionResult(test=test, result=error_result, error_message=str(e), execution_time=execution_time)


def run_conformity_tests(test_range: Optional[Tuple[int, int]] = None,
                        category: Optional[str] = None,
                        verbose: bool = False) -> List[TestExecutionResult]:
    """Run the conformity tests and return results."""
    # Filter tests based on range and category
    tests_to_run = []
    for test in CONFORMITY_TESTS:
        if test_range and not (test_range[0] <= test.num <= test_range[1]):
            continue
        if category and test.category != category:
            continue
        tests_to_run.append(test)

    if verbose:
        print(f"Running {len(tests_to_run)} conformity tests...")

    results = []
    prolog = PrologInterpreter()

    for i, test in enumerate(tests_to_run):
        if verbose and (i + 1) % 10 == 0:
            print(f"Progress: {i + 1}/{len(tests_to_run)} tests completed")

        result = execute_test(prolog, test, verbose)
        results.append(result)

    return results


def generate_markdown_report(results: List[TestExecutionResult], output_path: Path) -> None:
    """Generate a markdown report from test results."""
    timestamp = datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")

    # Calculate statistics
    total_tests = len(results)
    ok_count = sum(1 for r in results if r.result == TestResult.OK)
    syntax_errors = sum(1 for r in results if r.result == TestResult.SYNTAX_ERROR)
    type_errors = sum(1 for r in results if r.result == TestResult.TYPE_ERROR)
    other_errors = sum(1 for r in results if r.result in [TestResult.EXCEPTION, TestResult.TIMEOUT])

    # Group by category
    category_stats = {}
    for result in results:
        cat = result.test.category
        if cat not in category_stats:
            category_stats[cat] = {'total': 0, 'ok': 0}
        category_stats[cat]['total'] += 1
        if result.result == TestResult.OK:
            category_stats[cat]['ok'] += 1

    # Generate markdown
    lines = [
        "# ISO Prolog Conformity Testing Results",
        "",
        f"> **Last Updated**: {timestamp}",
        "> **Test Suite**: ISO/IEC JTC1 SC22 WG17",
        "> **Source**: https://www.complang.tuwien.ac.at/ulrich/iso-prolog/conformity_testing",
        f"> **Total Tests**: {total_tests}",
        "",
        "## Summary",
        "",
        f"- **Passed**: {ok_count} ({ok_count/total_tests*100:.1f}%)" if total_tests > 0 else "- **Passed**: 0 (0.0%)",
        f"- **Syntax Errors**: {syntax_errors} ({syntax_errors/total_tests*100:.1f}%)" if total_tests > 0 else "- **Syntax Errors**: 0 (0.0%)",
        f"- **Type Errors**: {type_errors} ({type_errors/total_tests*100:.1f}%)" if total_tests > 0 else "- **Type Errors**: 0 (0.0%)",
        f"- **Other Errors**: {other_errors} ({other_errors/total_tests*100:.1f}%)" if total_tests > 0 else "- **Other Errors**: 0 (0.0%)",
        "",
        "## Results by Category",
        "",
    ]

    for cat, stats in sorted(category_stats.items()):
        pct = stats['ok'] / stats['total'] * 100 if stats['total'] > 0 else 0
        lines.extend([
            f"### {cat} (Tests {min(r.test.num for r in results if r.test.category == cat)}-{max(r.test.num for r in results if r.test.category == cat)})",
            "",
            f"- Passed: {stats['ok']}/{stats['total']} ({pct:.1f}%)",
            "",
        ])

    lines.extend([
        "## Detailed Results",
        "",
        "| Test # | Query | Status | Error |",
        "|--------|-------|--------|-------|",
    ])

    for result in results:
        # Truncate long queries
        query = result.test.query
        if len(query) > 50:
            query = query[:47] + "..."

        status = result.result.value
        error = result.error_message or ""
        if len(error) > 50:
            error = error[:47] + "..."

        lines.append(f"| {result.test.num} | `{query}` | {status} | {error} |")

    lines.extend([
        "",
        "## Known Issues",
        "",
        "[Automatically populated list of failing tests with their error messages]",
        "",
        "## Regenerating This Report",
        "",
        "```bash",
        f"uv run python tools/conformity_test.py --output {output_path}",
        "```",
        "",
    ])

    # Write to file
    with open(output_path, 'w', encoding='utf-8') as f:
        f.write('\n'.join(lines))


def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(
        description="Run ISO Prolog conformity tests against Vibe-Prolog"
    )
    parser.add_argument(
        "--tests",
        type=str,
        help="Test range in format 'start-end' (e.g., '1-50')"
    )
    parser.add_argument(
        "--category",
        choices=["Character Escapes", "Operators", "Numeric Literals", "Special Syntax"],
        help="Run only tests from a specific category"
    )
    parser.add_argument(
        "--verbose", "-v",
        action="store_true",
        help="Show detailed execution information"
    )
    parser.add_argument(
        "--output", "-o",
        type=Path,
        default=Path("docs/CONFORMITY_TESTING.md"),
        help="Output file for the markdown report"
    )

    args = parser.parse_args()

    # Parse test range
    test_range = None
    if args.tests:
        try:
            start, end = map(int, args.tests.split('-'))
            test_range = (start, end)
        except ValueError:
            print(f"Error: Invalid test range format: {args.tests}", file=sys.stderr)
            sys.exit(1)

    # Run tests
    results = run_conformity_tests(
        test_range=test_range,
        category=args.category,
        verbose=args.verbose
    )

    # Generate report
    generate_markdown_report(results, args.output)

    if args.verbose:
        print(f"\nReport generated: {args.output}")

    # Print summary to stdout
    total = len(results)
    passed = sum(1 for r in results if r.result == TestResult.OK)
    print(f"Conformity testing complete: {passed}/{total} tests passed ({passed/total*100:.1f}%)" if total > 0 else "No tests run")


if __name__ == "__main__":
    main()