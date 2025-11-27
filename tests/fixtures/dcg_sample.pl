% DCG fixtures: simple grammar for balanced parentheses
% Tests definite clause grammars

s --> [].
s --> ['('], s, [')'], s.

% Queries:
% ?- phrase(s, ['(', '(', ')', ')']). % true
% ?- phrase(s, ['(', ')']). % true
% ?- phrase(s, ['(', ')', '(']). % false