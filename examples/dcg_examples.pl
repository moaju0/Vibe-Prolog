%% DCG Examples for Vibe-Prolog
%% This file demonstrates various uses of Definite Clause Grammars (DCGs)

%% ===================================================================
%% 1. Simple Terminal Matching
%% ===================================================================

%% Basic terminal matching
hello --> [hello].
world --> [world].

%% Sequence of terminals
hello_world --> hello, world.

%% Alternative terminals
greeting --> [hello] ; [hi] ; [hey].

%% ===================================================================
%% 2. Simple Sentence Parser
%% ===================================================================

%% Basic sentence structure: article + noun + verb + article + noun
article --> [the] ; [a].
noun --> [cat] ; [dog] ; [house] ; [car].
verb --> [chases] ; [sees] ; [likes].

sentence --> article, noun, verb, article, noun.

%% Test: phrase(sentence, [the, cat, chases, a, dog]).

%% ===================================================================
%% 3. Arithmetic Expression Parser
%% ===================================================================

%% Simple arithmetic expressions (simplified)
%% Note: Full expression parsing with precedence is complex
%% This is a basic example

%% Simple addition: X + Y
add_expr(Result) --> [X], ['+'], [Y], {integer(X), integer(Y), Result is X + Y}.

%% Test: phrase(add_expr(R), [2, '+', 3]).  % R = 5

%% ===================================================================
%% 4. List Processing with DCGs
%% ===================================================================

%% Parse a list of integers
int_list([]) --> [].
int_list([H|T]) --> [H], {integer(H)}, int_list(T).

%% Parse comma-separated values (simple CSV)
csv_row([]) --> [].
csv_row([H|T]) --> [H], {atom(H)}, ( [','], csv_row(T) ; {T = []} ).

%% Test: phrase(csv_row(Row), [name, ',', age, ',', city]).

%% ===================================================================
%% 5. Recursive Structures
%% ===================================================================

%% Balanced parentheses
parens --> [].
parens --> ['('], parens, [')'], parens.

%% Test: phrase(parens, ['(', '(', ')', ')']).

%% Simple language for nested expressions
expr_lang --> number ; ['('], expr_lang, op, expr_lang, [')'].
op --> ['+'] ; ['-'] ; ['*'] ; ['/'].

%% ===================================================================
%% 6. DCG with Parameters
%% ===================================================================

%% Repetition: rep(N, Char) matches N occurrences of Char
rep(0, _) --> [].
rep(N, C) --> [C], {N > 0, M is N - 1}, rep(M, C).

%% Test: phrase(rep(3, x), [x, x, x]).

%% ===================================================================
%% 7. Advanced Features
%% ===================================================================

%% DCG with embedded Prolog goals
word(W) --> [W], {atom(W), atom_length(W, L), L > 0}.

%% Sentence with validation
valid_sentence --> article, noun, verb, {true}.  %% Could add semantic checks here

%% Alternative parsing with cut
animal --> [cat], ! ; [dog] ; [bird].

%% ===================================================================
%% 8. JSON Parser (Simplified)
%% ===================================================================

%% Note: This is a very basic JSON parser for demonstration
%% A full JSON parser would be much more complex

%% JSON value
json_value(V) --> json_string(V) ; json_number(V) ; json_object(V) ; json_array(V) ; json_bool(V) ; json_null.

%% JSON string (simplified - no escapes)
json_string(S) --> ['"'], string_chars(Chars), ['"'], {atom_codes(S, Chars)}.

string_chars([]) --> [].
string_chars([H|T]) --> [H], {H \= '"'}, string_chars(T).

%% JSON number (simplified)
json_number(N) --> [N], {number(N)}.

%% JSON boolean
json_bool(true) --> [true].
json_bool(false) --> [false].

%% JSON null
json_null(null) --> [null].

%% JSON object (simplified - single key-value pair)
json_object(obj(K,V)) --> ['{'], ['"'], string_chars(KeyChars), ['"'], [':'], json_value(V), ['}'], {atom_codes(K, KeyChars)}.

%% JSON array (simplified - fixed size)
json_array(arr(A,B)) --> ['['], json_value(A), [','], json_value(B), [']'].

%% Test: phrase(json_value(Result), ['"', h, e, l, l, o, '"']).

%% ===================================================================
%% Usage Examples
%% ===================================================================

%% To test these DCGs, use phrase/2 or phrase/3:
%%
%% ?- phrase(hello_world, [hello, world]).
%% true.
%%
%% ?- phrase(sentence, S).
%% S = [the, cat, chases, a, dog] ;
%% S = [the, cat, chases, a, house] ;
%% etc.
%%
%% ?- phrase(add_expr(R), [2, '+', 3]).
%% R = 5.
%%
%% ?- phrase(rep(3, x), List).
%% List = [x, x, x].