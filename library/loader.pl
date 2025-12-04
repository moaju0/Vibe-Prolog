/** Loader utilities for module handling.

This module provides predicates for handling module-qualified terms,
compatible with Scryer-Prolog's library(loader).
*/

:- module(loader, [strip_module/3]).

%% strip_module(+Term, -Module, -Goal)
%
% Extracts the module qualifier from a term.
% If Term is Module:Goal, unifies Module and Goal accordingly.
% If Term has no module qualifier, Module is unified with 'user'.
%
% Examples:
%   ?- strip_module(lists:append(A,B,C), M, G).
%   M = lists, G = append(A,B,C).
%
%   ?- strip_module(foo(X), M, G).
%   M = user, G = foo(X).

strip_module(M:G0, M, G) :- !, strip_module(G0, _, G).
strip_module(G, user, G).
