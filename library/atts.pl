/*  Part of Vibe-Prolog

    Author:        Vibe-Prolog Team
    
    This library provides attributed variables support (SICStus/Scryer style).
    
    Attributed variables are the foundation for constraint logic programming.
    They allow metadata to be attached to unbound variables, and hooks to be
    called when those variables are unified.

    Usage:
    
        :- use_module(library(atts)).
        :- attribute myattr/1.
        
        % Set an attribute on a variable
        ?- put_atts(X, +myattr(value)).
        
        % Query an attribute
        ?- put_atts(X, +myattr(V)), get_atts(X, myattr(V)).
        
        % Check if attribute is present
        ?- put_atts(X, +myattr(value)), get_atts(X, +myattr(_)).
        
        % Check if attribute is absent
        ?- get_atts(X, -myattr(_)).
        
        % Test for attributed variable
        ?- put_atts(X, +myattr(value)), attvar(X).

    The verify_attributes/3 hook:
    
        When an attributed variable is unified with a non-variable term,
        the system calls verify_attributes/3 for each module that has
        attributes on the variable:
        
            verify_attributes(Var, Value, Goals)
        
        - Var: The attributed variable being unified
        - Value: The value it's being unified with  
        - Goals: Output - list of goals to execute after unification
        
        If verify_attributes/3 fails, the unification fails.
        The goals returned are executed; if any fail, unification fails.
*/

:- module(atts, [
    put_atts/2,
    get_atts/2,
    attvar/1,
    term_attvars/2,
    copy_term/3,
    del_atts/1
]).

% Note: These predicates are implemented as built-ins in Python.
% The library file provides the module interface for use_module/1.
