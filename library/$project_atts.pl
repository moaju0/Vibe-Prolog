/**  Residual Goal Projection for Attributed Variables
 
    This is an internal module that handles projection of attributed variable
    constraints into residual goals. It is used by library(iso_ext) for copy_term/3
    and by the toplevel for displaying constraint answers.
 
    Predicates:
    
    - term_residual_goals(+Term, -Goals)
      Collect all residual goals from attributed variables in Term.
      
    - project_attributes(+QueryVars, +AttrVars)
      Project constraints from AttrVars onto QueryVars.
 
    The attribute_goals//1 hook:
    
    Modules that define attributes should export attribute_goals//1 DCG
    to describe how their attributes should be displayed:
    
        attribute_goals(Var) -->
            { get_atts(Var, myattr(Value)) },
            [mymodule:myattr(Var, Value)].
*/

:- module('$project_atts', [
    term_residual_goals/2,
    project_attributes/2
]).

% Note: These predicates are implemented as built-ins in Python.
% The library file provides the module interface for use_module/1.
