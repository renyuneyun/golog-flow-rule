:- discontiguous [attribute/2,rdel/2].

% Golog interpreter
:- [golog_swi].

% actions

primitive_action(del(X)).

% preconditions

poss(del(X), S) :- attribute(X, S).

% successor-state axioms

attribute(X, do(A, S)) :- attribute(X, S), \+ (A = del(X)).

% restoreSitArg(attribute(X,V),S,attribute(X,V,S)). % Not necessary?


% Knowledge Base

attribute(a, s0).
attribute(b, s0).
attribute(c, s0).
attribute(d, s0).

:- do(del(b), s0, S1), do(del(d), S1, Sn), attribute(X, Sn).

