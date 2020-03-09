:- discontiguous [attribute/2,rdel/2].

% Golog interpreter
:- [golog_swi].

% actions

primitive_action(del(X)).

% preconditions

poss(del(X), S) :- attribute(X, S).

% successor-state axioms

attribute(X, do(A, S)) :- attribute(X, S), \+ (A = del(X)).

%% helper to turn sequence of action to unordered fluents (predicates)
rdel(X, do(A, S)) :- rdel(X, S), \+ (A = del(X)).

% Golog specific

proc(doDel, pi(x, ?(rdel(x)) : del(x))).
proc(propagate, while(some(x, rdel(x)), doDel)).

% restoreSitArg(attribute(X,V),S,attribute(X,V,S)). % Not necessary?
restoreSitArg(rdel(X),S,rdel(X,S)).


% Knowledge Base

attribute(a, s0).
attribute(b, s0).
attribute(c, s0).
attribute(d, s0).

rdel(b, s0).
rdel(d, s0).

% :- forall((do(propagate, s0, S), attribute(X, S)), (write(S), write(":  "), write(X), nl)).
:- do(propagate, s0, S), attribute(X, S).

