:- discontiguous [attribute/3,rdel1/2,rdel2/3].

% Golog interpreter
:- [golog_swi].

% actions

primitive_action(edit1(X, Vnew)).
primitive_action(edit2(X, V, Vnew)).

primitive_action(del1(X)).
primitive_action(del2(X, V)).

% preconditions

poss(edit1(X, Vnew), S) :- attribute(X, _V, S).
poss(edit2(X, V, Vnew), S) :- attribute(X, V, S).

poss(del1(X), S) :- attribute(X, _V, S).
poss(del2(X, V), S) :- attribute(X, V, S).

% successor-state axioms

attribute(X, V, do(A, S)) :-
              (attribute(X, V, S), \+ (
                            A = del1(X); A = del2(X, V);
                            (\+ (V2 = V), (A = edit1(X, V2) ; A = edit2(X, V, V2)))
                            ));
              (attribute(X, Vold, S), (A = edit1(X, V); A = edit2(X, Vold, V))).

% restoreSitArg(attribute(X,V),S,attribute(X,V,S)). % Not necessary?


% Knowledge Base

attribute(a, 2, s0).
attribute(b, "123", s0).
attribute(c, 45, s0).
attribute(d, "p", s0).


:- do(del1(b), s0, S1), do(del2(d, "p"), S2, SN), attribute(X, V, SN).
% :- do(del1(b), s0, S1), do(del2(c, 43), S1, SN), do(del2(d, "p"), S2, SN), attribute(X, V, SN). % Doesn't work because del2(c, 43) can't be executed.

