:- discontiguous [attribute/3,rdel1/2,rdel2/3].

% Golog interpreter
:- [golog_swi].

% actions

primitive_action(edit(X, V, Vnew)).

primitive_action(del(X, V)).

% preconditions

poss(edit(X, *, Vnew), S) :- attribute(X, _V, S).
poss(edit(X, V, Vnew), S) :- attribute(X, V, S).

poss(del(X, *), S) :- attribute(X, _V, S).
poss(del(X, V), S) :- attribute(X, V, S).

% successor-state axioms

attribute(X, V, do(A, S)) :-
              attribute(X, V, S), \+ (
                            A = del(X, *); A = del(X, V);
                            (\+ (V2 = V), (A = edit(X, *, V2) ; A = edit(X, V, V2)))
                            )
                            .

attribute(X, V, do(A, S)) :-
              attribute(X, Vold, S), (A = edit(X, *, V); A = edit(X, Vold, V))
              .

% restoreSitArg(attribute(X,V),S,attribute(X,V,S)). % Not necessary?


% Knowledge Base

attribute(a, 2, s0).
attribute(b, "123", s0).
attribute(b, "987", s0).
attribute(c, 45, s0).
attribute(d, "p", s0).


output(S) :-
              forall(attribute(X, V, S), writeln([X, V]))
              .

:- do(del(b, *), s0, S1), do(del(d, "p"), S1, SN), output(SN).
% :- do(del1(b), s0, S1), do(del2(c, 43), S1, SN), do(del2(d, "p"), S2, SN), attribute(X, V, SN). % Doesn't work because del2(c, 43) can't be executed.

