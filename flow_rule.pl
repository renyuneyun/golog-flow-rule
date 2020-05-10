:- discontiguous [attribute/3,rdel1/2,rdel2/3].

% Golog interpreter
:- [golog_swi].

% actions

primitive_action(edit(X, V, Vnew)).
primitive_action(edit(X, _, Vnew)).

primitive_action(del(X, V)).
primitive_action(del(X, _)).

% preconditions

poss(edit(X, _, Vnew), S) :- attribute(X, _V, S).
poss(edit(X, V, Vnew), S) :- attribute(X, V, S).

poss(del(X, _), S) :- attribute(X, _V, S).
poss(del(X, V), S) :- attribute(X, V, S).

% successor-state axioms

attribute(X, V, do(A, S)) :-
              attribute(X, V, S), \+ (
                            A = del(X, V);
                            (( A = edit(X, V, V2)), \+ (V2 = V))  %%% I removed A=edit(X,_,V2) because doing so gives seemingly correct result. I'm not sure if this is always true or not.
                            )
                            .

attribute(X, V, do(A, S)) :-
              attribute(X, Vold, S), ( A = edit(X, Vold, V))
              .

% restoreSitArg(attribute(X,V),S,attribute(X,V,S)). % Not necessary?


% Knowledge Base

attribute(a, 2, s0).
attribute(b, "123", s0).
attribute(b, "987", s0).
attribute(c, 45, s0).
attribute(d, "p", s0).


output(S) :-
              writeln(''),
              forall(attribute(X, V, S), writeln([S, X, V]))
              .

:- do(del(b, _), s0, S1), do(del(d, "p"), S1, SN), output(SN).
:- do(del(b, _), s0, S1), do(edit(c, _, 99), S1, SN), output(SN).
:- do(del(b, _), s0, S1), do(edit(c, _, 99), S1, S2), do(del(d, "p"), S2, SN), output(SN).
% :- do(del1(b), s0, S1), do(del2(c, 43), S1, SN), do(del2(d, "p"), S2, SN), attribute(X, V, SN). % Doesn't work because del2(c, 43) can't be executed.

