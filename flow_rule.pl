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

% attribute(X, do(A, S)) :- attribute(X, S), \+ (A = del(X)).

attribute(X, V, do(A, S)) :-
              (attribute(X, V, S), \+ (
                            A = del1(X); A = del2(X, V);
                            (\+ (V2 = V), (A = edit1(X, V2) ; A = edit2(X, V, V2)))
                            ));
              (attribute(X, Vold, S), (A = edit1(X, V); A = edit2(X, Vold, V))).

%% helper to turn sequence of action to unordered fluents (predicates)
rdel1(X, do(A, S)) :- rdel1(X, S), \+ (A = del1(X)).
rdel2(X, V, do(A, S)) :- rdel2(X, V, S), \+ (A = del2(X, V)).
redit1(X, Vnew, do(A, S)) :- redit1(X, Vnew, S), \+ (A = edit1(X, Vnew)).
redit2(X, V, Vnew, do(A, S)) :- redit2(X, V, Vnew, S), \+ (A = edit2(X, V, Vnew)).

% Golog specific

proc(doDel1, pi(x, ?(rdel1(x)) : del1(x))).
proc(doAllDel1, while(some(x, rdel1(x)), doDel1)).
proc(doDel2, pi(x, pi(v, ?(rdel2(x, v)) : del2(x, v)))).
% proc(doAllDel2, while(some(x, some(v, rdel2(x, v)& poss(del2(x, v)))), doDel2)).
proc(doAllDel2, while(some(x, some(v, rdel2(x, v))), doDel2)).
% proc(doDel, pi(x, ?(rdel1(x)) : del1(x))).
% proc(doEdit, pi(x, ?(redit(x)) : edit(x))).
% proc(propagate, while(some(x, redit(x)), doEdit) : while(some(x, rdel(x)), doDel)).
proc(propagate, doAllDel1 : doAllDel2).

% restoreSitArg(attribute(X,V),S,attribute(X,V,S)). % Not necessary?
restoreSitArg(rdel1(X),S,rdel1(X,S)).
restoreSitArg(rdel2(X,V),S,rdel2(X,V,S)).
restoreSitArg(redit1(X,Vnew),S,redit1(X,Vnew,S)).
restoreSitArg(redit2(X,V,Vnew),S,redit2(X,V,Vnew,S)).


% Knowledge Base

attribute(a, 2, s0).
attribute(b, "123", s0).
attribute(c, 45, s0).
attribute(d, "p", s0).

rdel1(b, s0).
rdel2(c, 43, s0). %TODO: works without this, probably because this rdel2 can not be done
rdel2(d, "p", s0).

% :- forall((do(propagate, s0, S), attribute(X, S)), (write(S), write(":  "), write(X), nl)).

% :- do(doAllDel1, s0, S).
% :- do(doAllDel2, s0, S).

:- do(del1(b), s0, S1), do(del2(c, 43), S1, SN), do(del2(d, "p"), S2, SN), attribute(X, V, SN). % Doesn't work because del2(c, 43) can't be executed.

