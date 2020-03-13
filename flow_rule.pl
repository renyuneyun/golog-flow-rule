:- discontiguous attr0/4.
:- discontiguous attr1/5.

% Golog interpreter
:- [golog_swi].

% actions

primitive_action(pr(Pin, Pout)).

primitive_action(edit1(X, Vnew)).
primitive_action(edit2(X, Vnew, Pout)).
primitive_action(edit3(X, Vnew, Pin)).
primitive_action(edit4(X, Vnew, Pin, Pout)).
primitive_action(edit5(X, V, Vnew)).
primitive_action(edit6(X, V, Vnew, Pout)).
primitive_action(edit7(X, V, Vnew, Pin)).
primitive_action(edit8(X, V, Vnew, Pin, Pout)).

primitive_action(del1(X)).
primitive_action(del2(X, Pout)).
primitive_action(del3(X, Pin)).
primitive_action(del4(X, Pin, Pout)).
primitive_action(del5(X, V)).
primitive_action(del6(X, V, Pout)).
primitive_action(del7(X, V, Pin)).
primitive_action(del8(X, V, Pin, Pout)).

% preconditions
poss(pr(Pin, Pout), S) :- attr0(X, V, Pin, S), !.

poss(edit1(X, Vnew), S) :- attr1(X, V, Pin, Pout, S), !.
poss(edit2(X, Vnew, Pout), S) :- attr1(X, V, Pin, Pout, S), !.
poss(edit3(X, Vnew, Pin), S) :- attr1(X, V, Pin, Pout, S), !.
poss(edit4(X, Vnew, Pin, Pout), S) :- attr1(X, V, Pin, Pout, S), !.
poss(edit5(X, V, Vnew), S) :- attr1(X, V, Pin, Pout, S), !.
poss(edit6(X, V, Vnew, Pout), S) :- attr1(X, V, Pin, Pout, S), !.
poss(edit7(X, V, Vnew, Pin), S) :- attr1(X, V, Pin, Pout, S), !.
poss(edit8(X, V, Vnew, Pin, Pout), S) :- attr1(X, V, Pin, Pout, S), !.

poss(del1(X), S) :- attr1(X, V, Pin, Pout, S), !.
poss(del2(X, Pout), S) :- attr1(X, V, Pin, Pout, S), !.
poss(del3(X, Pin), S) :- attr1(X, V, Pin, Pout, S), !.
poss(del4(X, Pin, Pout), S) :- attr1(X, V, Pin, Pout, S), !.
poss(del5(X, V), S) :- attr1(X, V, Pin, Pout, S), !.
poss(del6(X, V, Pout), S) :- attr1(X, V, Pin, Pout, S), !.
poss(del7(X, V, Pin), S) :- attr1(X, V, Pin, Pout, S), !.
poss(del8(X, V, Pin, Pout), S) :- attr1(X, V, Pin, Pout, S), !.

% successor-state axioms

% Formula (25), where v_{new} and v are replaced with v and v_{old}
attr1(X, V, Pin, Pout, do(A, S)) :-
              (attr1(X, V, Pin, Pout, S), \+ (
                            A = del1(X); A = del2(X, Pout); A = del3(X, Pin); A = del4(X, Pin, Pout);
                                          A = del5(X, V); A = del6(X, V, Pout); A = del7(X, V, Pin); A = del8(X, V, Pin, Pout);
                            (\+ (V2 = V), (
                                          A = edit1(X, V2); A = edit2(X, V2, Pout); A = edit3(X, V2, Pin); A = edit4(X, V2, Pin, Pout);
                                          A = edit5(X, V, V2); A = edit6(X, V, V2, Pout); A = edit7(X, V, V2, Pin); A = edit8(X, V, V2, Pin, Pout)
                                          )
                            )
                            )
              );
              (attr1(X, Vold, Pin, Pout, S), (
                                          A = edit1(X, V); A = edit2(X, V, Pout); A = edit3(X, V, Pin); A = edit4(X, V, Pin, Pout);
                                          A = edit5(X, Vold, V); A = edit6(X, Vold, V, Pout); A = edit7(X, Vold, V, Pin); A = edit8(X, Vold, V, Pin, Pout)
                            )
              );
              (attr0(X, V, Pin, S), A = pr(Pin, Pout))
                            .

attr0(X, V, Pin, do(A, S)) :- attr0(X, V, Pin, S).

% restoreSitArg(attribute(X,V),S,attribute(X,V,S)). % Not necessary?
restoreSitArg(attr0(X,V,Pin),S,attr0(X,V,Pin,S)). % Not necessary?


% Knowledge Base

attr0(a, 2, pi1, s0).
attr0(b, "123", pi1, s0).
attr0(c, 45, pi1, s0).
attr0(d, "p", pi2, s0).
attr0(e, "k", pi2, s0).


:- do(pr(pi2, po1), s0, S4), do(edit5(d, "p", "m"), S4, SN), attr1(d, V, Pin, Pout, SN).
% :- do(pr(pi1, po1), s0, S1), do(pr(pi2, po1), S1, S2), do(pr(pi2, po2), S2, S3), do(del1(b), S3, S4), do(del6(d, "p", po1), S4, SN), attr1(X, V, Pin, Pout, SN).
% :- do(pr(pi1, po1), s0, S1), do(pr(pi2, po1), S1, S2), do(pr(pi2, po2), S2, S3), do(del1(b), S3, S4), do(edit5(d, "p", "m"), S4, SN), attr1(X, V, Pin, Pout, SN).
% :- do(del1(b), s0, S1), do(del2(c, 43), S1, SN), do(del2(d, "p"), S2, SN), attribute(X, V, SN). % Doesn't work because del2(c, 43) can't be executed.

