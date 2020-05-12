:- discontiguous [attr0/4, attr1/5, obligation0/5].

% Golog interpreter
:- [golog_swi].

% actions

primitive_action(pr(Pin, Pout)).

primitive_action(edit(X, V, Vnew, Pin, Pout)).

primitive_action(del(X, V, Pin, Pout)).

% preconditions

poss(pr(Pin, Pout), S) :- attr0(X, V, Pin, S); obligation0(Ob, X, Cond, Pin, S).

poss(edit(X, V, Vnew, Pin, Pout), S) :- attr1(X, V, Pin, Pout, S).
poss(edit(X, V, Vnew, Pin, *), S) :- attr1(X, V, Pin, _Pout, S).
poss(edit(X, V, Vnew, *, Pout), S) :- attr1(X, V, _Pin, Pout, S).
poss(edit(X, V, Vnew, *, *), S) :- attr1(X, V, _Pin, _Pout, S).
poss(edit(X, *, Vnew, Pin, Pout), S) :- attr1(X, _V, Pin, Pout, S).
poss(edit(X, *, Vnew, Pin, *), S) :- attr1(X, _V, Pin, _Pout, S).
poss(edit(X, *, Vnew, *, Pout), S) :- attr1(X, _V, _Pin, Pout, S).
poss(edit(X, *, Vnew, *, *), S) :- attr1(X, _V, _Pin, _Pout, S).

poss(del(X, V, Pin, Pout), S) :- attr1(X, V, Pin, Pout, S).
poss(del(X, V, Pin, *), S) :- attr1(X, V, Pin, _Pout, S).
poss(del(X, V, *, Pout), S) :- attr1(X, V, _Pin, Pout, S).
poss(del(X, V, *, *), S) :- attr1(X, V, _Pin, _Pout, S).
poss(del(X, *, Pin, Pout), S) :- attr1(X, _V, Pin, Pout, S).
poss(del(X, *, Pin, *), S) :- attr1(X, _V, Pin, _Pout, S).
poss(del(X, *, *, Pout), S) :- attr1(X, _V, _Pin, Pout, S).
poss(del(X, *, *, *), S) :- attr1(X, _V, _Pin, _Pout, S).

% successor-state axioms

obligation1(Ob, X, Cond, Pin, Pout, do(A, S)) :-
    obligation1(Ob, X, Cond, Pin, Pout, S),
    \+ (
        attr1(X, V, Pin2, Pout2, S),
        (
            A = del(X, V, Pin, Pout);
            A = del(X, V, Pin, *);
            A = del(X, V, *, Pout);
            A = del(X, V, *, *);
            A = del(X, *, Pin, Pout);
            A = del(X, *, Pin, *);
            A = del(X, *, *, Pout);
            A = del(X, *, *, *)
        )
    )
    .

obligation1(Ob, X, Cond, Pin, Pout, do(A, S)) :-
    obligation0(Ob, X, Cond, Pin, S),
    A = pr(Pin, Pout)
    .

attr1(X, V, Pin, Pout, do(A, S)) :-
    attr1(X, V, Pin, Pout, S),
    \+ (
        A = del(X, V, Pin, Pout);
        A = del(X, V, Pin, *);
        A = del(X, V, *, Pout);
        A = del(X, V, *, *);
        A = del(X, *, Pin, Pout);
        A = del(X, *, Pin, *);
        A = del(X, *, *, Pout);
        A = del(X, *, *, *);
        (
            (
                A = edit(X, V, V2, Pin, Pout);
                A = edit(X, V, V2, Pin, *);
                A = edit(X, V, V2, *, Pout);
                A = edit(X, V, V2, *, *);
                A = edit(X, *, V2, Pin, Pout);
                A = edit(X, *, V2, Pin, *);
                A = edit(X, *, V2, *, Pout);
                A = edit(X, *, V2, *, *)
            ), \+ (V2 = V)
        )
    )
    .

attr1(X, V, Pin, Pout, do(A, S)) :-
    attr1(X, Vold, Pin, Pout, S),
    (
        A = edit(X, Vold, V, Pin, Pout);
        A = edit(X, Vold, V, Pin, *);
        A = edit(X, Vold, V, *, Pout);
        A = edit(X, Vold, V, *, *);
        A = edit(X, *, V, Pin, Pout);
        A = edit(X, *, V, Pin, *);
        A = edit(X, *, V, *, Pout);
        A = edit(X, *, V, *, *)
    )
    .

attr1(X, V, Pin, Pout, do(A, S)) :-
    attr0(X, V, Pin, S),
    A = pr(Pin, Pout)
    .

% Default behaviour

attr0(X, V, Pin, do(A, S)) :- attr0(X, V, Pin, S).

obligation0(Ob, X, Cond, Pin, do(A, S)) :- obligation0(Ob, X, Cond, Pin, S).

% restoreSitArg(attribute(X,V),S,attribute(X,V,S)). % Not necessary?


% Knowledge Base

attr0(a, 2, pi1, s0).
attr0(b, "123", pi1, s0).
attr0(c, 45, pi1, s0).
attr0(d, "p", pi2, s0).
attr0(e, "k", pi2, s0).

obligation0(oa, a, null, pi1, s0).
obligation0(ob, b, null, pi1, s0).
obligation0(od, d, onImport, pi2, s0).


output(S) :-
      writeln(''),
      forall(attr1(X, V, Pin, Pout, S), writeln([S, X, V, Pin, Pout])),
      forall(obligation1(Ob, X, Cond, Pin, Pout, S), writeln([S, Ob, X, Cond, Pin, Pout])).


:- do(pr(pi2, po1), s0, S4), do(edit(d, "p", "m", *, *), S4, SN), output(SN).
:- do(pr(pi1, po1), s0, S1), do(pr(pi2, po1), S1, S2), do(pr(pi2, po2), S2, S3), do(del(b, *, *, *), S3, S4), do(del(d, "p", *, po1), S4, SN), output(SN).
:- do(pr(pi1, po1), s0, S1), do(pr(pi2, po1), S1, S2), do(pr(pi2, po2), S2, S3), do(del(b, *, *, *), S3, S4), do(edit(d, "p", "m", *, *), S4, SN), output(SN).
% :- do(del1(b), s0, S1), do(del2(c, 43), S1, SN), do(del2(d, "p"), S2, SN), attribute(X, V, SN). % Doesn't work because del2(c, 43) can't be executed.

