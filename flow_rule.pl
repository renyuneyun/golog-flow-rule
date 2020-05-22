:- discontiguous attr0/6.
:- discontiguous obligation0/5.

% Golog interpreter
:- [golog_swi].

% actions

primitive_action(pr(Pin, Pout)).

primitive_action(edit(N, T, V, Tnew, Vnew, Pin, Pout)).

primitive_action(del(N, T, V, Pin, Pout)).

% preconditions

poss(pr(Pin, Pout), S) :- true.

poss(edit(N, T, V, Tnew, Vnew, Pin, Pout), S) :- true.

poss(del(N, T, V, Pin, Pout), S) :- true.

% successor-state axioms

obligation1(Ob, X, Cond, Pin, Pout, do(A, S)) :-
    obligation1(Ob, X, Cond, Pin, Pout, S),
    \+ (
        attr1(X, N, T, V, Pin, Pout, S),
        (
            A = del(N, T, V, Pin, Pout);
            A = del(N, T, V, Pin, *);
            A = del(N, T, V, *, Pout);
            A = del(N, T, V, *, *);
            A = del(N, T, *, Pin, Pout);
            A = del(N, T, *, Pin, *);
            A = del(N, T, *, *, Pout);
            A = del(N, T, *, *, *);
            A = del(N, *, V, Pin, Pout);
            A = del(N, *, V, Pin, *);
            A = del(N, *, V, *, Pout);
            A = del(N, *, V, *, *);
            A = del(N, *, *, Pin, Pout);
            A = del(N, *, *, Pin, *);
            A = del(N, *, *, *, Pout);
            A = del(N, *, *, *, *);
            A = del(*, T, V, Pin, Pout);
            A = del(*, T, V, Pin, *);
            A = del(*, T, V, *, Pout);
            A = del(*, T, V, *, *);
            A = del(*, T, *, Pin, Pout);
            A = del(*, T, *, Pin, *);
            A = del(*, T, *, *, Pout);
            A = del(*, T, *, *, *);
            A = del(*, *, V, Pin, Pout);
            A = del(*, *, V, Pin, *);
            A = del(*, *, V, *, Pout);
            A = del(*, *, V, *, *);
            A = del(*, *, *, Pin, Pout);
            A = del(*, *, *, Pin, *);
            A = del(*, *, *, *, Pout);
            A = del(*, *, *, *, *)
        )
    )
    .

obligation1(Ob, X, Cond, Pin, Pout, do(A, S)) :-
    obligation0(Ob, X, Cond, Pin, S),
    A = pr(Pin, Pout)
    .

attr1(X, N, T, V, Pin, Pout, do(A, S)) :-
    attr1(X, N, T, V, Pin, Pout, S),
    \+ (
            A = del(N, T, V, Pin, Pout);
            A = del(N, T, V, Pin, *);
            A = del(N, T, V, *, Pout);
            A = del(N, T, V, *, *);
            A = del(N, T, *, Pin, Pout);
            A = del(N, T, *, Pin, *);
            A = del(N, T, *, *, Pout);
            A = del(N, T, *, *, *);
            A = del(N, *, V, Pin, Pout);
            A = del(N, *, V, Pin, *);
            A = del(N, *, V, *, Pout);
            A = del(N, *, V, *, *);
            A = del(N, *, *, Pin, Pout);
            A = del(N, *, *, Pin, *);
            A = del(N, *, *, *, Pout);
            A = del(N, *, *, *, *);
            A = del(*, T, V, Pin, Pout);
            A = del(*, T, V, Pin, *);
            A = del(*, T, V, *, Pout);
            A = del(*, T, V, *, *);
            A = del(*, T, *, Pin, Pout);
            A = del(*, T, *, Pin, *);
            A = del(*, T, *, *, Pout);
            A = del(*, T, *, *, *);
            A = del(*, *, V, Pin, Pout);
            A = del(*, *, V, Pin, *);
            A = del(*, *, V, *, Pout);
            A = del(*, *, V, *, *);
            A = del(*, *, *, Pin, Pout);
            A = del(*, *, *, Pin, *);
            A = del(*, *, *, *, Pout);
            A = del(*, *, *, *, *);
        (
            (
                A = edit(N, T, V, T2, V2, Pin, Pout);
                A = edit(N, T, V, T2, V2, Pin, *);
                A = edit(N, T, V, T2, V2, *, Pout);
                A = edit(N, T, V, T2, V2, *, *);
                A = edit(N, T, *, T2, V2, Pin, Pout);
                A = edit(N, T, *, T2, V2, Pin, *);
                A = edit(N, T, *, T2, V2, *, Pout);
                A = edit(N, T, *, T2, V2, *, *);
                A = edit(N, *, V, T2, V2, Pin, Pout);
                A = edit(N, *, V, T2, V2, Pin, *);
                A = edit(N, *, V, T2, V2, *, Pout);
                A = edit(N, *, V, T2, V2, *, *);
                A = edit(N, *, *, T2, V2, Pin, Pout);
                A = edit(N, *, *, T2, V2, Pin, *);
                A = edit(N, *, *, T2, V2, *, Pout);
                A = edit(N, *, *, T2, V2, *, *);
                A = edit(*, T, V, T2, V2, Pin, Pout);
                A = edit(*, T, V, T2, V2, Pin, *);
                A = edit(*, T, V, T2, V2, *, Pout);
                A = edit(*, T, V, T2, V2, *, *);
                A = edit(*, T, *, T2, V2, Pin, Pout);
                A = edit(*, T, *, T2, V2, Pin, *);
                A = edit(*, T, *, T2, V2, *, Pout);
                A = edit(*, T, *, T2, V2, *, *);
                A = edit(*, *, V, T2, V2, Pin, Pout);
                A = edit(*, *, V, T2, V2, Pin, *);
                A = edit(*, *, V, T2, V2, *, Pout);
                A = edit(*, *, V, T2, V2, *, *);
                A = edit(*, *, *, T2, V2, Pin, Pout);
                A = edit(*, *, *, T2, V2, Pin, *);
                A = edit(*, *, *, T2, V2, *, Pout);
                A = edit(*, *, *, T2, V2, *, *)
            ), (T2 \== T; V2 \== V)
        )
    )
    .

attr1(X, N, T, V, Pin, Pout, do(A, S)) :-
    attr1(X, N, Told, Vold, Pin, Pout, S),
    (
        A = edit(N, Told, Vold, T, V ,Pin, Pout);
        A = edit(N, Told, Vold, T, V ,Pin, *);
        A = edit(N, Told, Vold, T, V ,*, Pout);
        A = edit(N, Told, Vold, T, V ,*, *);
        A = edit(N, Told, *, V, T ,Pin, Pout);
        A = edit(N, Told, *, V, T ,Pin, *);
        A = edit(N, Told, *, V, T ,*, Pout);
        A = edit(N, Told, *, V, T ,*, *);
        A = edit(N, *, Vold, T, V ,Pin, Pout);
        A = edit(N, *, Vold, T, V ,Pin, *);
        A = edit(N, *, Vold, T, V ,*, Pout);
        A = edit(N, *, Vold, T, V ,*, *);
        A = edit(N, *, *, V, T ,Pin, Pout);
        A = edit(N, *, *, V, T ,Pin, *);
        A = edit(N, *, *, V, T ,*, Pout);
        A = edit(N, *, *, V, T ,*, *);
        A = edit(*, Told, Vold, T, V ,Pin, Pout);
        A = edit(*, Told, Vold, T, V ,Pin, *);
        A = edit(*, Told, Vold, T, V ,*, Pout);
        A = edit(*, Told, Vold, T, V ,*, *);
        A = edit(*, Told, *, V, T ,Pin, Pout);
        A = edit(*, Told, *, V, T ,Pin, *);
        A = edit(*, Told, *, V, T ,*, Pout);
        A = edit(*, Told, *, V, T ,*, *);
        A = edit(*, *, Vold, T, V ,Pin, Pout);
        A = edit(*, *, Vold, T, V ,Pin, *);
        A = edit(*, *, Vold, T, V ,*, Pout);
        A = edit(*, *, Vold, T, V ,*, *);
        A = edit(*, *, *, V, T, Pin, Pout);
        A = edit(*, *, *, V, T, Pin, *);
        A = edit(*, *, *, V, T, *, Pout);
        A = edit(*, *, *, V, T, *, *)
    )
    .

attr1(X, N, T, V, Pin, Pout, do(A, S)) :-
    attr0(X, N, T, V, Pin, S),
    A = pr(Pin, Pout)
    .


% Maybe useful for Golog (never proven to be useful)
restoreSitArg(attr0(X,N,T,V,Pin),S,attr0(X,N,T,V,Pin,Pout,S)).
restoreSitArg(obligation0(Ob,X,Cond,Pin),S,obligation0(Ob,X,Cond,Pin,S)).
