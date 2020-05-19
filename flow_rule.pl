:- discontiguous attr0/6.
:- discontiguous obligation0/5.

% Golog interpreter
:- [golog_swi].

% actions

primitive_action(pr(Pin, Pout)).

primitive_action(edit(N, T, V, Vnew, Pin, Pout)).

primitive_action(del(N, T, V, Pin, Pout)).

% preconditions

poss(pr(Pin, Pout), S) :- true.

poss(edit(N, T, V, Vnew, Pin, Pout), S) :- true.

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
                A = edit(N, T, V, V2, Pin, Pout);
                A = edit(N, T, V, V2, Pin, *);
                A = edit(N, T, V, V2, *, Pout);
                A = edit(N, T, V, V2, *, *);
                A = edit(N, T, *, V2, Pin, Pout);
                A = edit(N, T, *, V2, Pin, *);
                A = edit(N, T, *, V2, *, Pout);
                A = edit(N, T, *, V2, *, *);
                A = edit(N, *, V, V2, Pin, Pout);
                A = edit(N, *, V, V2, Pin, *);
                A = edit(N, *, V, V2, *, Pout);
                A = edit(N, *, V, V2, *, *);
                A = edit(N, *, *, V2, Pin, Pout);
                A = edit(N, *, *, V2, Pin, *);
                A = edit(N, *, *, V2, *, Pout);
                A = edit(N, *, *, V2, *, *);
                A = edit(*, T, V, V2, Pin, Pout);
                A = edit(*, T, V, V2, Pin, *);
                A = edit(*, T, V, V2, *, Pout);
                A = edit(*, T, V, V2, *, *);
                A = edit(*, T, *, V2, Pin, Pout);
                A = edit(*, T, *, V2, Pin, *);
                A = edit(*, T, *, V2, *, Pout);
                A = edit(*, T, *, V2, *, *);
                A = edit(*, *, V, V2, Pin, Pout);
                A = edit(*, *, V, V2, Pin, *);
                A = edit(*, *, V, V2, *, Pout);
                A = edit(*, *, V, V2, *, *);
                A = edit(*, *, *, V2, Pin, Pout);
                A = edit(*, *, *, V2, Pin, *);
                A = edit(*, *, *, V2, *, Pout);
                A = edit(*, *, *, V2, *, *)
            ), (V2 \== V)
        )
    )
    .

attr1(X, N, T, V, Pin, Pout, do(A, S)) :-
    attr1(X, N, T, Vold, Pin, Pout, S),
    (
        A = edit(N, T, Vold, V ,Pin, Pout);
        A = edit(N, T, Vold, V ,Pin, *);
        A = edit(N, T, Vold, V ,*, Pout);
        A = edit(N, T, Vold, V ,*, *);
        A = edit(N, T, *, V ,Pin, Pout);
        A = edit(N, T, *, V ,Pin, *);
        A = edit(N, T, *, V ,*, Pout);
        A = edit(N, T, *, V ,*, *);
        A = edit(N, *, Vold, V ,Pin, Pout);
        A = edit(N, *, Vold, V ,Pin, *);
        A = edit(N, *, Vold, V ,*, Pout);
        A = edit(N, *, Vold, V ,*, *);
        A = edit(N, *, *, V ,Pin, Pout);
        A = edit(N, *, *, V ,Pin, *);
        A = edit(N, *, *, V ,*, Pout);
        A = edit(N, *, *, V ,*, *);
        A = edit(*, T, Vold, V ,Pin, Pout);
        A = edit(*, T, Vold, V ,Pin, *);
        A = edit(*, T, Vold, V ,*, Pout);
        A = edit(*, T, Vold, V ,*, *);
        A = edit(*, T, *, V ,Pin, Pout);
        A = edit(*, T, *, V ,Pin, *);
        A = edit(*, T, *, V ,*, Pout);
        A = edit(*, T, *, V ,*, *);
        A = edit(*, *, Vold, V ,Pin, Pout);
        A = edit(*, *, Vold, V ,Pin, *);
        A = edit(*, *, Vold, V ,*, Pout);
        A = edit(*, *, Vold, V ,*, *);
        A = edit(*, *, *, V, Pin, Pout);
        A = edit(*, *, *, V, Pin, *);
        A = edit(*, *, *, V, *, Pout);
        A = edit(*, *, *, V, *, *)
    )
    .

attr1(X, N, T, V, Pin, Pout, do(A, S)) :-
    attr0(X, N, T, V, Pin, S),
    A = pr(Pin, Pout)
    .


% Maybe useful for Golog (never proven to be useful)
restoreSitArg(attr0(X,N,T,V,Pin),S,attr0(X,N,T,V,Pin,Pout,S)).
restoreSitArg(obligation0(Ob,X,Cond,Pin),S,obligation0(Ob,X,Cond,Pin,S)).
