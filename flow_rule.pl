:- multifile attr/6.
:- multifile obligation/5.

% Golog interpreter
:- [golog_swi].

% actions

primitive_action(pr(Pin, Ps)).

primitive_action(edit(N, T, V, Tnew, Vnew, Pin, Pout)).

primitive_action(del(N, T, V, Pin, Pout)).

primitive_action(end(Pout)).

% preconditions

poss(pr(Pin, Ps), S) :- true.

poss(edit(N, T, V, Tnew, Vnew, Pin, Pout), S) :- true.

poss(del(N, T, V, Pin, Pout), S) :- true.

poss(end(Pout), S) :- true.

% successor-state axioms

prop_obligation(Ob, X, Cond, Pin, Pout, do(A, S)) :-
    prop_obligation(Ob, X, Cond, Pin, Pout, S),
    \+ (
        % prop_attr(X, N, T, V, Pin, Pout, S),
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
        );
        A = end(Pout)
    )
    .

prop_obligation(Ob, X, Cond, Pin, Pout, do(A, S)) :-
    obligation(Ob, X, Cond, Pin, S),
    A = pr(Pin, Ps), member(Pout, Ps)
    .

obligation(Ob, X, Cond, P, do(A, S)) :-
    obligation(Ob, X, Cond, P, S), A \= pr(P, Ps)
    .

obligation(Ob, X, Cond, P, do(A, S)) :-
    prop_obligation(Ob, X, Cond, Pin, P, S),
    A = end(P)
    .

prop_attr(X, N, T, V, Pin, Pout, do(A, S)) :-
    prop_attr(X, N, T, V, Pin, Pout, S),
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
        );
        A = end(Pout)
    )
    .

prop_attr(X, N, T, V, Pin, Pout, do(A, S)) :-
    prop_attr(X, N, Told, Vold, Pin, Pout, S),
    (
        A = edit(N, Told, Vold, T, V, Pin, Pout);
        A = edit(N, Told, Vold, T, V, Pin, *);
        A = edit(N, Told, Vold, T, V, *, Pout);
        A = edit(N, Told, Vold, T, V, *, *);
        A = edit(N, Told, *, T, V, Pin, Pout);
        A = edit(N, Told, *, T, V, Pin, *);
        A = edit(N, Told, *, T, V, *, Pout);
        A = edit(N, Told, *, T, V, *, *);
        A = edit(N, *, Vold, T, V, Pin, Pout);
        A = edit(N, *, Vold, T, V, Pin, *);
        A = edit(N, *, Vold, T, V, *, Pout);
        A = edit(N, *, Vold, T, V, *, *);
        A = edit(N, *, *, T, V, Pin, Pout);
        A = edit(N, *, *, T, V, Pin, *);
        A = edit(N, *, *, T, V, *, Pout);
        A = edit(N, *, *, T, V, *, *);
        A = edit(*, Told, Vold, T, V, Pin, Pout);
        A = edit(*, Told, Vold, T, V, Pin, *);
        A = edit(*, Told, Vold, T, V, *, Pout);
        A = edit(*, Told, Vold, T, V, *, *);
        A = edit(*, Told, *, T, V, Pin, Pout);
        A = edit(*, Told, *, T, V, Pin, *);
        A = edit(*, Told, *, T, V, *, Pout);
        A = edit(*, Told, *, T, V, *, *);
        A = edit(*, *, Vold, T, V, Pin, Pout);
        A = edit(*, *, Vold, T, V, Pin, *);
        A = edit(*, *, Vold, T, V, *, Pout);
        A = edit(*, *, Vold, T, V, *, *);
        A = edit(*, *, *, T, V, Pin, Pout);
        A = edit(*, *, *, T, V, Pin, *);
        A = edit(*, *, *, T, V, *, Pout);
        A = edit(*, *, *, T, V, *, *)
    )
    .

prop_attr(X, N, T, V, Pin, Pout, do(A, S)) :-
    attr(X, N, T, V, Pin, S),
    A = pr(Pin, Ps) , member(Pout, Ps)
    .

attr(X, N, T, V, P, do(A, S)) :-
    attr(X, N, T, V, P, S), A \= pr(P, Ps)
    .

attr(X, N, T, V, P, do(A, S)) :-
    prop_attr(X, N, T, V, Pin, P, S),
    A == end(P)
    .


% Maybe useful for Golog (never proven to be useful)
% restoreSitArg(attr0(X,N,T,V,Pin),S,attr0(X,N,T,V,Pin,Pout,S)).
% restoreSitArg(obligation0(Ob,X,Cond,Pin),S,obligation0(Ob,X,Cond,Pin,S)).
