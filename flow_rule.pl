:- multifile attr/5.
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

prop_obligation(Ob, XH, Cond, Pin, Pout, do(A, S)) :-
    prop_obligation(Ob, XH, Cond, Pin, Pout, S),
    \+ (
        prop_attr(N, T, V, [Pout|XH], S),
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

prop_obligation(Ob, XH, Cond, Pin, Pout, do(pr(Pin, Ps), S)) :-
    member(Pout, Ps),
    obligation(Ob, XH, Cond, Pin, S)
    .

obligation(Ob, XH, Cond, P, do(A, S)) :-
    A \= pr(P, _Ps),
    obligation(Ob, XH, Cond, P, S)
    .

obligation(Ob, null, Cond, P, do(end(P), S)) :-
    prop_obligation(Ob, null, Cond, Pin, P, S)
    .

obligation(Ob, [P|XH1], Cond, P, do(end(P), S)) :-
    prop_obligation(Ob, XH, Cond, Pin, P, S),
    XH = [Pin|XH1]
    .

prop_attr(N, T, V, H, do(A, S)) :-
    H = [Pout|[Pin|_]],
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
    ), prop_attr(N, T, V, H, S)
    .

prop_attr(N, T, V, H, do(A, S)) :-
    H = [Pout|[Pin|_]],
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
    ), prop_attr(N, Told, Vold, H, S)
    .

prop_attr(N, T, V, [Pout|H], do(pr(Pin, Ps), S)) :-
    member(Pout, Ps), H = [Pin|_],
    attr(N, T, V, H, S)
    .

attr(N, T, V, H, do(A, S)) :-
    H = [P|_], A \= pr(P, Ps),
    attr(N, T, V, H, S)
    .

attr(N, T, V, Ho, do(end(P), S)) :-
    Ho = [P|H1], H = [P|[_Pin|H1]],
    prop_attr(N, T, V, H, S)
    .


% Maybe useful for Golog (never proven to be useful)
% restoreSitArg(attr0(X,N,T,V,Pin),S,attr0(X,N,T,V,Pin,Pout,S)).
% restoreSitArg(obligation0(Ob,X,Cond,Pin),S,obligation0(Ob,X,Cond,Pin,S)).
