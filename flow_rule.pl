:- discontiguous [attr0/6, attr1/7, obligation0/5].

% Golog interpreter
:- [golog_swi].

% actions

primitive_action(pr(Pin, Pout)).

primitive_action(edit(N, T, V, Vnew, Pin, Pout)).

primitive_action(del(N, T, V, Pin, Pout)).

% preconditions

poss(pr(Pin, Pout), S) :- attr0(X, N, T, V, Pin, S); obligation0(Ob, X, Cond, Pin, S).

poss(edit(N, T, V, Vnew, Pin, Pout), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(edit(N, T, V, Vnew, Pin, *), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(edit(N, T, V, Vnew, *, Pout), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(edit(N, T, V, Vnew, *, *), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(edit(N, T, *, Vnew, Pin, Pout), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(edit(N, T, *, Vnew, Pin, *), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(edit(N, T, *, Vnew, *, Pout), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(edit(N, T, *, Vnew, *, *), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(edit(N, *, V, Vnew, Pin, Pout), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(edit(N, *, V, Vnew, Pin, *), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(edit(N, *, V, Vnew, *, Pout), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(edit(N, *, V, Vnew, *, *), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(edit(N, *, *, Vnew, Pin, Pout), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(edit(N, *, *, Vnew, Pin, *), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(edit(N, *, *, Vnew, *, Pout), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(edit(N, *, *, Vnew, *, *), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(edit(*, T, V, Vnew, Pin, Pout), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(edit(*, T, V, Vnew, Pin, *), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(edit(*, T, V, Vnew, *, Pout), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(edit(*, T, V, Vnew, *, *), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(edit(*, T, *, Vnew, Pin, Pout), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(edit(*, T, *, Vnew, Pin, *), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(edit(*, T, *, Vnew, *, Pout), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(edit(*, T, *, Vnew, *, *), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(edit(*, *, V, Vnew, Pin, Pout), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(edit(*, *, V, Vnew, Pin, *), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(edit(*, *, V, Vnew, *, Pout), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(edit(*, *, V, Vnew, *, *), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(edit(*, *, *, Vnew, Pin, Pout), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(edit(*, *, *, Vnew, Pin, *), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(edit(*, *, *, Vnew, *, Pout), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(edit(*, *, *, Vnew, *, *), S) :- attr1(X, N, T, V, Pin, Pout, S).

poss(del(N, T, V, Pin, Pout), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(del(N, T, V, Pin, *), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(del(N, T, V, *, Pout), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(del(N, T, V, *, *), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(del(N, T, *, Pin, Pout), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(del(N, T, *, Pin, *), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(del(N, T, *, *, Pout), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(del(N, T, *, *, *), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(del(N, *, V, Pin, Pout), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(del(N, *, V, Pin, *), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(del(N, *, V, *, Pout), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(del(N, *, V, *, *), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(del(N, *, *, Pin, Pout), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(del(N, *, *, Pin, *), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(del(N, *, *, *, Pout), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(del(N, *, *, *, *), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(del(*, T, V, Pin, Pout), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(del(*, T, V, Pin, *), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(del(*, T, V, *, Pout), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(del(*, T, V, *, *), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(del(*, T, *, Pin, Pout), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(del(*, T, *, Pin, *), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(del(*, T, *, *, Pout), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(del(*, T, *, *, *), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(del(*, *, V, Pin, Pout), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(del(*, *, V, Pin, *), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(del(*, *, V, *, Pout), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(del(*, *, V, *, *), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(del(*, *, *, Pin, Pout), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(del(*, *, *, Pin, *), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(del(*, *, *, *, Pout), S) :- attr1(X, N, T, V, Pin, Pout, S).
poss(del(*, *, *, *, *), S) :- attr1(X, N, T, V, Pin, Pout, S).

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
            ), \+ (V2 = V)
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

% Default behaviour

attr0(X, N, T, Vold, Pin, do(A, S)) :- attr0(X, N, T, Vold, Pin, S).

obligation0(Ob, X, Cond, Pin, do(A, S)) :- obligation0(Ob, X, Cond, Pin, S).

% restoreSitArg(attribute(X,V),S,attribute(X,V,S)). % Not necessary?


% Helper for printing results

output(S) :-
      writeln(''),
      forall(attr1(X, N, T, V, Pin, Pout, S), writeln([X, N, T, V, Pin, Pout])),
      forall(obligation1(Ob, X, Cond, Pin, Pout, S), writeln([Ob, X, Cond, Pin, Pout])).

outputs(S) :-
      writeln('======'),
      forall(obligation1(Ob, X, Cond, Pin, Pout, S), writeln([Ob, X, Cond, Pout])),
      forall(attr1(X, N, T, V, Pin, Pout, S), writeln([X, N, T, V, Pout])),
      writeln('======').


% Knowledge Base

attr0(a, name1, "int", 2, pi1, s0).
attr0(b, name2, "str", "123", pi1, s0).
attr0(c, name3, "int", 45, pi1, s0).
attr0(d, name4, "str", "p", pi2, s0).
attr0(e, name5, "str", "k", pi2, s0).

obligation0(oa, a, null, pi1, s0).
obligation0(ob, b, null, pi1, s0).
obligation0(od, d, onImport, pi2, s0).


:- do(pr(pi2, po1), s0, S4), do(edit(*, *, "p", "m", *, *), S4, SN), output(SN).
:- do(pr(pi1, po1), s0, S1), do(pr(pi2, po1), S1, S2), do(pr(pi2, po2), S2, S3), do(del(name2, *, *, *, *), S3, S4), do(del(*, *, "p", *, po1), S4, SN), output(SN).
:- do(pr(pi1, po1), s0, S1), do(pr(pi2, po1), S1, S2), do(pr(pi2, po2), S2, S3), do(del(name2, *, *, *, *), S3, S4), do(edit(*, *, "p", "m", *, *), S4, SN), output(SN).
% :- do(del1(b), s0, S1), do(del2(c, 43), S1, SN), do(del2(d, "p"), S2, SN), attribute(X, V, SN). % Doesn't work because del2(c, 43) can't be executed.


%%% Remove sensitive
obligation0(keep-secret, sp, null, input2, s20).
attr0(sp, secret-part, column, 3, input2, s20).
:- do(pr(input2, output1), s20, S3), do(pr(input2, output2), S3, S4), do(edit(*, column, 3, 5, *, output1), S4, SN), outputs(SN).
% :- do(pr(input1, output1), s20, S1), do(pr(input1, output2), S1, S2), do(pr(input2, output1), S2, S3), do(pr(input2, output2), S3, S4), do(edit(*, column, 3, 5, *, output1), S4, SN), outputs(SN).  % Precondition of pr() doesn't meet

% Component 2

obligation0(keep-secret, sp, null, input, s21).
attr0(sp, secret-part, column, 5, input, s21).
:- do(pr(input, output), s21, S1), do(del(*, column, 5, *, output), S1, SN), outputs(SN).

%%% End of remove sensitive


%%% Acknowledge source
obligation0("acknowledge stat", src, "OnPublish", input1, s30).
attr0(src, source, string, "NCBI", input1, s30).
attr0(stat, statement, string, "We acknowledge that", input1, s30).

obligation0("acknowledge stat", src, "OnPublish", input2, s30).
attr0(src, source, string, "NCBI", input2, s30).
attr0(stat, statement, string, "We acknowledge that", input2, s30).

:- do(pr(input1, output), s30, S1), do(pr(input2, output), S1, SN), outputs(SN).

% Component 2

obligation0("acknowledge stat", src, "OnPublish", input2, s31).
attr0(src, source, string, "NCBI", input2, s31).
attr0(stat, statement, string, "We acknowledge that", input2, s31).

obligation0("acknowledge mstat", src2, "OnPublish", input1, s31).
attr0(src2, source, string, "IRIS", input1, s31).
attr0(mstat, my-statement, string, "Part of the data if from", input1, s31).

:- do(pr(input1, output), s31, S1), do(pr(input2, output), S1, SN), outputs(SN).

%%% End of acknowledge source
