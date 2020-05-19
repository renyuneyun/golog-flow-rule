%%% Sample queries for debugging

:- discontiguous attr0/6.
:- discontiguous obligation0/5.

:- [helper].


% Maybe useful for Golog (never proven to be useful)
restoreSitArg(attr0(X,N,T,V,Pin),S,attr0(X,N,T,V,Pin,Pout,S)).
restoreSitArg(obligation0(Ob,X,Cond,Pin),S,obligation0(Ob,X,Cond,Pin,S)).


% Persist attr0 and obligation0

attr0(X, N, T, Vold, Pin, do(A, S)) :- attr0(X, N, T, Vold, Pin, S).

obligation0(Ob, X, Cond, Pin, do(A, S)) :- obligation0(Ob, X, Cond, Pin, S).


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

