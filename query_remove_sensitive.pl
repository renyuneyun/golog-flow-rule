%%% Remove sensitive

:- discontiguous attr0/6.
:- discontiguous obligation0/5.

:- [helper].


% Persist attr0 and obligation0

attr0(X, N, T, Vold, Pin, do(A, S)) :- attr0(X, N, T, Vold, Pin, S).

obligation0(Ob, X, Cond, Pin, do(A, S)) :- obligation0(Ob, X, Cond, Pin, S).


% Knowledge Base

obligation0("keep-secret", sp, nil, input2, s20).
attr0(sp, secret-part, column, 3, input2, s20).
% :- do(pr(input2, output1), s20, S3), do(pr(input2, output2), S3, S4), do(edit(*, column, 3, 5, *, output1), S4, SN), outputs(SN).
:- do(pr(input1, output1), s20, S1), do(pr(input1, output2), S1, S2), do(pr(input2, output1), S2, S3), do(pr(input2, output2), S3, S4), do(edit(*, column, 3, column, 5, *, output1), S4, SN), outputs(SN).

% Component 2

obligation0("keep-secret", sp, nil, input, s21).
attr0(sp, secret-part, column, 5, input, s21).
:- do(pr(input, output), s21, S1), do(del(*, column, 5, *, output), S1, SN), outputs(SN).

