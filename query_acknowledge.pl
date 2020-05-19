%%% Acknowledge source

:- discontiguous attr0/6.
:- discontiguous obligation0/5.

:- [helper].


% Persist attr0 and obligation0

attr0(X, N, T, Vold, Pin, do(A, S)) :- attr0(X, N, T, Vold, Pin, S).

obligation0(Ob, X, Cond, Pin, do(A, S)) :- obligation0(Ob, X, Cond, Pin, S).


% Knowledge Base


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

