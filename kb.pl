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
