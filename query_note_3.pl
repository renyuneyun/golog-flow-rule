%%% Queries for Section 4 (of document Notes)
:- multifile attr/6.

:- [helper].


attr(a, name1, column, 1, pi1, s0).
attr(b, dob1, column, 2, pi1, s0).
attr(c, year1, column, 2, pi1, s0).
attr(d, weight, column, 3, pi1, s0).


% 4
:- do(pr(pi1, [po1]) : del(*, column, 2, pi1, po1) : edit(*, column, 3, column, 2, pi1, po1) : end(po1), s0, SN), outputs(SN).

