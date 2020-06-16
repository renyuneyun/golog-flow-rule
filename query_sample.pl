%%% Sample queries for debugging
:- multifile attr/6.

:- [helper].


attr(a, name1, "int", 2, pi1, s0).
attr(b, name2, "str", "123", pi1, s0).
attr(c, name3, "int", 45, pi1, s0).
attr(d, name4, "str", "p", pi2, s0).
attr(e, name5, "str", "k", pi2, s0).


:- do(pr(pi1, []) : pr(pi2, [po1]) : end(po1), s0, SN), outputs(SN).

:- do(pr(pi1, []) : pr(pi2, [po1]) : edit(name4, "str", "p", "str", "m", pi2, po1) : end(po1), s0, SN), outputs(SN).
:- do(pr(pi1, []) : pr(pi2, [po1]) : del(name4, "str", "p", pi2, po1) : end(po1), s0, SN), outputs(SN).
:- do(pr(pi1, [po1]) : pr(pi2, [po1]) : edit(name4, "str", "p", "str", "m", pi2, po1) : end(po1), s0, SN), outputs(SN).

