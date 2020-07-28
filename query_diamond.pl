%%% Sample queries for debugging
:- multifile attr/6.
:- multifile obligation/5.

:- [helper].


attr(a, name, str, "UoE", ai, s0).
obligation(credit, a, null, ai, s0).


% :- do(pr(ai, [ao1, ao2]) : end(ao1) : end(ao2) :
% 	pr(ao1, [bi]) : end(bi) : pr(ao2, [ci]) : end(ci) :
% 	pr(bi, [bo]) : end(bo) :
% 	pr(ci, [co]) : end(co) :
% 	pr(bo, [di1]) : end(di1) : pr(co, [di2]) : end(di2) :
% 	pr(di1, [do]) : pr(di2, [do]) : end(do),
% 	s0, SN
% ), outputs(SN).
%% Too slow, trying to optimise using the method below


:- do(pr(ai, [ao1, ao2]) : end(ao1) : end(ao2), s0, S1), !,
	do(pr(ao1, [bi]) : end(bi) : pr(ao2, [ci]) : end(ci), S1, S2), !,
	do(pr(bi, [bo]) : end(bo), S2, S3), !,
	do(pr(ci, [co]) : edit(*, str, "UoE", str, "University of Edinburgh", *, *) : end(co), S3, S4), !,
	do(pr(bo, [di1]) : end(di1), S4, S5), !,
	do(pr(co, [di2]) : end(di2), S5, S6), !,
	do(pr(di1, [do]) : pr(di2, [do]) : end(do), S6, S7), !,
	outputs(S7).
%% Not as fast as expected either
