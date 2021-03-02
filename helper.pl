% Helper for printing results

output(S) :-
      writeln('***'),
      forall(prop_attr(N, T, V, H, S), writeln([N, T, V, H])),
      forall(prop_obligation(Ob, XHL, BHL, Cond, Pin, Pout, S), writeln([Ob, XHL, BHL, Cond, Pin, Pout])),
      writeln('####')
      .

outputs(S) :-
      writeln('======'),
      forall(attr(N, T, V, H, S), writeln([N, T, V, H])),
      writeln('-'),
      forall(obligation(Ob, XHL, BHL, Cond, P, S), writeln([Ob, XHL, BHL, Cond, P])),
      writeln('======').

