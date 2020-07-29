% Helper for printing results

output(S) :-
      writeln('***'),
      forall(prop_attr(N, T, V, H, S), writeln([N, T, V, H])),
      forall(prop_obligation(Ob, X, Cond, Pin, Pout, S), writeln([Ob, X, Cond, Pin, Pout])),
      writeln('####')
      .

outputs(S) :-
      writeln('======'),
      forall(attr(N, T, V, H, S), writeln([N, T, V, H])),
      writeln('-'),
      forall(obligation(Ob, X, Cond, P, S), writeln([Ob, X, Cond, P])),
      writeln('======').

