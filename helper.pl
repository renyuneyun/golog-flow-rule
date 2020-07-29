% Helper for printing results

output(S) :-
      writeln(''),
      forall(prop_attr(X, N, T, V, Pin, Pout, H, S), writeln([X, N, T, V, Pin, Pout, H])),
      forall(prop_obligation(Ob, X, Cond, Pin, Pout, S), writeln([Ob, X, Cond, Pin, Pout]))
      .

outputs(S) :-
      writeln('======'),
      forall(attr(X, N, T, V, P, H, S), writeln([X, N, T, V, P, H])),
      writeln('-'),
      forall(obligation(Ob, X, Cond, P, S), writeln([Ob, X, Cond, P])),
      writeln('======').

