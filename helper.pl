% Helper for printing results

output(S) :-
      writeln(''),
      forall(attr1(X, N, T, V, Pin, Pout, S), writeln([X, N, T, V, Pin, Pout])),
      forall(obligation1(Ob, X, Cond, Pin, Pout, S), writeln([Ob, X, Cond, Pin, Pout])).

outputs(S) :-
      writeln('======'),
      forall(obligation1(Ob, X, Cond, Pin, Pout, S), writeln([Ob, X, Cond, Pout])),
      forall(attr1(X, N, T, V, Pin, Pout, S), writeln([X, N, T, V, Pout])),
      writeln('======').


