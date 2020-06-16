% Helper for printing results

output(S) :-
      writeln(''),
      forall(prop_attr(X, N, T, V, Pin, Pout, S), writeln([X, N, T, V, Pin, Pout])).

outputs(S) :-
      writeln('======'),
      forall(attr(X, N, T, V, P, S), writeln([X, N, T, V, P])),
      writeln('======').

