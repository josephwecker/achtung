-module(dummy).
-export([duplicate/2]).

-spec duplicate(non_neg_integer(), [T]) -> [T].

duplicate(N, X) when is_integer(N), N >= 0 -> duplicate(N, X, []).
duplicate(0, _, L) -> L;
duplicate(N, X, L) -> duplicate(N-1, X, [X|L]).

