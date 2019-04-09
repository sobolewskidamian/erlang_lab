-module(zad1).
-author("Damian").
-export([power/2]).

power(A, B) ->
  case B of
    0 -> 1;
    1 -> A;
    _ -> A * power(A, B - 1)
  end.
