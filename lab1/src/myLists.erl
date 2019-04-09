-module(myLists).
-author("Damian").
-export([contains/2, duplicate/1, sumFloat/1, sumFloat2/2]).

contains(Num, [H | T]) ->
  if
    H == Num -> true;
    T == [] -> false;
    true -> contains(Num, T)
  end.


duplicate([]) -> [];
duplicate([H | T]) ->
  [H | [H | duplicate(T)]].


sumFloat([]) -> 0;
sumFloat([H | T]) -> H + sumFloat(T).


sumFloat2([], Sum) -> Sum;
sumFloat2([H | T], Sum) -> sumFloat2(T, Sum + H).