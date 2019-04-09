%%%-------------------------------------------------------------------
%%% @author Damian
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. mar 2019 13:09
%%%-------------------------------------------------------------------
-module(qsort).
-author("Damian").
-export([qs/1, randomElems/3, compareSpeeds/3, liczbaCyfr/1]).

lessThan(List, Arg) -> [X || X <- List, X < Arg].
grtEqThan(List, Arg) -> [X || X <- List, X >= Arg].

qs([]) -> [];
qs([Pivot | Tail]) -> qs(lessThan(Tail, Pivot)) ++ [Pivot] ++ qs(grtEqThan(Tail, Pivot)).

randomElems(N, Min, Max) -> [rand:uniform(Max - Min + 1) + Min - 1 || X <- lists:seq(1, N)].

compareSpeeds(List, Fun1, Fun2) ->
  {T1, _} = timer:tc(?MODULE, Fun1, [List]),
  {T2, _} = timer:tc(lists, Fun2, [List]),
  {T1 / 1000000, T2 / 1000000}.

liczbaCyfr(Liczba) ->
  Liczba2 = integer_to_list(Liczba),
  Dodaj = fun (_, X) -> X+1 end,
  lists:foldl(Dodaj, 0, Liczba2).


%rzucanie bledu - {error, "opis"}
