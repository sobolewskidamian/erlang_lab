%%%-------------------------------------------------------------------
%%% @author Damian
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. mar 2019 20:00
%%%-------------------------------------------------------------------
-module(hello).
-author("Damian").
-export([testRecord/0, testNestedRecord/0, catchme/1, generate_exception/1]).
-record(grupa, {nazwa, licznosc, stan=aktywna}).
-record(nadgrupa, {nadnazwa, grp}).

testRecord() ->
  Grupa1 = #grupa{nazwa="Gruppa 1", licznosc=12},
  %Grupa2 = #grupa{nazwa="Gruppa 2", licznosc=7, stan=0},
  Grupa2 = Grupa1#grupa{nazwa = "abc"},
  io:format(Grupa2#grupa.nazwa).

testNestedRecord() ->
  Nad = #nadgrupa{
    nadnazwa = "Nad 3",
    grp = #grupa{nazwa="Gruppa 3", licznosc=7}},
  (Nad#nadgrupa.grp)#grupa.nazwa,
  Nad#nadgrupa.grp#grupa.nazwa.


catchme(N) ->
  try generate_exception(N) of
    Val -> {N, normal, Val}
  catch
    throw:X -> {N, thw, X};
    exit:X -> {N, ext, X};
    error:X -> {N, err, X}
  end.
generate_exception(1) -> a;
generate_exception(2) -> throw(a);
generate_exception(3) -> exit(a);
generate_exception(4) -> erlang:error(a);
generate_exception(5) -> {'EXIT', a};
generate_exception(6) -> 1/0;
generate_exception(7) -> list:seq(1,asd).