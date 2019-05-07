-module(pingpong).
-author("Damian").

-export([start/0, send/1, stop/0, ping_loop/0, pong_loop/0]).

ping_loop() ->
  receive
    stop -> ok;
    0 -> pingpong:ping_loop();
    N ->
      timer:sleep(500),
      pong ! (N - 1),
      io:format("Ping ~w~n", [N]),
      pingpong:ping_loop()
  after
    5000 -> io:format("Koniec dzialania ping~n"), ok
  end.

pong_loop() ->
  receive
    stop -> ok;
    0 -> pingpong:pong_loop();
    N ->
      timer:sleep(500),
      ping ! (N - 1),
      io:format("Pong ~w~n", [N]),
      pingpong:pong_loop()
  after
    5000 -> io:format("Koniec dzialania pong~n"), ok
  end.

start() ->
  register(ping, spawn(?MODULE, ping_loop, [])),
  register(pong, spawn(?MODULE, pong_loop, [])).

send(N) ->
  ping ! N.

stop() ->
  ping ! stop,
  pong ! stop.