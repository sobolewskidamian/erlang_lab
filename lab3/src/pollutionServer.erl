-module(pollutionServer).

-export([start/0]).
-export([init/0]).
-export([send/2]).
-export([stop/0]).
-export([addStation/2]).
-export([addValue/4]).
-export([removeValue/3]).
-export([getOneValue/3]).
-export([getStationMean/2]).
-export([getDailyMean/2]).
-export([getDailyAverageDataCount/1]).
-export([test/0]).

start() -> register(server, spawn(?MODULE, init, [])).
init() -> loop(pollution:createMonitor()).

getMonitor(Function, Monitor, Pid) ->
  case Function of
    {error, Error} -> Pid ! {reply, {error, Error}},
      loop(Monitor);
    _ -> Pid ! {reply, ok},
      loop(Function)
  end.

loop(Monitor) ->
  receive
    {request, Pid, addStation, {Name, {X, Y}}} ->
      getMonitor(pollution:addStation(Name, {X, Y}, Monitor), Monitor, Pid);

    {request, Pid, addValue, {{X, Y}, {Date, Time}, Type, Value}} ->
      getMonitor(pollution:addValue({X, Y}, {Date, Time}, Type, Value, Monitor), Monitor, Pid);
    {request, Pid, addValue, {Name, {Date, Time}, Type, Value}} ->
      getMonitor(pollution:addValue(Name, {Date, Time}, Type, Value, Monitor), Monitor, Pid);

    {request, Pid, removeValue, {{X, Y}, {Date, Time}, Type}} ->
      getMonitor(pollution:removeValue({X, Y}, {Date, Time}, Type, Monitor), Monitor, Pid);
    {request, Pid, removeValue, {Name, {Date, Time}, Type}} ->
      getMonitor(pollution:removeValue(Name, {Date, Time}, Type, Monitor), Monitor, Pid);

    {request, Pid, getOneValue, {{X, Y}, {Date, Time}, Type}} ->
      Pid ! {reply, pollution:getOneValue({X, Y}, {Date, Time}, Type, Monitor)},
      loop(Monitor);
    {request, Pid, getOneValue, {Name, {Date, Time}, Type}} ->
      Pid ! {reply, pollution:getOneValue(Name, {Date, Time}, Type, Monitor)},
      loop(Monitor);

    {request, Pid, getStationMean, {{X, Y}, Type}} ->
      Pid ! {reply, pollution:getStationMean({X, Y}, Type, Monitor)},
      loop(Monitor);
    {request, Pid, getStationMean, {Name, Type}} ->
      Pid ! {reply, pollution:getStationMean(Name, Type, Monitor)},
      loop(Monitor);

    {request, Pid, getDailyMean, {Date, Type}} ->
      Pid ! {reply, pollution:getDailyMean(Date, Type, Monitor)},
      loop(Monitor);

    {request, Pid, getDailyAverageDataCount, Name} ->
      Pid ! {reply, pollution:getDailyAverageDataCount(Name, Monitor)},
      loop(Monitor);

    {request, _, stop} -> ok
  end.

stop() -> server ! {request, self(), stop}.

send(Function, Data) ->
  server ! {request, self(), Function, Data},
  receive
    {reply, Reply} -> Reply
  end.

addStation(Name, {X, Y}) -> send(addStation, {Name, {X, Y}}).

addValue({X, Y}, {Date, Time}, Type, Value) -> send(addValue, {{X, Y}, {Date, Time}, Type, Value});
addValue(Name, {Date, Time}, Type, Value) -> send(addValue, {Name, {Date, Time}, Type, Value}).

removeValue({X, Y}, {Date, Time}, Type) -> send(removeValue, {{X, Y}, {Date, Time}, Type});
removeValue(Name, {Date, Time}, Type) -> send(removeValue, {Name, {Date, Time}, Type}).

getOneValue({X, Y}, {Date, Time}, Type) -> send(getOneValue, {{X, Y}, {Date, Time}, Type});
getOneValue(Name, {Date, Time}, Type) -> send(getOneValue, {Name, {Date, Time}, Type}).

getStationMean({X, Y}, Type) -> send(getStationMean, {{X, Y}, Type});
getStationMean(Name, Type) -> send(getStationMean, {Name, Type}).

getDailyMean(Date, Type) -> send(getDailyMean, {Date, Type}).

getDailyAverageDataCount(Name) -> send(getDailyAverageDataCount, Name).


test() ->
  pollutionServer:start(),
  pollutionServer:addStation("Stacja", {10, 20}),
  pollutionServer:addStation("Aleja Slowackiego", {50.2345, 18.3445}),
  pollutionServer:addValue({50.2345, 18.3445}, {{2019, 4, 11}, {23, 1, 57}}, "PM10", 59),
  pollutionServer:addValue("Aleja Slowackiego", {{2019, 4, 11}, {23, 1, 58}}, "PM2.5", 113),
  pollutionServer:addValue({50.2345, 18.3445}, {{2019, 4, 11}, {23, 1, 59}}, "PM10", 61),
  pollutionServer:addValue({50.2345, 18.3445}, {{2019, 4, 10}, {23, 2, 00}}, "PM2.5", 115),
  pollutionServer:addStation("Aleja Slowackiego 2", {50.2346, 18.3446}),
  pollutionServer:addValue("Aleja Slowackiego 2", {{2019, 4, 11}, {23, 1, 00}}, "PM2.5", 108),
  pollutionServer:getDailyMean({2019, 4, 11}, "PM2.5"),
  pollutionServer:getDailyMean({2019, 4, 11}, "PM10"),
  pollutionServer:getOneValue("Aleja Slowackiego", {{2019, 4, 11}, {23, 1, 58}}, "PM2.5"),
  pollutionServer:getStationMean("Aleja Slowackiego", "PM2.5"),
  pollutionServer:getDailyAverageDataCount("Aleja Slowackiego").