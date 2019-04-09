-module(pollution).
-export([createMonitor/0]).
-export([addStation/3]).
-export([addValue/5]).
-export([removeValue/4]).
-export([getOneValue/4]).
-export([getStationMean/3]).
-export([getDailyMean/3]).
-export([getDailyAverageDataCount/2]).
-export([test/0]).

-record(station, {name, coords}).
-record(coords, {x, y}).
-record(measurement, {date, time, type, value}).


createMonitor() -> #{}.


existStation([], _, _) -> false;
existStation([#station{name = Name} | _], Name, _) -> true;
existStation([#station{coords = #coords{x = X, y = Y}} | _], _, {X, Y}) -> true;
existStation([_ | T], Name, {X, Y}) -> existStation(T, Name, {X, Y}).


addStation(Name, {X, Y}, Monitor) ->
  case existStation(maps:keys(Monitor), Name, {X, Y}) of
    true -> {error, "Taka stacja juz istnieje"};
    false -> Monitor#{#station{name = Name, coords = #coords{x = X, y = Y}} => []}
  end.


existMeasurement(_, _, []) -> false;
existMeasurement({Date, Time}, Type, [#measurement{date = Date, time = Time, type = Type} | _]) -> true;
existMeasurement({Date, Time}, Type, [_ | T]) -> existMeasurement({Date, Time}, Type, T).


getStation(_, []) -> {error, "Brak stacji"};
getStation({coords, X, Y}, [H | T]) -> case H of
                                         #station{coords = {coords, X, Y}} -> H;
                                         _ -> getStation({coords, X, Y}, T)
                                       end;
getStation(Name, [H | T]) -> case H of
                               #station{name = Name} -> H;
                               _ -> getStation(Name, T)
                             end.


addValue({X, Y}, {Date, Time}, Type, Value, Monitor) ->
  case existStation(maps:keys(Monitor), n, {X, Y}) of
    false -> {error, "Taka stacja juz istnieje"};
    true ->
      ActStation = getStation({coords, X, Y}, maps:keys(Monitor)),
      ActMeasurements = maps:get(ActStation, Monitor),
      case existMeasurement({Date, Time}, Type, ActMeasurements) of
        true -> {error, "Takie dane juz istnieja"};
        false ->
          Monitor#{ActStation := ActMeasurements ++ [#measurement{date = Date, time = Time, type = Type, value = Value}]}
      end
  end;

addValue(Name, {Date, Time}, Type, Value, Monitor) ->
  case existStation(maps:keys(Monitor), Name, {x, y}) of
    false -> {error, "Taka stacja juz istnieje"};
    true ->
      ActStation = getStation(Name, maps:keys(Monitor)),
      ActMeasurements = maps:get(ActStation, Monitor),
      case existMeasurement({Date, Time}, Type, ActMeasurements) of
        true -> {error, "Takie dane juz istnieja"};
        false ->
          Monitor#{ActStation := ActMeasurements ++ [#measurement{date = Date, time = Time, type = Type, value = Value}]}
      end
  end.


getMeasurement(_, _, []) -> {error, "Pomiar nie istnieje"};
getMeasurement({Date, Time}, Type, [H | T]) -> case H of
                                                 #measurement{date = Date, time = Time, type = Type} -> H;
                                                 _ -> getMeasurement({Date, Time}, Type, T)
                                               end.

removeValue({X, Y}, {Date, Time}, Type, Monitor) -> case existStation(maps:keys(Monitor), n, {X, Y}) of
                                                      false -> {error, "Stacja nie istnieje"};
                                                      true ->
                                                        ActStation = getStation({coords, X, Y}, maps:keys(Monitor)),
                                                        ActMeasurements = maps:get(ActStation, Monitor),
                                                        case existMeasurement({Date, Time}, Type, ActMeasurements) of
                                                          true -> Monitor#{ActStation := ActMeasurements -- [getMeasurement({Date, Time}, Type, ActMeasurements)]};
                                                          false -> {error, "Pomiar nie istnieje"}
                                                        end
                                                    end;

removeValue(Name, {Date, Time}, Type, Monitor) -> case existStation(maps:keys(Monitor), Name, {x, y}) of
                                                    false -> {error, "Stacja nie istnieje"};
                                                    true ->
                                                      ActStation = getStation(Name, maps:keys(Monitor)),
                                                      ActMeasurements = maps:get(ActStation, Monitor),
                                                      case existMeasurement({Date, Time}, Type, ActMeasurements) of
                                                        true -> Monitor#{ActStation := ActMeasurements -- [getMeasurement({Date, Time}, Type, ActMeasurements)]};
                                                        false -> {error, "Pomiar nie istnieje"}
                                                      end
                                                  end.


getValue(_, _, []) -> io:format("Pomiar nie istnieje");
getValue({Date, Time}, Type, [#measurement{date = Date, time = Time, type = Type, value = Value} | _]) -> Value;
getValue({Date, Time}, Type, [_ | T]) -> getValue({Date, Time}, Type, T).

getOneValue({X, Y}, {Date, Time}, Type, Monitor) ->
  case existStation(maps:keys(Monitor), n, {X, Y}) of
    false -> {error, "Stacja nie istnieje"};
    true -> getValue({Date, Time}, Type, maps:get(getStation({coords, X, Y}, maps:keys(Monitor)), Monitor))
  end;

getOneValue(Name, {Date, Time}, Type, Monitor) ->
  case existStation(maps:keys(Monitor), Name, {x, y}) of
    false -> {error, "Stacja nie istnieje"};
    true -> getValue({Date, Time}, Type, maps:get(getStation(Name, maps:keys(Monitor)), Monitor))
  end.


countStationMean(_, [], {Sum, Amount}) -> {Sum, Amount};
countStationMean(Type, [#measurement{type = Type, value = Value} | T], {Sum, Amount}) ->
  countStationMean(Type, T, {Sum + Value, Amount + 1});
countStationMean(Type, [_ | T], {Sum, Amount}) -> countStationMean(Type, T, {Sum, Amount}).

getStationMean({X, Y}, Type, Monitor) ->
  case existStation(maps:keys(Monitor), n, {X, Y}) of
    false -> {error, "Stacja nie istnieje"};
    true ->
      ActStation = getStation({coords, X, Y}, maps:keys(Monitor)),
      ActMeasurements = maps:get(ActStation, Monitor),
      {Sum, Amount} = countStationMean(Type, ActMeasurements, {0, 0}),
      case Amount of
        0 -> {error, "Brak pomiarow"};
        _ -> Sum / Amount
      end
  end;

getStationMean(Name, Type, Monitor) ->
  case existStation(maps:keys(Monitor), Name, {x, y}) of
    false -> {error, "Stacja nie istnieje"};
    true ->
      ActStation = getStation(Name, maps:keys(Monitor)),
      ActMeasurements = maps:get(ActStation, Monitor),
      {Sum, Amount} = countStationMean(Type, ActMeasurements, {0, 0}),
      case Amount of
        0 -> {error, "Brak pomiarow"};
        _ -> Sum / Amount
      end
  end.


countStationMeanDaily(_, _, [], {Sum, Amount}) -> {Sum, Amount};
countStationMeanDaily(Type, Date, [#measurement{date = Date, type = Type, value = Value} | T], {Sum, Amount}) ->
  countStationMeanDaily(Type, Date, T, {Sum + Value, Amount + 1});
countStationMeanDaily(Type, Date, [_ | T], {Sum, Amount}) -> countStationMeanDaily(Type, Date, T, {Sum, Amount}).

getDailyMean(Date, Type, Monitor) ->
  ActMeasurements = lists:append(maps:values(Monitor)),
  {Sum, Amount} = countStationMeanDaily(Type, Date, ActMeasurements, {0, 0}),
  case Amount of
    0 -> {error, "Brak pomiarow"};
    _ -> Sum / Amount
  end.


countDailyDataCount([], T, AmountOfUpdates) -> {T, AmountOfUpdates};
countDailyDataCount([#measurement{date = Date, type = Type, value = Value} | T], Map, AmountOfUpdates) ->
  case maps:is_key(Date, Map) of
    true -> countDailyDataCount(T, Map#{Date := [{Type, Value}] ++ maps:get(Date, Map)}, AmountOfUpdates + 1);
    false -> countDailyDataCount(T, maps:put(Date, [{Type, Value}], Map), AmountOfUpdates + 1)
  end;
countDailyDataCount([_ | T], Map, AmountOfUpdates) -> countDailyDataCount(T, Map, AmountOfUpdates).

getDailyAverageDataCount(Name, Monitor) ->
  case existStation(maps:keys(Monitor), Name, {x, y}) of
    false -> {error, "Stacja nie istnieje"};
    true -> ActStation = getStation(Name, maps:keys(Monitor)),
      ActMeasurements = maps:get(ActStation, Monitor),
      {A, AmountOfUpdates} = countDailyDataCount(ActMeasurements, #{}, 0),
      AmountOfDays = length(maps:keys(A)),
      case AmountOfDays of
        0 -> {error, "Brak pomiarow"};
        _ -> AmountOfUpdates / AmountOfDays
      end
  end.




test() ->
  P = pollution:createMonitor(),
  P1 = pollution:addStation("Aleja Slowackiego", {50.2345, 18.3445}, P),
  P2 = pollution:addValue({50.2345, 18.3445}, calendar:local_time(), "PM10", 59, P1),
  P3 = pollution:addValue("Aleja Slowackiego", calendar:local_time(), "PM2.5", 113, P2),
  P3.
