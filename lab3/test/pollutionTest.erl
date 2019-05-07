-module(pollutionTest).

-include_lib("eunit/include/eunit.hrl").

createMonitor_test() ->
  ?assert(pollution:createMonitor() =:= #{}).

addStations_test() ->
  P = pollution:createMonitor(),
  P1 = pollution:addStation("Aleja Slowackiego", {50.2345, 18.3445}, P),
  P2 = pollution:addStation("Aleja Slowackiego 2", {50.2346, 18.3446}, P1),
  [?assert(P1 =:= #{{station, "Aleja Slowackiego", {coords, 50.2345, 18.3445}} => []}),
    ?assert(P2 =:= #{{station, "Aleja Slowackiego", {coords, 50.2345, 18.3445}} => [], {station, "Aleja Slowackiego 2", {coords, 50.2346, 18.3446}} => []})].

addStations2_test() ->
  P = pollution:createMonitor(),
  P1 = pollution:addStation("Aleja Slowackiego", {50.2345, 18.3445}, P),
  P2 = pollution:addStation("Aleja Slowackiego", {50.2346, 18.3446}, P1),
  ?assertMatch({error, _}, P2).


addValue_test() ->
  P = pollution:createMonitor(),
  P1 = pollution:addStation("Aleja Slowackiego", {50.2345, 18.3445}, P),
  P2 = pollution:addValue({50.2345, 18.3445}, {{2019, 4, 11}, {23, 1, 57}}, "PM10", 59, P1),
  P3 = pollution:addValue("Aleja Slowackiego", {{2019, 4, 11}, {23, 1, 58}}, "PM2.5", 113, P2),
  [?assert(P2 =:= #{{station, "Aleja Slowackiego", {coords, 50.2345, 18.3445}} =>[{measurement, {2019, 4, 11}, {23, 1, 57}, "PM10", 59}]}),
    ?assert(P3 =:= #{{station, "Aleja Slowackiego", {coords, 50.2345, 18.3445}} =>[{measurement, {2019, 4, 11}, {23, 1, 57}, "PM10", 59}, {measurement, {2019, 4, 11}, {23, 1, 58}, "PM2.5", 113}]})].

addValue2_test() ->
  P = pollution:createMonitor(),
  P1 = pollution:addStation("Aleja Slowackiego", {50.2345, 18.3445}, P),
  P2 = pollution:addValue("Aleja Slowackiego", {{2019, 4, 11}, {23, 1, 57}}, "PM10", 59, P1),
  P3 = pollution:addValue("Aleja Slowackiego", {{2019, 4, 11}, {23, 1, 57}}, "PM10", 60, P2),
  P4 = pollution:addValue("Aleja Slowackiego 2", {{2019, 4, 11}, {23, 1, 57}}, "PM5", 59, P2),
  [?assertMatch({error, _}, P3), ?assertMatch({error, _}, P4)].


removeValue_test() ->
  P = pollution:createMonitor(),
  P1 = pollution:addStation("Aleja Slowackiego", {50.2345, 18.3445}, P),
  P2 = pollution:addValue({50.2345, 18.3445}, {{2019, 4, 11}, {23, 1, 57}}, "PM10", 59, P1),
  P3 = pollution:addValue("Aleja Slowackiego", {{2019, 4, 11}, {23, 1, 58}}, "PM2.5", 113, P2),
  P4 = pollution:removeValue("Aleja Slowackiego", {{2019, 4, 11}, {23, 1, 58}}, "PM2.5", P3),
  P5 = pollution:removeValue({50.2345, 18.3445}, {{2019, 4, 11}, {23, 1, 57}}, "PM10", P4),
  [?assert(P4 =:= #{{station, "Aleja Slowackiego", {coords, 50.2345, 18.3445}} =>[{measurement, {2019, 4, 11}, {23, 1, 57}, "PM10", 59}]}),
    ?assert(P5 =:= #{{station, "Aleja Slowackiego", {coords, 50.2345, 18.3445}} =>[]})].

removeValue2_test() ->
  P = pollution:createMonitor(),
  P1 = pollution:addStation("Aleja Slowackiego", {50.2345, 18.3445}, P),
  P2 = pollution:addValue("Aleja Slowackiego", {{2019, 4, 11}, {23, 1, 58}}, "PM2.5", 113, P1),
  P3 = pollution:removeValue("Aleja Slowackiego", {{2019, 4, 11}, {23, 1, 58}}, "PM10", P2),
  P4 = pollution:removeValue("Aleja Slowackiego 2", {{2019, 4, 11}, {23, 1, 58}}, "PM2.5", P2),
  [?assertMatch({error, _}, P3), ?assertMatch({error, _}, P4)].


getOneValue_test() ->
  P = pollution:createMonitor(),
  P1 = pollution:addStation("Aleja Slowackiego", {50.2345, 18.3445}, P),
  P2 = pollution:addValue({50.2345, 18.3445}, {{2019, 4, 11}, {23, 1, 57}}, "PM10", 59, P1),
  P3 = pollution:addValue("Aleja Slowackiego", {{2019, 4, 11}, {23, 1, 58}}, "PM2.5", 113, P2),
  P4 = pollution:getOneValue("Aleja Slowackiego", {{2019, 4, 11}, {23, 1, 58}}, "PM2.5", P3),
  P5 = pollution:getOneValue({50.2345, 18.3445}, {{2019, 4, 11}, {23, 1, 57}}, "PM10", P3),
  [?assert(P4 =:= 113), ?assert(P5 =:= 59)].

getOneValue2_test() ->
  P = pollution:createMonitor(),
  P1 = pollution:addStation("Aleja Slowackiego", {50.2345, 18.3445}, P),
  P2 = pollution:addValue("Aleja Slowackiego", {{2019, 4, 11}, {23, 1, 57}}, "PM10", 59, P1),
  P3 = pollution:getOneValue("Aleja Slowackiego 2", {{2019, 4, 11}, {23, 1, 57}}, "PM10", P2),
  ?assertMatch({error, _}, P3).


getStationMean_test() ->
  P = pollution:createMonitor(),
  P1 = pollution:addStation("Aleja Slowackiego", {50.2345, 18.3445}, P),
  P2 = pollution:addValue({50.2345, 18.3445}, {{2019, 4, 11}, {23, 1, 57}}, "PM10", 59, P1),
  P3 = pollution:addValue("Aleja Slowackiego", {{2019, 4, 11}, {23, 1, 58}}, "PM2.5", 113, P2),
  P4 = pollution:addValue({50.2345, 18.3445}, {{2019, 4, 11}, {23, 1, 59}}, "PM10", 61, P3),
  P5 = pollution:addValue({50.2345, 18.3445}, {{2019, 4, 11}, {23, 2, 00}}, "PM2.5", 115, P4),
  P6 = pollution:getStationMean("Aleja Slowackiego", "PM2.5", P5),
  P7 = pollution:getStationMean({50.2345, 18.3445}, "PM10", P5),
  [?assert(P6 =:= 114.0), ?assert(P7 =:= 60.0)].

getStationMean2_test() ->
  P = pollution:createMonitor(),
  P1 = pollution:addStation("Aleja Slowackiego", {50.2345, 18.3445}, P),
  P2 = pollution:addValue("Aleja Slowackiego", {{2019, 4, 11}, {23, 1, 58}}, "PM2.5", 113, P1),
  P3 = pollution:getStationMean("Aleja Slowackiego", "PM10", P2),
  P4 = pollution:getStationMean("Aleja Slowackiego 2", "PM2.5", P2),
  [?assertMatch({error, _}, P3), ?assertMatch({error, _}, P4)].


getDailyMean_test() ->
  P = pollution:createMonitor(),
  P1 = pollution:addStation("Aleja Slowackiego", {50.2345, 18.3445}, P),
  P2 = pollution:addValue({50.2345, 18.3445}, {{2019, 4, 11}, {23, 1, 57}}, "PM10", 59, P1),
  P3 = pollution:addValue("Aleja Slowackiego", {{2019, 4, 11}, {23, 1, 58}}, "PM2.5", 113, P2),
  P4 = pollution:addValue({50.2345, 18.3445}, {{2019, 4, 11}, {23, 1, 59}}, "PM10", 61, P3),
  P5 = pollution:addValue({50.2345, 18.3445}, {{2019, 4, 11}, {23, 2, 00}}, "PM2.5", 115, P4),
  P6 = pollution:addStation("Aleja Slowackiego 2", {50.2346, 18.3446}, P5),
  P7 = pollution:addValue("Aleja Slowackiego 2", {{2019, 4, 11}, {23, 1, 00}}, "PM2.5", 108, P6),
  P8 = pollution:getDailyMean({2019, 4, 11}, "PM2.5", P7),
  P9 = pollution:getDailyMean({2019, 4, 11}, "PM10", P7),
  [?assert(P8 =:= 112.0), ?assert(P9 =:= 60.0)].

getDailyMean2_test() ->
  P = pollution:createMonitor(),
  P1 = pollution:addStation("Aleja Slowackiego", {50.2345, 18.3445}, P),
  P2 = pollution:addValue("Aleja Slowackiego", {{2019, 4, 11}, {23, 1, 58}}, "PM2.5", 113, P1),
  P3 = pollution:getDailyMean({2019, 4, 11}, "PM10", P2),
  ?assertMatch({error, _}, P3).


getDailyAverageDataCount_test() ->
  P = pollution:createMonitor(),
  P1 = pollution:addStation("Aleja Slowackiego", {50.2345, 18.3445}, P),
  P2 = pollution:addValue({50.2345, 18.3445}, {{2019, 4, 11}, {23, 1, 57}}, "PM10", 59, P1),
  P3 = pollution:addValue("Aleja Slowackiego", {{2019, 4, 11}, {23, 1, 58}}, "PM2.5", 113, P2),
  P4 = pollution:addValue({50.2345, 18.3445}, {{2019, 4, 11}, {23, 1, 59}}, "PM10", 61, P3),
  P5 = pollution:addValue({50.2345, 18.3445}, {{2019, 4, 10}, {23, 2, 00}}, "PM2.5", 115, P4),
  P6 = pollution:addStation("Aleja Slowackiego 2", {50.2346, 18.3446}, P5),
  P7 = pollution:addValue("Aleja Slowackiego 2", {{2019, 4, 11}, {23, 1, 00}}, "PM2.5", 108, P6),
  P8 = pollution:getDailyAverageDataCount("Aleja Slowackiego", P7),
  P9 = pollution:getDailyAverageDataCount("Aleja Slowackiego 2", P7),
  [?assert(P8 =:= 2.0), ?assert(P9 =:= 1.0)].

getDailyAverageDataCount2_test() ->
  P = pollution:createMonitor(),
  P1 = pollution:addStation("Aleja Slowackiego", {50.2345, 18.3445}, P),
  P2 = pollution:getDailyAverageDataCount("Aleja Slowackiego", P1),
  P3 = pollution:getDailyAverageDataCount("Aleja Slowackiego 2", P1),
  [?assertMatch({error, _}, P2), ?assertMatch({error, _}, P3)].