-module(rtb_ad_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../include/rtb_ad.hrl").


start() ->
  mnesia:create_schema([node()]),
  application:start(mnesia),
  mnesia:create_table(rtb_ad, [
    {attributes,record_info(fields,rtb_ad)}
  ]),
  mnesia:wait_for_tables([rtb_ad],5000).

stop() ->
  application:stop(mnesia).


add_rtb_ad_test() ->
  start(),
  ?assertEqual(ok, rtb_ad:add_rtb_ad(1,50.0, <<"<a href=\"http://adingo.jp\">test</a>">>, <<"adingo.jp">>, 100,100,"O",
                                              [20,30], ["IAB1"], ["IAB1"], 1, "2012-09-01 00:00:00", "2012-09-30 23:59:59")),
  ?assertEqual({1, 50.0, <<"<a href=\"http://adingo.jp\">test</a>">>, <<"adingo.jp">>, 100,100, 3, 12, 1, 1, 1,
      {{2012,9,1},{0,0,0}}, {{2012,9,30},{23,59,59}}}, rtb_ad:find_by_adid(1)),
  stop().

search_exist_1_test() ->
  start(),
  ?assertEqual(ok, rtb_ad:add_rtb_ad(1,50.0, <<"<a href=\"http://adingo.jp\">test</a>">>, <<"adingo.jp">>, 100,100,"O",
                                              [20,30], ["IAB1"], ["IAB1"], 1, "2012-09-01 00:00:00", "2012-09-30 23:59:59")),

  [Rec] = rtb_ad:search({calendar:local_time(),
                        {"impId",100,100,0}, { utils:year_to_generation(utils:year_to_generation(0)), utils:gender_to_bits("O")}}),
  ?assertEqual("impId", element(1,Rec)),
  stop().
search_exist_2_test() ->
  start(),
  ?assertEqual(ok, rtb_ad:add_rtb_ad(1,50.0, <<"<a href=\"http://adingo.jp\">test</a>">>, <<"adingo.jp">>, 100,100,"O",
                                              [20,30], ["IAB1"], ["IAB1"], 1, "2012-09-01 00:00:00", "2012-09-30 23:59:59")),

  [Rec] = rtb_ad:search({calendar:local_time(), 
                        {"impId",100,100,0},{ utils:year_to_generation(1980),utils:gender_to_bits("O")}}),
  ?assertEqual("impId", element(1,Rec)),
  stop().

search_exist_3_test() ->
  start(),
  ?assertEqual(ok, rtb_ad:add_rtb_ad(1,50.0, <<"<a href=\"http://adingo.jp\">test</a>">>, <<"adingo.jp">>, 100,100,"A",
                                              [20,30], ["IAB1"], ["IAB1"], 1, "2012-09-01 00:00:00", "2012-09-30 23:59:59")),

  [Rec] = rtb_ad:search({calendar:local_time(), 
                        {"impId",100,100,0},{ utils:year_to_generation(0),utils:gender_to_bits("O")}}),
  ?assertEqual("impId", element(1,Rec)),
  stop().

search_exist_4_test() ->
  start(),
  ?assertEqual(ok, rtb_ad:add_rtb_ad(1,50.0, <<"<a href=\"http://adingo.jp\">test</a>">>, <<"adingo.jp">>, 100,100,"M",
                                              [20,30], ["IAB1"], ["IAB1"], 1, "2012-09-01 00:00:00", "2012-09-30 23:59:59")),

  [Rec] = rtb_ad:search({calendar:local_time(), 
                        {"impId",100,100,0},{ utils:year_to_generation(0),utils:gender_to_bits("M")}}),
  ?assertEqual("impId", element(1,Rec)),
  stop().

search_exist_5_test() ->
  start(),
  ?assertEqual(ok, rtb_ad:add_rtb_ad(1,50.0, <<"<a href=\"http://adingo.jp\">test</a>">>, <<"adingo.jp">>, 100,100,"M",
                                              [20,30], ["IAB1"], ["IAB1"], 1, "2012-09-01 00:00:00", "2012-09-30 23:59:59")),

  [Rec] = rtb_ad:search({calendar:local_time(), 
                        {"impId",100,100, utils:categories_to_bits(["IAB2"])},{ utils:year_to_generation(0),utils:gender_to_bits("M")}}),
  ?assertEqual("impId", element(1,Rec)),
  stop().



search_empty_1_test() ->
  start(),
  ?assertEqual(ok, rtb_ad:add_rtb_ad(1,50.0, <<"<a href=\"http://adingo.jp\">test</a>">>, <<"adingo.jp">>, 100,100,"O",
                                              [20,30], ["IAB1"], ["IAB1"], 1, "2012-09-01 00:00:00", "2012-09-30 23:59:59")),

  ?assertEqual([], rtb_ad:search({calendar:local_time(), 
                  {"impId",120,100,0},{ utils:year_to_generation(0), utils:gender_to_bits("O")}})),
  stop().

search_empty_2_test() ->
  start(),
  ?assertEqual(ok, rtb_ad:add_rtb_ad(1,50.0, <<"<a href=\"http://adingo.jp\">test</a>">>, <<"adingo.jp">>, 100,100,"M",
                                              [20,30], ["IAB1"], ["IAB1"], 1, "2012-09-01 00:00:00", "2012-09-30 23:59:59")),

  ?assertEqual([], rtb_ad:search({calendar:local_time(), 
                  {"impId",100,100,0},{ utils:year_to_generation(0), utils:gender_to_bits("F")}})),
  stop().
search_empty_3_test() ->
  start(),
  ?assertEqual(ok, rtb_ad:add_rtb_ad(1,50.0, <<"<a href=\"http://adingo.jp\">test</a>">>, <<"adingo.jp">>, 100,100,"M",
                                              [20,30], ["IAB1"], ["IAB1"], 1, "2012-09-01 00:00:00", "2012-09-30 23:59:59")),

  ?assertEqual([], rtb_ad:search({calendar:local_time(), 
                  {"impId",100,100, utils:categories_to_bits(["IAB1"])},{ utils:year_to_generation(0), utils:gender_to_bits("O")}})),
  stop().
