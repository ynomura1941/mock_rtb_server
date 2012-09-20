-module(rtb_service).
-include("../include/common.hrl").

-export([execute/1, rest_keys/1]).

execute(RowData) ->
  {struct,JsonData} = mochijson2:decode(RowData),

  {Imps, Site, App, User, Device, Restrict} = analyze(JsonData),

  ?LOG([Imps, Site, App, User, Device, Restrict]),

  AdList = fetch_ad(Imps, {Site, App, User, Device, Restrict}),

  ?LOG(AdList),
  Res = [
      {id, proplists:get_value(<<"id">>, JsonData)},
      {bidid, list_to_binary(response_id:gen())},
      {cur, <<"JPY">>},
      {seatbid, 
        case AdList of
          [] -> 0;
          _  ->
            [ {bid,
                AdList}]
        end
      }
    ],
  mochijson2:encode({struct, Res}).


convert_ads([]) -> [];
convert_ads([H|T]) ->
  [ad_to_json(H) | convert_ads(T)].
ad_to_json(Ad) ->
  {IId, Adid, P, C, D, _H, _W, _Gen, _G, _Sc, _Tc, _At, _Sd, _Ed} = Ad,
  [{impid, IId}, {price, P}, {adid, Adid}, {adm, C}, {domain,D}].


analyze(JsonData) ->
  Imps = lists:map(fun({struct,Imp}) -> Imp end, proplists:get_value(<<"imp">>, JsonData,[])),
  {struct, Site} = proplists:get_value(<<"site">>, JsonData, {struct, undefined}),
  {struct, App}  = proplists:get_value(<<"app">>, JsonData, {struct, undefined}),
  {struct, User} = proplists:get_value(<<"user">>, JsonData, {struct, undefined}),
  {struct, Device} = proplists:get_value(<<"device">>, JsonData, {struct, undefined}),
  {struct, Restrictions} = proplists:get_value(<<"restrictions">>, JsonData, {struct, undefined}),
  {Imps, Site, App, User, Device,Restrictions}.

fetch_ad([], _Opts) -> [];
fetch_ad([H|T], Opts) ->
  ad_search(H, Opts) ++ fetch_ad(T, Opts).

ad_search(Imp, {_Site, _App, User, _Device, Restrict}) ->
  Current = calendar:local_time(),
  ConvImp = imp_keys(Imp),
  ConvUser = user_keys(User),
  ConvRest = rest_keys(Restrict),

  case rtb_ad:search({Current, ConvImp,ConvUser,ConvRest}) of
    Results -> convert_ads(Results)
  end.
  
imp_keys(Imp) ->
  Battr = proplists:get_value(<<"battr">>,Imp, []),
  {proplists:get_value(<<"impid">>,Imp), proplists:get_value(<<"w">>,Imp),
    proplists:get_value(<<"h">>,Imp), utils:categories_to_bits(Battr)}.
user_keys(User) ->
  Yob = proplists:get_value(<<"yob">>,User, 0),
  Gender = proplists:get_value(<<"gender">>,User, "O"),
  {utils:year_to_generation(Yob), utils:gender_to_bits(Gender)}.

rest_keys(Restrict) ->
  BCatJson   = proplists:get_value(<<"bcat">>,Restrict, []),
  BAdvJson   = proplists:get_value(<<"badv">>,Restrict, []),
  BAdvidJson = proplists:get_value(<<"badvid">>,Restrict, []),

  BCats  = lists:map(fun({struct,Cat}) -> proplists:get_value(<<"iab">>,Cat) end, BCatJson),
  BAdvs   = lists:map(fun({struct,Adv}) -> proplists:get_value(<<"domain">>,Adv) end, BAdvJson),
  BAdvids = lists:map(fun({struct,Advid}) -> proplists:get_value(<<"id">>,Advid) end, BAdvidJson),
  {utils:categories_to_bits(BCats),BAdvs, BAdvids}.
