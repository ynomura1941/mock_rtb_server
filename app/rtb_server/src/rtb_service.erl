-module(rtb_service).
-include("../include/common.hrl").

-export([execute/1]).

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

ad_search(Imp, {Site, App, User, Device, Restrict}) ->
  Current = calendar:local_time(),
  ConvImp = {proplists:get_value(<<"impid">>,Imp), proplists:get_value(<<"w">>,Imp), 
              proplists:get_value(<<"h">>,Imp), proplists:get_value(<<"battr">>,Imp)},
  ConvUser = {proplists:get_value(<<"yob">>,User, 0), proplists:get_value(<<"gender">>,User, "O")},

  case rtb_ad:search({Current, ConvImp,ConvUser}) of
    Results -> convert_ads(Results)
  end.
  
