%% -*- coding: utf-8 -*-
-module(rtb_ad).
-include_lib("stdlib/include/qlc.hrl").

-include("../include/common.hrl").
-include("../include/rtb_ad.hrl").
-export([setup/1, start/0, find_by_adid/1,
      insert_from_file/1, insert/1, add_rtb_ad/13, search/1]).

setup(Nodes) ->
  ?LOG(Nodes),
  ok = mnesia:create_schema(Nodes),
  application:start(mnesia),
  mnesia:create_table(rtb_ad, [
    {attributes,record_info(fields,rtb_ad)},
    {disc_copies, Nodes}
    ]),
  application:stop(mnesia),
  ok.

start() ->
  mnesia:wait_for_tables([rtb_ad],5000),
  ok.

find_by_adid(Adid) ->
  F = fun() -> mnesia:read({rtb_ad, Adid}) end,
  case mnesia:activity(transaction,F) of
    [] -> undefined;
    [#rtb_ad{adid=Adid, price=P, contents=C, domain=D, h=H, w=W, gender=Gen, generations=G,
      self_categories=Sc, target_categories=Tc, ad_type=At, start_datetime=Sd, end_datetime=Ed}]
        -> {Adid, P, C, D, H, W, Gen, G, Sc, Tc, At, Sd, Ed}
  end.
insert_from_file(File) ->
  case import:start(File) of
    {error,Reason} ->
      io:format("~p~n", [Reason]),
      error;
    {ok,RtbData} ->
      io:format("loaded: ~p~n", [RtbData]),
      insert(RtbData)
  end.
insert(RtbData) ->
  F = fun() ->
    mnesia:write(RtbData)
  end,
  mnesia:activity(transaction,F).

add_rtb_ad(Adid,Price,Contents,Domain,Height,Width,Gender,Generation,
  SelfCategories,TargetCategories,AdType,StartDatetime,EndDatetime) ->
  ConvertedGender           = utils:gender_to_bits(Gender),
  ConvertedGeneration       = utils:list_to_generation(Generation),
  ConvertedSelfCategories   = utils:categories_to_bits(SelfCategories),
  ConvertedTargetCategories = utils:categories_to_bits(TargetCategories),
  ConvertedStartDatetime    = utils:datetime_parse(StartDatetime),
  ConvertedEndDatetime    = utils:datetime_parse(EndDatetime),
  F = fun() ->
    mnesia:write(#rtb_ad{
      adid = Adid, price = Price, contents = Contents, domain = Domain, h = Height, w = Width,
      gender = ConvertedGender, generations = ConvertedGeneration,
      self_categories = ConvertedSelfCategories, target_categories = ConvertedTargetCategories,
      ad_type = AdType,
      start_datetime = ConvertedStartDatetime, end_datetime = ConvertedEndDatetime
      })
  end,
  mnesia:activity(transaction,F).

search(Cond) ->
  ?LOG(Cond),
  {Current, Imp, User} = Cond,
  {IId, CW, CH, CBattrBits}= Imp,

  {YearBits,GenderBits} = User,
  F = fun() ->
    Q1 = qlc:q([ Rec || Rec <- mnesia:table(rtb_ad),
      Rec#rtb_ad.h =:= CH,
      Rec#rtb_ad.w =:= CW,
      Rec#rtb_ad.generations band YearBits > 0,
      Rec#rtb_ad.gender band GenderBits > 0,
      Rec#rtb_ad.self_categories band CBattrBits =:= 0,
      Rec#rtb_ad.start_datetime =< Current,
      Rec#rtb_ad.end_datetime >= Current]),
    Q2 = qlc:sort(Q1, {order, fun(Ad1,Ad2) -> Ad1#rtb_ad.price > Ad2#rtb_ad.price end}),
    CR = qlc:cursor(Q2),
    R = qlc:next_answers(CR,1),
    qlc:delete_cursor(CR),
    ?LOG(R),
    [{IId, Adid, P, C, D, H, W, Gen, G, Sc, Tc, At, Sd, Ed} || 
      #rtb_ad{adid=Adid, price=P, contents=C, domain=D, h=H, w=W, gender=Gen, generations=G,
            self_categories=Sc, target_categories=Tc, ad_type=At, start_datetime=Sd, end_datetime=Ed} <- R]
  end,
  mnesia:activity(transaction,F).

