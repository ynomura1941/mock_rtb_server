%% -*- coding: utf-8 -*-
-module(utils).
-compile(export_all).
-include("../include/common.hrl").

gender_to_bits(G) ->
  case G of
    "M" -> 1;
    "F" -> 2;
    "O" -> 3;
    _   -> 3
  end.
to_integer(IS) ->
  {Int,_} = string:to_integer(IS),
  Int.
to_float(FS) ->
  {Float,_} = string:to_integer(FS),
  Float.

datetime_parse(DT) ->
  [DSTR,TSTR] = string:tokens(DT," "),
  [YS,MOS,DS] = string:tokens(DSTR,"-"),
  [HS,MIS,SS] = string:tokens(TSTR,":"),
  {{to_integer(YS), to_integer(MOS), to_integer(DS)},{to_integer(HS), to_integer(MIS), to_integer(SS)}}.

categories_to_bits([H|T]) ->
  N =list_to_integer(re:replace(H,"IAB","",[{return,list}])),
  one_bsl(N -1) + categories_to_bits(T);
categories_to_bits([])->0.

list_to_generation([H|T]) when is_list(H) =:= true ->
  generation_to_bit(to_integer(H)) + list_to_generation(T);
list_to_generation([H|T]) when is_integer(H) =:= true ->
  generation_to_bit(H) + list_to_generation(T);
list_to_generation([]) -> 0.

generation_to_bit(Int) ->
  if 
    Int =< 100 , Int > 0 ->
      one_bsl(Int div 10);
    true ->
      -1
  end.
year_to_generation(Int) ->
   { {Year, _, _},_} = calendar:local_time(),
  Gen = Year - Int,
  generation_to_bit(Gen).

one_bsl(Int) ->
  1 bsl Int.
