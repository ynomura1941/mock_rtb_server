%% -*- coding: utf-8 -*-
-module(import).
-import(utils).
-include("../include/common.hrl").
-include("../include/rtb_ad.hrl").
-export([parse/3,start/1]).

parse(In,Record,N) ->
  case file:read_line(In) of
    eof -> Record;
    {ok, Line} ->
      Row = binary:bin_to_list(binary:replace(Line, [<<"\n">>,<<"\r\n">>],<<"">>)),
      case N of
        1 ->
          Rec = Record#rtb_ad{adid=utils:to_integer(Row)};
        2 ->
          Rec = Record#rtb_ad{domain=binary:replace(Line, [<<"\n">>,<<"\r\n">>],<<"">>)};
        3 ->
          Rec = Record#rtb_ad{gender=utils:gender_to_bits(Row)};
        4 ->
          Rec = Record#rtb_ad{price=utils:to_float(Row)};
        5 ->
          Rec = Record#rtb_ad{generations=utils:list_to_generation(string:tokens(Row,","))};
        6 ->
          Rec = Record#rtb_ad{self_categories=utils:categories_to_bits(string:tokens(Row,","))};
        7 ->
          Rec = Record#rtb_ad{target_categories=utils:categories_to_bits(string:tokens(Row,","))};
        8 ->
          [W,H] = string:tokens(Row,"x"),
          Rec = Record#rtb_ad{w=utils:to_integer(W),h=utils:to_integer(H)};
        9 ->
          Rec = Record#rtb_ad{start_datetime=utils:datetime_parse(Row)};
        10 ->
          Rec = Record#rtb_ad{end_datetime=utils:datetime_parse(Row)};
        11 ->
          Rec = Record#rtb_ad{ad_type=utils:to_integer(Row)};
        _ ->
          Con= Record#rtb_ad.contents,
          case Con of
            undefined ->
              Rec = Record#rtb_ad{contents = Line};
            _ ->
              Rec = Record#rtb_ad{contents = <<Con/binary,Line/binary>>}
          end
      end,
      RtnRec = parse(In,Rec,N+1),
      RtnRec
  end.

start(FilePath) ->
  case file:open(FilePath, [read, raw,binary]) of
    {error, Reason} -> {error,Reason};
    %{error, Reason} -> ?LOG(Reason);
    {ok, In} ->
      Result = parse(In,#rtb_ad{},1),
      ?LOG([Result]),
      file:close(In),
      {ok,Result}
  end.
