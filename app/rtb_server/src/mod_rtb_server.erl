%% -*- coding: utf-8 -*-
-module(mod_rtb_server).
-export([start/1, out/1, out/2]).

-include("../../deps/yaws/include/yaws.hrl").
-include("../../deps/yaws/include/yaws_api.hrl").
-include("../include/common.hrl").




start(Args) ->
  ?LOG(Args),
  DBDir = proplists:get_value("mnesia_dir", Args#sconf.opaque),
  application:set_env(mnesia, dir, DBDir),
  application:start(mnesia),
  rtb_ad:start(),
  response_id:init().

out(Args) ->
  out((Args#arg.req)#http_request.method, Args).

out('POST', Args) ->
  case Args#arg.clidata of
    <<>> ->
      error_json();
    PostData ->
      ResJson = rtb_service:execute(PostData),
      {content, "application/json; charset=UTF-8", ResJson}
  end;

out(Method, _Args) ->
  short_response(405, "unsupported method ~p", [Method]).

short_response(Code, Text) ->
  [{status, Code}, {content, "text/plain", Text}].
short_response(Code, Fmt, Vals) ->
    short_response(Code, lists:flatten(io_lib:format(Fmt, Vals))).

error_json() -> 
  ResId = list_to_binary(response_id:gen()),

  ErrJsonStart = <<"{bidid: ">>,
  ErrJsonEnd   = <<"}">>,
  {content, "application/json; charset=UTF-8", <<ErrJsonStart/binary, ResId/binary, ErrJsonEnd/binary>>}.
