-module(rtb_service_tests).
-include_lib("eunit/include/eunit.hrl").

rest_keys_test() ->

  Restrict = [{<<"bcat">>, [{struct,[{<<"iab">>,<<"IAB4">>}]},{struct,[{<<"iab">>,<<"IAB5">>}]}]}],
  {Result,_,_} = rtb_service:rest_keys(Restrict),
  ?assertEqual(utils:categories_to_bits(["IAB4","IAB5"]), Result).
