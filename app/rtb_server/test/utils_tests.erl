-module(utils_test).
-include_lib("eunit/include/eunit.hrl").

gender_to_bits_other_test() -> 3 = utils:gender_to_bits("A").
gender_to_bits_Oto3_test() -> 3 = utils:gender_to_bits("O").
gender_to_bits_Mto1_test() -> 1 = utils:gender_to_bits("M").
gender_to_bits_Fto2_test() -> 2 = utils:gender_to_bits("F").

datetime_parse_ng_test() -> ?assertException(error, {badmatch,_}, utils:datetime_parse("2012-0101 00:00:00")).
datetime_parse_ok_test() -> {{2012,9,1},{0,0,0}} = utils:datetime_parse("2012-09-01 0:0:0").

generation_to_bit_0_test() -> 1 = utils:generation_to_bit(9).
generation_to_bit_1_test() -> 2 = utils:generation_to_bit(19).
