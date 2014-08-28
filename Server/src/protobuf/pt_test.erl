%%%------------------------------------
%%% @author 严利宏 <542430172@qq.com>
%%% @copyright jieyou 2014.08.25
%%% @doc description.
%%% @end
%%%------------------------------------

-module(pt_test).
-compile(export_all).

-include("pt1_pb.hrl").

encode() ->
	Person = #person{ name="John"},
	pt1_pb:encode_person(Person).

decode() ->
	Data = encode(),
	pt1_pb:decode_person(Data).

encode_repeat() ->
	RepeatData =
	[
		#person_phonenumber{number="12345"},
        #person_phonenumber{number="22222", type=#person_phonenumber_phonetype{mobile=11}},
		#person_phonenumber{number="11",    type=#person_phonenumber_phonetype{mobile=11, home=22, work=33}}
	],
	Person = #person{id=1, name="me", email="hh", phone=RepeatData},
	pt1_pb:encode_person(Person).
	
decode_repeat() ->
	Data = encode_repeat(),
	pt1_pb:decode_person(Data).

