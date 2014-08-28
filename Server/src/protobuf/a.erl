%%%------------------------------------
%%% @author 严利宏 <542430172@qq.com>
%%% @copyright jieyou 2014.08.25
%%% @doc description.
%%% @end
%%%------------------------------------

-module(a).
-compile(export_all).

-include("pt201_pb.hrl").

encode() ->
	Person = #pt20100{ name="John", id=2},
	pt201_pb:encode_pt20100(Person).

decode() ->
	Data = encode(),
	pt201_pb:decode_pt20100(Data).

encode_repeat() ->
	RepeatData =
	[
		#pt20100_phone{number="12345", type=0},
        #pt20100_phone{number="22222", type=1},
		#pt20100_phone{number="11", type=2}
	],
	Person = #pt20100{id=1, name="me", email="hh", phones=RepeatData},
	pt201_pb:encode_pt20100(Person).
	
decode_repeat() ->
	Data = encode_repeat(),
	pt201_pb:decode_pt20100(Data).

routing(Cmd, Binary) ->
    [H1,H2,H3,H4,H5 | _] = integer_to_list(Cmd),
    Module = list_to_atom([$p,$t,H1,H2,H3,$_,$p,$b]), % atom = pt123_pb
    Fun = list_to_atom([$d,$e,$c,$o,$d,$e,H1,H2,H3,H4,H5,$_,$p,$b]), % atom = decode12345_pb
    {ok, Module:Fun(Binary)}.

pack(Cmd, RecordData) ->
    [H1,H2,H3,H4,H5 | _] = integer_to_list(Cmd),
    Module = list_to_atom([$p,$t,H1,H2,H3,$_,$p,$b]), % atom = pt123_pb
    Fun = list_to_atom([$d,$e,$c,$o,$d,$e,H1,H2,H3,H4,H5,$_,$p,$b]), % atom = decode12345_pb
    Bin = Module:Fun(RecordData),
    Len = byte_size(Bin) + 2,
    {ok, <<Len:16, Cmd:16, Bin/binary>>}.

t() ->
    	RepeatData =
	[
		#pt20100_phone{number="12345"},
        #pt20100_phone{number="22222", type=1},
		#pt20100_phone{number="11", type=2}
	],
	Person = #pt20100{id=1, name="me", email="hh", phones=RepeatData},
    {ok, Bin} = pack(20100, Person),
    io:format("Bin = ~p~n", [Bin]),

    <<_Len:16, Cmd:16, BinR/binary>> = Bin,
    {ok, A} = routing(Cmd, BinR),
    io:format("A = ~p~n", [A]).





