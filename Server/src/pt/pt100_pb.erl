-file("src/pt100_pb.erl", 1).

-module(pt100_pb).

-export([encode_pt10002_toc_player/1,
	 decode_pt10002_toc_player/1,
	 delimited_decode_pt10002_toc_player/1,
	 encode_pt10010_toc/1, decode_pt10010_toc/1,
	 delimited_decode_pt10010_toc/1, encode_pt10010_tos/1,
	 decode_pt10010_tos/1, delimited_decode_pt10010_tos/1,
	 encode_pt10006_toc/1, decode_pt10006_toc/1,
	 delimited_decode_pt10006_toc/1, encode_pt10006_tos/1,
	 decode_pt10006_tos/1, delimited_decode_pt10006_tos/1,
	 encode_pt10005_toc/1, decode_pt10005_toc/1,
	 delimited_decode_pt10005_toc/1, encode_pt10005_tos/1,
	 decode_pt10005_tos/1, delimited_decode_pt10005_tos/1,
	 encode_pt10004_toc/1, decode_pt10004_toc/1,
	 delimited_decode_pt10004_toc/1, encode_pt10004_tos/1,
	 decode_pt10004_tos/1, delimited_decode_pt10004_tos/1,
	 encode_pt10003_toc/1, decode_pt10003_toc/1,
	 delimited_decode_pt10003_toc/1, encode_pt10003_tos/1,
	 decode_pt10003_tos/1, delimited_decode_pt10003_tos/1,
	 encode_pt10002_toc/1, decode_pt10002_toc/1,
	 delimited_decode_pt10002_toc/1, encode_pt10002_tos/1,
	 decode_pt10002_tos/1, delimited_decode_pt10002_tos/1,
	 encode_pt10000_toc/1, decode_pt10000_toc/1,
	 delimited_decode_pt10000_toc/1, encode_pt10000_tos/1,
	 decode_pt10000_tos/1, delimited_decode_pt10000_tos/1]).

-export([has_extension/2, extension_size/1,
	 get_extension/2, set_extension/3]).

-export([decode_extensions/1]).

-export([encode/1, decode/2, delimited_decode/2]).

-export([int_to_enum/2, enum_to_int/2]).

-record(pt10002_toc_player,
	{id, status, career, sex, lv, name, realm}).

-record(pt10010_toc, {code}).

-record(pt10010_tos, {name}).

-record(pt10006_toc, {}).

-record(pt10006_tos, {}).

-record(pt10005_toc, {code}).

-record(pt10005_tos, {id}).

-record(pt10004_toc, {code}).

-record(pt10004_tos, {id, time, ticket}).

-record(pt10003_toc, {code, id}).

-record(pt10003_tos, {realm, career, sex, name}).

-record(pt10002_toc, {players}).

-record(pt10002_tos, {}).

-record(pt10000_toc, {code, num, career, time}).

-record(pt10000_tos, {acc_id, accname, time, ticket}).

encode([]) -> [];
encode(Records) when is_list(Records) ->
    delimited_encode(Records);
encode(Record) -> encode(element(1, Record), Record).

encode_pt10002_toc_player(Records)
    when is_list(Records) ->
    delimited_encode(Records);
encode_pt10002_toc_player(Record)
    when is_record(Record, pt10002_toc_player) ->
    encode(pt10002_toc_player, Record).

encode_pt10010_toc(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_pt10010_toc(Record)
    when is_record(Record, pt10010_toc) ->
    encode(pt10010_toc, Record).

encode_pt10010_tos(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_pt10010_tos(Record)
    when is_record(Record, pt10010_tos) ->
    encode(pt10010_tos, Record).

encode_pt10006_toc(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_pt10006_toc(Record)
    when is_record(Record, pt10006_toc) ->
    encode(pt10006_toc, Record).

encode_pt10006_tos(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_pt10006_tos(Record)
    when is_record(Record, pt10006_tos) ->
    encode(pt10006_tos, Record).

encode_pt10005_toc(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_pt10005_toc(Record)
    when is_record(Record, pt10005_toc) ->
    encode(pt10005_toc, Record).

encode_pt10005_tos(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_pt10005_tos(Record)
    when is_record(Record, pt10005_tos) ->
    encode(pt10005_tos, Record).

encode_pt10004_toc(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_pt10004_toc(Record)
    when is_record(Record, pt10004_toc) ->
    encode(pt10004_toc, Record).

encode_pt10004_tos(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_pt10004_tos(Record)
    when is_record(Record, pt10004_tos) ->
    encode(pt10004_tos, Record).

encode_pt10003_toc(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_pt10003_toc(Record)
    when is_record(Record, pt10003_toc) ->
    encode(pt10003_toc, Record).

encode_pt10003_tos(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_pt10003_tos(Record)
    when is_record(Record, pt10003_tos) ->
    encode(pt10003_tos, Record).

encode_pt10002_toc(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_pt10002_toc(Record)
    when is_record(Record, pt10002_toc) ->
    encode(pt10002_toc, Record).

encode_pt10002_tos(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_pt10002_tos(Record)
    when is_record(Record, pt10002_tos) ->
    encode(pt10002_tos, Record).

encode_pt10000_toc(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_pt10000_toc(Record)
    when is_record(Record, pt10000_toc) ->
    encode(pt10000_toc, Record).

encode_pt10000_tos(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_pt10000_tos(Record)
    when is_record(Record, pt10000_tos) ->
    encode(pt10000_tos, Record).

encode(pt10000_tos, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(pt10000_tos, Record) ->
    [iolist(pt10000_tos, Record)
     | encode_extensions(Record)];
encode(pt10000_toc, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(pt10000_toc, Record) ->
    [iolist(pt10000_toc, Record)
     | encode_extensions(Record)];
encode(pt10002_tos, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(pt10002_tos, Record) ->
    [iolist(pt10002_tos, Record)
     | encode_extensions(Record)];
encode(pt10002_toc, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(pt10002_toc, Record) ->
    [iolist(pt10002_toc, Record)
     | encode_extensions(Record)];
encode(pt10003_tos, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(pt10003_tos, Record) ->
    [iolist(pt10003_tos, Record)
     | encode_extensions(Record)];
encode(pt10003_toc, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(pt10003_toc, Record) ->
    [iolist(pt10003_toc, Record)
     | encode_extensions(Record)];
encode(pt10004_tos, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(pt10004_tos, Record) ->
    [iolist(pt10004_tos, Record)
     | encode_extensions(Record)];
encode(pt10004_toc, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(pt10004_toc, Record) ->
    [iolist(pt10004_toc, Record)
     | encode_extensions(Record)];
encode(pt10005_tos, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(pt10005_tos, Record) ->
    [iolist(pt10005_tos, Record)
     | encode_extensions(Record)];
encode(pt10005_toc, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(pt10005_toc, Record) ->
    [iolist(pt10005_toc, Record)
     | encode_extensions(Record)];
encode(pt10006_tos, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(pt10006_tos, Record) ->
    [iolist(pt10006_tos, Record)
     | encode_extensions(Record)];
encode(pt10006_toc, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(pt10006_toc, Record) ->
    [iolist(pt10006_toc, Record)
     | encode_extensions(Record)];
encode(pt10010_tos, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(pt10010_tos, Record) ->
    [iolist(pt10010_tos, Record)
     | encode_extensions(Record)];
encode(pt10010_toc, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(pt10010_toc, Record) ->
    [iolist(pt10010_toc, Record)
     | encode_extensions(Record)];
encode(pt10002_toc_player, Records)
    when is_list(Records) ->
    delimited_encode(Records);
encode(pt10002_toc_player, Record) ->
    [iolist(pt10002_toc_player, Record)
     | encode_extensions(Record)].

encode_extensions(_) -> [].

delimited_encode(Records) ->
    lists:map(fun (Record) ->
		      IoRec = encode(Record),
		      Size = iolist_size(IoRec),
		      [protobuffs:encode_varint(Size), IoRec]
	      end,
	      Records).

iolist(pt10000_tos, Record) ->
    [pack(1, required,
	  with_default(Record#pt10000_tos.acc_id, none), int32,
	  []),
     pack(2, required,
	  with_default(Record#pt10000_tos.accname, none), string,
	  []),
     pack(3, required,
	  with_default(Record#pt10000_tos.time, none), int32, []),
     pack(4, required,
	  with_default(Record#pt10000_tos.ticket, none), string,
	  [])];
iolist(pt10000_toc, Record) ->
    [pack(1, required,
	  with_default(Record#pt10000_toc.code, none), int32, []),
     pack(2, required,
	  with_default(Record#pt10000_toc.num, none), int32, []),
     pack(3, required,
	  with_default(Record#pt10000_toc.career, none), int32,
	  []),
     pack(4, required,
	  with_default(Record#pt10000_toc.time, none), int32,
	  [])];
iolist(pt10002_tos, _Record) -> [];
iolist(pt10002_toc, Record) ->
    [pack(1, repeated,
	  with_default(Record#pt10002_toc.players, none),
	  pt10002_toc_player, [])];
iolist(pt10003_tos, Record) ->
    [pack(1, required,
	  with_default(Record#pt10003_tos.realm, none), int32,
	  []),
     pack(2, required,
	  with_default(Record#pt10003_tos.career, none), int32,
	  []),
     pack(3, required,
	  with_default(Record#pt10003_tos.sex, none), int32, []),
     pack(4, required,
	  with_default(Record#pt10003_tos.name, none), string,
	  [])];
iolist(pt10003_toc, Record) ->
    [pack(1, required,
	  with_default(Record#pt10003_toc.code, none), int32, []),
     pack(2, required,
	  with_default(Record#pt10003_toc.id, none), int32, [])];
iolist(pt10004_tos, Record) ->
    [pack(1, required,
	  with_default(Record#pt10004_tos.id, none), int32, []),
     pack(2, required,
	  with_default(Record#pt10004_tos.time, none), int32, []),
     pack(3, required,
	  with_default(Record#pt10004_tos.ticket, none), string,
	  [])];
iolist(pt10004_toc, Record) ->
    [pack(1, required,
	  with_default(Record#pt10004_toc.code, none), int32,
	  [])];
iolist(pt10005_tos, Record) ->
    [pack(1, required,
	  with_default(Record#pt10005_tos.id, none), int32, [])];
iolist(pt10005_toc, Record) ->
    [pack(1, required,
	  with_default(Record#pt10005_toc.code, none), int32,
	  [])];
iolist(pt10006_tos, _Record) -> [];
iolist(pt10006_toc, _Record) -> [];
iolist(pt10010_tos, Record) ->
    [pack(1, required,
	  with_default(Record#pt10010_tos.name, none), string,
	  [])];
iolist(pt10010_toc, Record) ->
    [pack(1, required,
	  with_default(Record#pt10010_toc.code, none), int32,
	  [])];
iolist(pt10002_toc_player, Record) ->
    [pack(1, required,
	  with_default(Record#pt10002_toc_player.id, none), int32,
	  []),
     pack(2, required,
	  with_default(Record#pt10002_toc_player.status, none),
	  int32, []),
     pack(3, required,
	  with_default(Record#pt10002_toc_player.career, none),
	  int32, []),
     pack(4, required,
	  with_default(Record#pt10002_toc_player.sex, none),
	  int32, []),
     pack(5, required,
	  with_default(Record#pt10002_toc_player.lv, none), int32,
	  []),
     pack(6, required,
	  with_default(Record#pt10002_toc_player.name, none),
	  string, []),
     pack(7, required,
	  with_default(Record#pt10002_toc_player.realm, none),
	  int32, [])].

with_default(Default, Default) -> undefined;
with_default(Val, _) -> Val.

pack(_, optional, undefined, _, _) -> [];
pack(_, repeated, undefined, _, _) -> [];
pack(_, repeated_packed, undefined, _, _) -> [];
pack(_, repeated_packed, [], _, _) -> [];
pack(FNum, required, undefined, Type, _) ->
    exit({error,
	  {required_field_is_undefined, FNum, Type}});
pack(_, repeated, [], _, Acc) -> lists:reverse(Acc);
pack(FNum, repeated, [Head | Tail], Type, Acc) ->
    pack(FNum, repeated, Tail, Type,
	 [pack(FNum, optional, Head, Type, []) | Acc]);
pack(FNum, repeated_packed, Data, Type, _) ->
    protobuffs:encode_packed(FNum, Data, Type);
pack(FNum, _, Data, _, _) when is_tuple(Data) ->
    [RecName | _] = tuple_to_list(Data),
    protobuffs:encode(FNum, encode(RecName, Data), bytes);
pack(FNum, _, Data, Type, _)
    when Type =:= bool;
	 Type =:= int32;
	 Type =:= uint32;
	 Type =:= int64;
	 Type =:= uint64;
	 Type =:= sint32;
	 Type =:= sint64;
	 Type =:= fixed32;
	 Type =:= sfixed32;
	 Type =:= fixed64;
	 Type =:= sfixed64;
	 Type =:= string;
	 Type =:= bytes;
	 Type =:= float;
	 Type =:= double ->
    protobuffs:encode(FNum, Data, Type);
pack(FNum, _, Data, Type, _) when is_atom(Data) ->
    protobuffs:encode(FNum, enum_to_int(Type, Data), enum).

enum_to_int(pikachu, value) -> 1.

int_to_enum(_, Val) -> Val.

decode_pt10002_toc_player(Bytes)
    when is_binary(Bytes) ->
    decode(pt10002_toc_player, Bytes).

decode_pt10010_toc(Bytes) when is_binary(Bytes) ->
    decode(pt10010_toc, Bytes).

decode_pt10010_tos(Bytes) when is_binary(Bytes) ->
    decode(pt10010_tos, Bytes).

decode_pt10006_toc(Bytes) when is_binary(Bytes) ->
    decode(pt10006_toc, Bytes).

decode_pt10006_tos(Bytes) when is_binary(Bytes) ->
    decode(pt10006_tos, Bytes).

decode_pt10005_toc(Bytes) when is_binary(Bytes) ->
    decode(pt10005_toc, Bytes).

decode_pt10005_tos(Bytes) when is_binary(Bytes) ->
    decode(pt10005_tos, Bytes).

decode_pt10004_toc(Bytes) when is_binary(Bytes) ->
    decode(pt10004_toc, Bytes).

decode_pt10004_tos(Bytes) when is_binary(Bytes) ->
    decode(pt10004_tos, Bytes).

decode_pt10003_toc(Bytes) when is_binary(Bytes) ->
    decode(pt10003_toc, Bytes).

decode_pt10003_tos(Bytes) when is_binary(Bytes) ->
    decode(pt10003_tos, Bytes).

decode_pt10002_toc(Bytes) when is_binary(Bytes) ->
    decode(pt10002_toc, Bytes).

decode_pt10002_tos(Bytes) when is_binary(Bytes) ->
    decode(pt10002_tos, Bytes).

decode_pt10000_toc(Bytes) when is_binary(Bytes) ->
    decode(pt10000_toc, Bytes).

decode_pt10000_tos(Bytes) when is_binary(Bytes) ->
    decode(pt10000_tos, Bytes).

delimited_decode_pt10000_tos(Bytes) ->
    delimited_decode(pt10000_tos, Bytes).

delimited_decode_pt10000_toc(Bytes) ->
    delimited_decode(pt10000_toc, Bytes).

delimited_decode_pt10002_tos(Bytes) ->
    delimited_decode(pt10002_tos, Bytes).

delimited_decode_pt10002_toc(Bytes) ->
    delimited_decode(pt10002_toc, Bytes).

delimited_decode_pt10003_tos(Bytes) ->
    delimited_decode(pt10003_tos, Bytes).

delimited_decode_pt10003_toc(Bytes) ->
    delimited_decode(pt10003_toc, Bytes).

delimited_decode_pt10004_tos(Bytes) ->
    delimited_decode(pt10004_tos, Bytes).

delimited_decode_pt10004_toc(Bytes) ->
    delimited_decode(pt10004_toc, Bytes).

delimited_decode_pt10005_tos(Bytes) ->
    delimited_decode(pt10005_tos, Bytes).

delimited_decode_pt10005_toc(Bytes) ->
    delimited_decode(pt10005_toc, Bytes).

delimited_decode_pt10006_tos(Bytes) ->
    delimited_decode(pt10006_tos, Bytes).

delimited_decode_pt10006_toc(Bytes) ->
    delimited_decode(pt10006_toc, Bytes).

delimited_decode_pt10010_tos(Bytes) ->
    delimited_decode(pt10010_tos, Bytes).

delimited_decode_pt10010_toc(Bytes) ->
    delimited_decode(pt10010_toc, Bytes).

delimited_decode_pt10002_toc_player(Bytes) ->
    delimited_decode(pt10002_toc_player, Bytes).

delimited_decode(Type, Bytes) when is_binary(Bytes) ->
    delimited_decode(Type, Bytes, []).

delimited_decode(_Type, <<>>, Acc) ->
    {lists:reverse(Acc), <<>>};
delimited_decode(Type, Bytes, Acc) ->
    try protobuffs:decode_varint(Bytes) of
      {Size, Rest} when size(Rest) < Size ->
	  {lists:reverse(Acc), Bytes};
      {Size, Rest} ->
	  <<MessageBytes:Size/binary, Rest2/binary>> = Rest,
	  Message = decode(Type, MessageBytes),
	  delimited_decode(Type, Rest2, [Message | Acc])
    catch
      _What:_Why -> {lists:reverse(Acc), Bytes}
    end.

decode(enummsg_values, 1) -> value1;
decode(pt10000_tos, Bytes) when is_binary(Bytes) ->
    Types = [{4, ticket, string, []}, {3, time, int32, []},
	     {2, accname, string, []}, {1, acc_id, int32, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(pt10000_tos, Decoded);
decode(pt10000_toc, Bytes) when is_binary(Bytes) ->
    Types = [{4, time, int32, []}, {3, career, int32, []},
	     {2, num, int32, []}, {1, code, int32, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(pt10000_toc, Decoded);
decode(pt10002_tos, Bytes) when is_binary(Bytes) ->
    Types = [],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(pt10002_tos, Decoded);
decode(pt10002_toc, Bytes) when is_binary(Bytes) ->
    Types = [{1, players, pt10002_toc_player,
	      [is_record, repeated]}],
    Defaults = [{1, players, []}],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(pt10002_toc, Decoded);
decode(pt10003_tos, Bytes) when is_binary(Bytes) ->
    Types = [{4, name, string, []}, {3, sex, int32, []},
	     {2, career, int32, []}, {1, realm, int32, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(pt10003_tos, Decoded);
decode(pt10003_toc, Bytes) when is_binary(Bytes) ->
    Types = [{2, id, int32, []}, {1, code, int32, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(pt10003_toc, Decoded);
decode(pt10004_tos, Bytes) when is_binary(Bytes) ->
    Types = [{3, ticket, string, []}, {2, time, int32, []},
	     {1, id, int32, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(pt10004_tos, Decoded);
decode(pt10004_toc, Bytes) when is_binary(Bytes) ->
    Types = [{1, code, int32, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(pt10004_toc, Decoded);
decode(pt10005_tos, Bytes) when is_binary(Bytes) ->
    Types = [{1, id, int32, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(pt10005_tos, Decoded);
decode(pt10005_toc, Bytes) when is_binary(Bytes) ->
    Types = [{1, code, int32, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(pt10005_toc, Decoded);
decode(pt10006_tos, Bytes) when is_binary(Bytes) ->
    Types = [],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(pt10006_tos, Decoded);
decode(pt10006_toc, Bytes) when is_binary(Bytes) ->
    Types = [],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(pt10006_toc, Decoded);
decode(pt10010_tos, Bytes) when is_binary(Bytes) ->
    Types = [{1, name, string, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(pt10010_tos, Decoded);
decode(pt10010_toc, Bytes) when is_binary(Bytes) ->
    Types = [{1, code, int32, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(pt10010_toc, Decoded);
decode(pt10002_toc_player, Bytes)
    when is_binary(Bytes) ->
    Types = [{7, realm, int32, []}, {6, name, string, []},
	     {5, lv, int32, []}, {4, sex, int32, []},
	     {3, career, int32, []}, {2, status, int32, []},
	     {1, id, int32, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(pt10002_toc_player, Decoded).

decode(<<>>, Types, Acc) ->
    reverse_repeated_fields(Acc, Types);
decode(Bytes, Types, Acc) ->
    {ok, FNum} = protobuffs:next_field_num(Bytes),
    case lists:keyfind(FNum, 1, Types) of
      {FNum, Name, Type, Opts} ->
	  {Value1, Rest1} = case lists:member(is_record, Opts) of
			      true ->
				  {{FNum, V}, R} = protobuffs:decode(Bytes,
								     bytes),
				  RecVal = decode(Type, V),
				  {RecVal, R};
			      false ->
				  case lists:member(repeated_packed, Opts) of
				    true ->
					{{FNum, V}, R} =
					    protobuffs:decode_packed(Bytes,
								     Type),
					{V, R};
				    false ->
					{{FNum, V}, R} =
					    protobuffs:decode(Bytes, Type),
					{unpack_value(V, Type), R}
				  end
			    end,
	  case lists:member(repeated, Opts) of
	    true ->
		case lists:keytake(FNum, 1, Acc) of
		  {value, {FNum, Name, List}, Acc1} ->
		      decode(Rest1, Types,
			     [{FNum, Name, [int_to_enum(Type, Value1) | List]}
			      | Acc1]);
		  false ->
		      decode(Rest1, Types,
			     [{FNum, Name, [int_to_enum(Type, Value1)]} | Acc])
		end;
	    false ->
		decode(Rest1, Types,
		       [{FNum, Name, int_to_enum(Type, Value1)} | Acc])
	  end;
      false ->
	  case lists:keyfind('$extensions', 2, Acc) of
	    {_, _, Dict} ->
		{{FNum, _V}, R} = protobuffs:decode(Bytes, bytes),
		Diff = size(Bytes) - size(R),
		<<V:Diff/binary, _/binary>> = Bytes,
		NewDict = dict:store(FNum, V, Dict),
		NewAcc = lists:keyreplace('$extensions', 2, Acc,
					  {false, '$extensions', NewDict}),
		decode(R, Types, NewAcc);
	    _ ->
		{ok, Skipped} = protobuffs:skip_next_field(Bytes),
		decode(Skipped, Types, Acc)
	  end
    end.

reverse_repeated_fields(FieldList, Types) ->
    [begin
       case lists:keyfind(FNum, 1, Types) of
	 {FNum, Name, _Type, Opts} ->
	     case lists:member(repeated, Opts) of
	       true -> {FNum, Name, lists:reverse(Value)};
	       _ -> Field
	     end;
	 _ -> Field
       end
     end
     || {FNum, Name, Value} = Field <- FieldList].

unpack_value(Binary, string) when is_binary(Binary) ->
    binary_to_list(Binary);
unpack_value(Value, _) -> Value.

to_record(pt10000_tos, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       pt10000_tos),
						   Record, Name, Val)
			  end,
			  #pt10000_tos{}, DecodedTuples),
    Record1;
to_record(pt10000_toc, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       pt10000_toc),
						   Record, Name, Val)
			  end,
			  #pt10000_toc{}, DecodedTuples),
    Record1;
to_record(pt10002_tos, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       pt10002_tos),
						   Record, Name, Val)
			  end,
			  #pt10002_tos{}, DecodedTuples),
    Record1;
to_record(pt10002_toc, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       pt10002_toc),
						   Record, Name, Val)
			  end,
			  #pt10002_toc{}, DecodedTuples),
    Record1;
to_record(pt10003_tos, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       pt10003_tos),
						   Record, Name, Val)
			  end,
			  #pt10003_tos{}, DecodedTuples),
    Record1;
to_record(pt10003_toc, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       pt10003_toc),
						   Record, Name, Val)
			  end,
			  #pt10003_toc{}, DecodedTuples),
    Record1;
to_record(pt10004_tos, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       pt10004_tos),
						   Record, Name, Val)
			  end,
			  #pt10004_tos{}, DecodedTuples),
    Record1;
to_record(pt10004_toc, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       pt10004_toc),
						   Record, Name, Val)
			  end,
			  #pt10004_toc{}, DecodedTuples),
    Record1;
to_record(pt10005_tos, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       pt10005_tos),
						   Record, Name, Val)
			  end,
			  #pt10005_tos{}, DecodedTuples),
    Record1;
to_record(pt10005_toc, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       pt10005_toc),
						   Record, Name, Val)
			  end,
			  #pt10005_toc{}, DecodedTuples),
    Record1;
to_record(pt10006_tos, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       pt10006_tos),
						   Record, Name, Val)
			  end,
			  #pt10006_tos{}, DecodedTuples),
    Record1;
to_record(pt10006_toc, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       pt10006_toc),
						   Record, Name, Val)
			  end,
			  #pt10006_toc{}, DecodedTuples),
    Record1;
to_record(pt10010_tos, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       pt10010_tos),
						   Record, Name, Val)
			  end,
			  #pt10010_tos{}, DecodedTuples),
    Record1;
to_record(pt10010_toc, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       pt10010_toc),
						   Record, Name, Val)
			  end,
			  #pt10010_toc{}, DecodedTuples),
    Record1;
to_record(pt10002_toc_player, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       pt10002_toc_player),
						   Record, Name, Val)
			  end,
			  #pt10002_toc_player{}, DecodedTuples),
    Record1.

decode_extensions(Record) -> Record.

decode_extensions(_Types, [], Acc) ->
    dict:from_list(Acc);
decode_extensions(Types, [{Fnum, Bytes} | Tail], Acc) ->
    NewAcc = case lists:keyfind(Fnum, 1, Types) of
	       {Fnum, Name, Type, Opts} ->
		   {Value1, Rest1} = case lists:member(is_record, Opts) of
				       true ->
					   {{FNum, V}, R} =
					       protobuffs:decode(Bytes, bytes),
					   RecVal = decode(Type, V),
					   {RecVal, R};
				       false ->
					   case lists:member(repeated_packed,
							     Opts)
					       of
					     true ->
						 {{FNum, V}, R} =
						     protobuffs:decode_packed(Bytes,
									      Type),
						 {V, R};
					     false ->
						 {{FNum, V}, R} =
						     protobuffs:decode(Bytes,
								       Type),
						 {unpack_value(V, Type), R}
					   end
				     end,
		   case lists:member(repeated, Opts) of
		     true ->
			 case lists:keytake(FNum, 1, Acc) of
			   {value, {FNum, Name, List}, Acc1} ->
			       decode(Rest1, Types,
				      [{FNum, Name,
					lists:reverse([int_to_enum(Type, Value1)
						       | lists:reverse(List)])}
				       | Acc1]);
			   false ->
			       decode(Rest1, Types,
				      [{FNum, Name, [int_to_enum(Type, Value1)]}
				       | Acc])
			 end;
		     false ->
			 [{Fnum,
			   {optional, int_to_enum(Type, Value1), Type, Opts}}
			  | Acc]
		   end;
	       false -> [{Fnum, Bytes} | Acc]
	     end,
    decode_extensions(Types, Tail, NewAcc).

set_record_field(Fields, Record, '$extensions',
		 Value) ->
    Decodable = [],
    NewValue = decode_extensions(element(1, Record),
				 Decodable, dict:to_list(Value)),
    Index = list_index('$extensions', Fields),
    erlang:setelement(Index + 1, Record, NewValue);
set_record_field(Fields, Record, Field, Value) ->
    Index = list_index(Field, Fields),
    erlang:setelement(Index + 1, Record, Value).

list_index(Target, List) -> list_index(Target, List, 1).

list_index(Target, [Target | _], Index) -> Index;
list_index(Target, [_ | Tail], Index) ->
    list_index(Target, Tail, Index + 1);
list_index(_, [], _) -> -1.

extension_size(_) -> 0.

has_extension(_Record, _FieldName) -> false.

get_extension(_Record, _FieldName) -> undefined.

set_extension(Record, _, _) -> {error, Record}.

