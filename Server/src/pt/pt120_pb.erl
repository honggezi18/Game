-file("src/pt120_pb.erl", 1).

-module(pt120_pb).

-export([encode_pt12009_toc/1, decode_pt12009_toc/1,
	 delimited_decode_pt12009_toc/1, encode_pt12008_toc/1,
	 decode_pt12008_toc/1, delimited_decode_pt12008_toc/1,
	 encode_pt12007_toc/1, decode_pt12007_toc/1,
	 delimited_decode_pt12007_toc/1, encode_pt12006_toc/1,
	 decode_pt12006_toc/1, delimited_decode_pt12006_toc/1,
	 encode_pt12005_toc/1, decode_pt12005_toc/1,
	 delimited_decode_pt12005_toc/1, encode_pt12005_tos/1,
	 decode_pt12005_tos/1, delimited_decode_pt12005_tos/1,
	 encode_pt12004_toc/1, decode_pt12004_toc/1,
	 delimited_decode_pt12004_toc/1, encode_pt12003_toc/1,
	 decode_pt12003_toc/1, delimited_decode_pt12003_toc/1,
	 encode_pt12002_toc/1, decode_pt12002_toc/1,
	 delimited_decode_pt12002_toc/1, encode_pt12002_tos/1,
	 decode_pt12002_tos/1, delimited_decode_pt12002_tos/1,
	 encode_pt12001_toc/1, decode_pt12001_toc/1,
	 delimited_decode_pt12001_toc/1, encode_pt12001_tos/1,
	 decode_pt12001_tos/1, delimited_decode_pt12001_tos/1,
	 encode_pt_mon/1, decode_pt_mon/1,
	 delimited_decode_pt_mon/1, encode_pt_player/1,
	 decode_pt_player/1, delimited_decode_pt_player/1]).

-export([has_extension/2, extension_size/1,
	 get_extension/2, set_extension/3]).

-export([decode_extensions/1]).

-export([encode/1, decode/2, delimited_decode/2]).

-export([int_to_enum/2, enum_to_int/2]).

-record(pt12009_toc,
	{id, platform, server_id, hp, hp_lim}).

-record(pt12008_toc, {id, x, y, pix, piy}).

-record(pt12007_toc, {mon}).

-record(pt12006_toc, {id}).

-record(pt12005_toc, {id, x, y, name, scene_id}).

-record(pt12005_tos, {scene_id}).

-record(pt12004_toc, {id, platform, server_id}).

-record(pt12003_toc, {player}).

-record(pt12002_toc, {players, mons}).

-record(pt12002_tos, {}).

-record(pt12001_toc,
	{x, y, id, platform, server_id, fly, d1, d2, skill}).

-record(pt12001_tos, {x, y, fly, d1, d2, skill, scene}).

-record(pt_mon, {id}).

-record(pt_player, {id}).

encode([]) -> [];
encode(Records) when is_list(Records) ->
    delimited_encode(Records);
encode(Record) -> encode(element(1, Record), Record).

encode_pt12009_toc(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_pt12009_toc(Record)
    when is_record(Record, pt12009_toc) ->
    encode(pt12009_toc, Record).

encode_pt12008_toc(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_pt12008_toc(Record)
    when is_record(Record, pt12008_toc) ->
    encode(pt12008_toc, Record).

encode_pt12007_toc(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_pt12007_toc(Record)
    when is_record(Record, pt12007_toc) ->
    encode(pt12007_toc, Record).

encode_pt12006_toc(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_pt12006_toc(Record)
    when is_record(Record, pt12006_toc) ->
    encode(pt12006_toc, Record).

encode_pt12005_toc(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_pt12005_toc(Record)
    when is_record(Record, pt12005_toc) ->
    encode(pt12005_toc, Record).

encode_pt12005_tos(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_pt12005_tos(Record)
    when is_record(Record, pt12005_tos) ->
    encode(pt12005_tos, Record).

encode_pt12004_toc(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_pt12004_toc(Record)
    when is_record(Record, pt12004_toc) ->
    encode(pt12004_toc, Record).

encode_pt12003_toc(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_pt12003_toc(Record)
    when is_record(Record, pt12003_toc) ->
    encode(pt12003_toc, Record).

encode_pt12002_toc(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_pt12002_toc(Record)
    when is_record(Record, pt12002_toc) ->
    encode(pt12002_toc, Record).

encode_pt12002_tos(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_pt12002_tos(Record)
    when is_record(Record, pt12002_tos) ->
    encode(pt12002_tos, Record).

encode_pt12001_toc(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_pt12001_toc(Record)
    when is_record(Record, pt12001_toc) ->
    encode(pt12001_toc, Record).

encode_pt12001_tos(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_pt12001_tos(Record)
    when is_record(Record, pt12001_tos) ->
    encode(pt12001_tos, Record).

encode_pt_mon(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_pt_mon(Record) when is_record(Record, pt_mon) ->
    encode(pt_mon, Record).

encode_pt_player(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_pt_player(Record)
    when is_record(Record, pt_player) ->
    encode(pt_player, Record).

encode(pt_player, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(pt_player, Record) ->
    [iolist(pt_player, Record) | encode_extensions(Record)];
encode(pt_mon, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(pt_mon, Record) ->
    [iolist(pt_mon, Record) | encode_extensions(Record)];
encode(pt12001_tos, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(pt12001_tos, Record) ->
    [iolist(pt12001_tos, Record)
     | encode_extensions(Record)];
encode(pt12001_toc, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(pt12001_toc, Record) ->
    [iolist(pt12001_toc, Record)
     | encode_extensions(Record)];
encode(pt12002_tos, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(pt12002_tos, Record) ->
    [iolist(pt12002_tos, Record)
     | encode_extensions(Record)];
encode(pt12002_toc, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(pt12002_toc, Record) ->
    [iolist(pt12002_toc, Record)
     | encode_extensions(Record)];
encode(pt12003_toc, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(pt12003_toc, Record) ->
    [iolist(pt12003_toc, Record)
     | encode_extensions(Record)];
encode(pt12004_toc, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(pt12004_toc, Record) ->
    [iolist(pt12004_toc, Record)
     | encode_extensions(Record)];
encode(pt12005_tos, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(pt12005_tos, Record) ->
    [iolist(pt12005_tos, Record)
     | encode_extensions(Record)];
encode(pt12005_toc, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(pt12005_toc, Record) ->
    [iolist(pt12005_toc, Record)
     | encode_extensions(Record)];
encode(pt12006_toc, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(pt12006_toc, Record) ->
    [iolist(pt12006_toc, Record)
     | encode_extensions(Record)];
encode(pt12007_toc, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(pt12007_toc, Record) ->
    [iolist(pt12007_toc, Record)
     | encode_extensions(Record)];
encode(pt12008_toc, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(pt12008_toc, Record) ->
    [iolist(pt12008_toc, Record)
     | encode_extensions(Record)];
encode(pt12009_toc, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(pt12009_toc, Record) ->
    [iolist(pt12009_toc, Record)
     | encode_extensions(Record)].

encode_extensions(_) -> [].

delimited_encode(Records) ->
    lists:map(fun (Record) ->
		      IoRec = encode(Record),
		      Size = iolist_size(IoRec),
		      [protobuffs:encode_varint(Size), IoRec]
	      end,
	      Records).

iolist(pt_player, Record) ->
    [pack(1, required,
	  with_default(Record#pt_player.id, none), int32, [])];
iolist(pt_mon, Record) ->
    [pack(1, required, with_default(Record#pt_mon.id, none),
	  int32, [])];
iolist(pt12001_tos, Record) ->
    [pack(1, required,
	  with_default(Record#pt12001_tos.x, none), int32, []),
     pack(2, required,
	  with_default(Record#pt12001_tos.y, none), int32, []),
     pack(3, required,
	  with_default(Record#pt12001_tos.fly, none), int32, []),
     pack(4, required,
	  with_default(Record#pt12001_tos.d1, none), int32, []),
     pack(5, required,
	  with_default(Record#pt12001_tos.d2, none), int32, []),
     pack(6, required,
	  with_default(Record#pt12001_tos.skill, none), int32,
	  []),
     pack(7, required,
	  with_default(Record#pt12001_tos.scene, none), int32,
	  [])];
iolist(pt12001_toc, Record) ->
    [pack(1, required,
	  with_default(Record#pt12001_toc.x, none), int32, []),
     pack(2, required,
	  with_default(Record#pt12001_toc.y, none), int32, []),
     pack(3, required,
	  with_default(Record#pt12001_toc.id, none), int32, []),
     pack(4, required,
	  with_default(Record#pt12001_toc.platform, none), string,
	  []),
     pack(5, required,
	  with_default(Record#pt12001_toc.server_id, none), int32,
	  []),
     pack(6, required,
	  with_default(Record#pt12001_toc.fly, none), int32, []),
     pack(7, required,
	  with_default(Record#pt12001_toc.d1, none), int32, []),
     pack(8, required,
	  with_default(Record#pt12001_toc.d2, none), int32, []),
     pack(9, required,
	  with_default(Record#pt12001_toc.skill, none), int32,
	  [])];
iolist(pt12002_tos, _Record) -> [];
iolist(pt12002_toc, Record) ->
    [pack(1, repeated,
	  with_default(Record#pt12002_toc.players, none),
	  pt_player, []),
     pack(2, repeated,
	  with_default(Record#pt12002_toc.mons, none), pt_mon,
	  [])];
iolist(pt12003_toc, Record) ->
    [pack(1, required,
	  with_default(Record#pt12003_toc.player, none),
	  pt_player, [])];
iolist(pt12004_toc, Record) ->
    [pack(1, required,
	  with_default(Record#pt12004_toc.id, none), int32, []),
     pack(2, required,
	  with_default(Record#pt12004_toc.platform, none), string,
	  []),
     pack(3, required,
	  with_default(Record#pt12004_toc.server_id, none), int32,
	  [])];
iolist(pt12005_tos, Record) ->
    [pack(1, required,
	  with_default(Record#pt12005_tos.scene_id, none), int32,
	  [])];
iolist(pt12005_toc, Record) ->
    [pack(1, required,
	  with_default(Record#pt12005_toc.id, none), int32, []),
     pack(2, required,
	  with_default(Record#pt12005_toc.x, none), int32, []),
     pack(3, required,
	  with_default(Record#pt12005_toc.y, none), int32, []),
     pack(4, required,
	  with_default(Record#pt12005_toc.name, none), string,
	  []),
     pack(5, required,
	  with_default(Record#pt12005_toc.scene_id, none), int32,
	  [])];
iolist(pt12006_toc, Record) ->
    [pack(1, required,
	  with_default(Record#pt12006_toc.id, none), int32, [])];
iolist(pt12007_toc, Record) ->
    [pack(1, required,
	  with_default(Record#pt12007_toc.mon, none), pt_mon,
	  [])];
iolist(pt12008_toc, Record) ->
    [pack(1, required,
	  with_default(Record#pt12008_toc.id, none), int32, []),
     pack(2, required,
	  with_default(Record#pt12008_toc.x, none), int32, []),
     pack(3, required,
	  with_default(Record#pt12008_toc.y, none), int32, []),
     pack(4, required,
	  with_default(Record#pt12008_toc.pix, none), int32, []),
     pack(5, required,
	  with_default(Record#pt12008_toc.piy, none), int32, [])];
iolist(pt12009_toc, Record) ->
    [pack(1, required,
	  with_default(Record#pt12009_toc.id, none), int32, []),
     pack(2, required,
	  with_default(Record#pt12009_toc.platform, none), string,
	  []),
     pack(3, required,
	  with_default(Record#pt12009_toc.server_id, none), int32,
	  []),
     pack(4, required,
	  with_default(Record#pt12009_toc.hp, none), int32, []),
     pack(5, required,
	  with_default(Record#pt12009_toc.hp_lim, none), int32,
	  [])].

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

decode_pt12009_toc(Bytes) when is_binary(Bytes) ->
    decode(pt12009_toc, Bytes).

decode_pt12008_toc(Bytes) when is_binary(Bytes) ->
    decode(pt12008_toc, Bytes).

decode_pt12007_toc(Bytes) when is_binary(Bytes) ->
    decode(pt12007_toc, Bytes).

decode_pt12006_toc(Bytes) when is_binary(Bytes) ->
    decode(pt12006_toc, Bytes).

decode_pt12005_toc(Bytes) when is_binary(Bytes) ->
    decode(pt12005_toc, Bytes).

decode_pt12005_tos(Bytes) when is_binary(Bytes) ->
    decode(pt12005_tos, Bytes).

decode_pt12004_toc(Bytes) when is_binary(Bytes) ->
    decode(pt12004_toc, Bytes).

decode_pt12003_toc(Bytes) when is_binary(Bytes) ->
    decode(pt12003_toc, Bytes).

decode_pt12002_toc(Bytes) when is_binary(Bytes) ->
    decode(pt12002_toc, Bytes).

decode_pt12002_tos(Bytes) when is_binary(Bytes) ->
    decode(pt12002_tos, Bytes).

decode_pt12001_toc(Bytes) when is_binary(Bytes) ->
    decode(pt12001_toc, Bytes).

decode_pt12001_tos(Bytes) when is_binary(Bytes) ->
    decode(pt12001_tos, Bytes).

decode_pt_mon(Bytes) when is_binary(Bytes) ->
    decode(pt_mon, Bytes).

decode_pt_player(Bytes) when is_binary(Bytes) ->
    decode(pt_player, Bytes).

delimited_decode_pt_player(Bytes) ->
    delimited_decode(pt_player, Bytes).

delimited_decode_pt_mon(Bytes) ->
    delimited_decode(pt_mon, Bytes).

delimited_decode_pt12001_tos(Bytes) ->
    delimited_decode(pt12001_tos, Bytes).

delimited_decode_pt12001_toc(Bytes) ->
    delimited_decode(pt12001_toc, Bytes).

delimited_decode_pt12002_tos(Bytes) ->
    delimited_decode(pt12002_tos, Bytes).

delimited_decode_pt12002_toc(Bytes) ->
    delimited_decode(pt12002_toc, Bytes).

delimited_decode_pt12003_toc(Bytes) ->
    delimited_decode(pt12003_toc, Bytes).

delimited_decode_pt12004_toc(Bytes) ->
    delimited_decode(pt12004_toc, Bytes).

delimited_decode_pt12005_tos(Bytes) ->
    delimited_decode(pt12005_tos, Bytes).

delimited_decode_pt12005_toc(Bytes) ->
    delimited_decode(pt12005_toc, Bytes).

delimited_decode_pt12006_toc(Bytes) ->
    delimited_decode(pt12006_toc, Bytes).

delimited_decode_pt12007_toc(Bytes) ->
    delimited_decode(pt12007_toc, Bytes).

delimited_decode_pt12008_toc(Bytes) ->
    delimited_decode(pt12008_toc, Bytes).

delimited_decode_pt12009_toc(Bytes) ->
    delimited_decode(pt12009_toc, Bytes).

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
decode(pt_player, Bytes) when is_binary(Bytes) ->
    Types = [{1, id, int32, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(pt_player, Decoded);
decode(pt_mon, Bytes) when is_binary(Bytes) ->
    Types = [{1, id, int32, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(pt_mon, Decoded);
decode(pt12001_tos, Bytes) when is_binary(Bytes) ->
    Types = [{7, scene, int32, []}, {6, skill, int32, []},
	     {5, d2, int32, []}, {4, d1, int32, []},
	     {3, fly, int32, []}, {2, y, int32, []},
	     {1, x, int32, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(pt12001_tos, Decoded);
decode(pt12001_toc, Bytes) when is_binary(Bytes) ->
    Types = [{9, skill, int32, []}, {8, d2, int32, []},
	     {7, d1, int32, []}, {6, fly, int32, []},
	     {5, server_id, int32, []}, {4, platform, string, []},
	     {3, id, int32, []}, {2, y, int32, []},
	     {1, x, int32, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(pt12001_toc, Decoded);
decode(pt12002_tos, Bytes) when is_binary(Bytes) ->
    Types = [],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(pt12002_tos, Decoded);
decode(pt12002_toc, Bytes) when is_binary(Bytes) ->
    Types = [{2, mons, pt_mon, [is_record, repeated]},
	     {1, players, pt_player, [is_record, repeated]}],
    Defaults = [{1, players, []}, {2, mons, []}],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(pt12002_toc, Decoded);
decode(pt12003_toc, Bytes) when is_binary(Bytes) ->
    Types = [{1, player, pt_player, [is_record]}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(pt12003_toc, Decoded);
decode(pt12004_toc, Bytes) when is_binary(Bytes) ->
    Types = [{3, server_id, int32, []},
	     {2, platform, string, []}, {1, id, int32, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(pt12004_toc, Decoded);
decode(pt12005_tos, Bytes) when is_binary(Bytes) ->
    Types = [{1, scene_id, int32, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(pt12005_tos, Decoded);
decode(pt12005_toc, Bytes) when is_binary(Bytes) ->
    Types = [{5, scene_id, int32, []},
	     {4, name, string, []}, {3, y, int32, []},
	     {2, x, int32, []}, {1, id, int32, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(pt12005_toc, Decoded);
decode(pt12006_toc, Bytes) when is_binary(Bytes) ->
    Types = [{1, id, int32, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(pt12006_toc, Decoded);
decode(pt12007_toc, Bytes) when is_binary(Bytes) ->
    Types = [{1, mon, pt_mon, [is_record]}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(pt12007_toc, Decoded);
decode(pt12008_toc, Bytes) when is_binary(Bytes) ->
    Types = [{5, piy, int32, []}, {4, pix, int32, []},
	     {3, y, int32, []}, {2, x, int32, []},
	     {1, id, int32, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(pt12008_toc, Decoded);
decode(pt12009_toc, Bytes) when is_binary(Bytes) ->
    Types = [{5, hp_lim, int32, []}, {4, hp, int32, []},
	     {3, server_id, int32, []}, {2, platform, string, []},
	     {1, id, int32, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(pt12009_toc, Decoded).

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

to_record(pt_player, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       pt_player),
						   Record, Name, Val)
			  end,
			  #pt_player{}, DecodedTuples),
    Record1;
to_record(pt_mon, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields, pt_mon),
						   Record, Name, Val)
			  end,
			  #pt_mon{}, DecodedTuples),
    Record1;
to_record(pt12001_tos, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       pt12001_tos),
						   Record, Name, Val)
			  end,
			  #pt12001_tos{}, DecodedTuples),
    Record1;
to_record(pt12001_toc, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       pt12001_toc),
						   Record, Name, Val)
			  end,
			  #pt12001_toc{}, DecodedTuples),
    Record1;
to_record(pt12002_tos, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       pt12002_tos),
						   Record, Name, Val)
			  end,
			  #pt12002_tos{}, DecodedTuples),
    Record1;
to_record(pt12002_toc, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       pt12002_toc),
						   Record, Name, Val)
			  end,
			  #pt12002_toc{}, DecodedTuples),
    Record1;
to_record(pt12003_toc, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       pt12003_toc),
						   Record, Name, Val)
			  end,
			  #pt12003_toc{}, DecodedTuples),
    Record1;
to_record(pt12004_toc, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       pt12004_toc),
						   Record, Name, Val)
			  end,
			  #pt12004_toc{}, DecodedTuples),
    Record1;
to_record(pt12005_tos, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       pt12005_tos),
						   Record, Name, Val)
			  end,
			  #pt12005_tos{}, DecodedTuples),
    Record1;
to_record(pt12005_toc, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       pt12005_toc),
						   Record, Name, Val)
			  end,
			  #pt12005_toc{}, DecodedTuples),
    Record1;
to_record(pt12006_toc, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       pt12006_toc),
						   Record, Name, Val)
			  end,
			  #pt12006_toc{}, DecodedTuples),
    Record1;
to_record(pt12007_toc, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       pt12007_toc),
						   Record, Name, Val)
			  end,
			  #pt12007_toc{}, DecodedTuples),
    Record1;
to_record(pt12008_toc, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       pt12008_toc),
						   Record, Name, Val)
			  end,
			  #pt12008_toc{}, DecodedTuples),
    Record1;
to_record(pt12009_toc, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       pt12009_toc),
						   Record, Name, Val)
			  end,
			  #pt12009_toc{}, DecodedTuples),
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

