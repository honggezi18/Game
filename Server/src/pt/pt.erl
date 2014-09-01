%%%-----------------------------------
%%% @Module  : pt
%%% @Author  : xyao
%%% @Email   : jiexiaowen@gmail.com
%%% @Created : 2011.06.30
%%% @Description: 协议公共函数
%%%-----------------------------------
-module(pt).
-export([
            read_string/1,
            write_string/1,
            pack/1,
            pack/2,
            pack/3,
            read_id_num_list/3,
            read_id_list/3,
			get_time_stamp/0,
            read_id8_list/3
            ,player_record/1
            ,mon_record/1
        ]).

-include("server.hrl").
-include("scene.hrl").
-include("pt120_pb.hrl").

%%读取{ID，数量}列表 -> {Rest, IdNumList}
read_id_num_list(<<Id:32, Num:16, Rest/binary>>, List, ListNum) when ListNum > 0 ->
    NewList = case lists:keyfind(Id, 1, List) of
                {_,N} -> lists:keyreplace(Id, 1, List, {Id,(N + Num)});
                false -> [{Id,Num}|List]
            end,
    read_id_num_list(Rest, NewList, ListNum-1);
read_id_num_list(Rest, List, _) -> {Rest, List}.

%%读取Id列表 -> {Rest, IdList}
read_id_list(<<Id:32, Rest/binary>>, L, Num) when Num > 0 ->
    NewL = case lists:member(Id, L) of
               false -> [Id|L];
               true -> L
           end,
    read_id_list(Rest, NewL, Num-1);
read_id_list(Rest, L, _) -> {Rest, L}.

%%读取Id列表
read_id8_list(<<Id:8, Rest/binary>>, L, Num) when Num > 0 ->
    NewL = case lists:member(Id, L) of
               false -> [Id|L];
               true -> L
           end,
    read_id8_list(Rest, NewL, Num-1);
read_id8_list(Rest, L, _) -> {Rest, L}.

%%读取字符串
read_string(Bin) ->
    case Bin of
        <<Len:16, Bin1/binary>> ->
            case Bin1 of
                <<Str:Len/binary-unit:8, Rest/binary>> ->
                    {binary_to_list(Str), Rest};
                _R1 ->
                    {[],<<>>}
            end;
        _R1 ->
            {[],<<>>}
    end.

%%打包字符串
write_string(S) when is_list(S)->
    SB = iolist_to_binary(S),
    L = byte_size(SB),
    <<L:16, SB/binary>>;

write_string(S) when is_binary(S)->
    L = byte_size(S),
    <<L:16, S/binary>>;

write_string(S) when is_integer(S)->
	SS = integer_to_list(S),
	SB = list_to_binary(SS),
    L = byte_size(SB),
    <<L:16, SB/binary>>;

write_string(_S) ->
	%util:errlog("pt:write_string error, Error = ~p~n", [S]),
	<<0:16, <<>>/binary>>.

%% 旧代码 : 使用自定义二进制流
%% 打包信息，添加消息头
%% Zip:1压缩0不压缩 默认不压缩
%pack(Cmd, Data) ->
%    pack(Cmd, Data, 0).
%pack(Cmd, Data, Zip) ->
%    case Zip =:= 1 of
%        true ->
%            Data1 = zlib:compress(Data),
%            L = byte_size(Data1) + 7,
%            <<L:32, Cmd:16, Zip:8, Data1/binary>>;
%        false ->
%            L = byte_size(Data) + 7,
%            <<L:32, Cmd:16, Zip:8, Data/binary>>
%    end.
%% 新代码 : 使用protobuf
pack(RecordData) ->
    RecordName = erlang:element(1, RecordData),
    [$p,$t,H1,H2,H3,H4,H5,$_,$t,$o,$c] = atom_to_list(RecordName), % H1-H5 是协议号
    Module = list_to_atom([$p,$t,H1,H2,H3,$_,$p,$b]),  % 根据协议号前3位取得功能号，组成模块名
    DBin = Module:encode(RecordData),
    Bin = erlang:iolist_to_binary(DBin),
    Len = byte_size(Bin) + 4,
    Cmd = list_to_integer([H1,H2,H3,H4,H5]),
    {ok, <<Len:16, Cmd:16, Bin/binary>>}.
pack(Cmd, RecordData) ->
    [H1,H2,H3,_H4,_H5 | _] = integer_to_list(Cmd),
    Module = list_to_atom([$p,$t,H1,H2,H3,$_,$p,$b]), % atom = pt123_pb
    DBin = Module:encode(RecordData),
    Bin = erlang:iolist_to_binary(DBin),
    Len = byte_size(Bin) + 4,
    {ok, <<Len:16, Cmd:16, Bin/binary>>}.
pack(Cmd, RecordData, _) -> pack(Cmd, RecordData).

%% 获得当前的时间戳
get_time_stamp() ->
	{M, S, _} = erlang:now(),
	TS = M * 1000000 + S,
	TS.


% 组织玩家协议record
player_record(PlayerStatus) when is_record(PlayerStatus, player_status) ->
    #pt_player{id = PlayerStatus#player_status.id
    };
player_record(SceneUser) when is_record(SceneUser, ets_scene_user) ->
    #pt_player{id = SceneUser#ets_scene_user.id
    }.

%% 组织怪物协议record
mon_record(SceneMon) when is_record(SceneMon, ets_mon) ->
    #pt_mon{id=SceneMon#ets_mon.id
    }.
