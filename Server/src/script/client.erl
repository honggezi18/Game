%%%-------------------------------------------------------------------
%%% @author JoyHuang
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%           模拟客户端
%%% @end
%%% Created : 14. 八月 2014 19:09
%%%-------------------------------------------------------------------
-module(client).

%% API
-export([start/0]).
-compile(export_all).
-include("pt100_pb.hrl").

start() ->
    case gen_tcp:connect("192.168.7.13", 8010, [binary, {packet, 0}]) of
		{ok, Socket} ->
            Pid = spawn(fun() -> rec(Socket) end),
            ok = gen_tcp:controlling_process(Socket, Pid),
			login(Socket, 1),
            timer:sleep(1000),
            get_role_list(Socket, 1),
            %timer:sleep(1000),
            %create_role(Socket),
            timer:sleep(1000),
            enter(Socket),
            timer:sleep(1000),
%%             loadscene(Socket),
%%             timer:sleep(1000),
            %rec(Socket),
            ok;
		{error, _Reason} ->
            io:format("connect failed!~n")
    end.

%%验证登陆
login(Socket, Accid) ->
    Tstamp = 1408016543,
    Dbin = pt100_pb:encode(#pt10000_tos{acc_id=Accid, accname="我", time=Tstamp, ticket="123456"}),
    Bin = erlang:iolist_to_binary(Dbin),
    L = byte_size(Bin) + 4,
    gen_tcp:send(Socket, <<L:16, 10000:16, Bin/binary>>),
    io:format("==>Send 10000 ~p~n", [self()]),
    ok.
%    rec(Socket).

get_role_list(Socket, _Accid) ->
    {ok, B} = pack(#pt10002_tos{}),
    gen_tcp:send(Socket, B),
    io:format("==>Send 10002 ~p~n", [self()]),
    ok.

create_role(Socket) ->
    {ok, Bin} = pack(#pt10003_tos{realm=1,career=2,sex=1,name="你好啊"}),
    gen_tcp:send(Socket, <<Bin/binary>>),
    io:format("==>Send 10003~n"),
    ok.
%%     rec(Socket).

%%玩家列表
%player_list(Socket) ->
%    gen_tcp:send(Socket, <<4:16,10002:16>>),
%    rec(Socket).

%%选择角色进入
enter(Socket) ->
    Id = 2,
    Time = 1408016543,
    {ok, Bin} = pack(#pt10004_tos{id=Id,time=Time,ticket="aa"}),
    gen_tcp:send(Socket, <<Bin/binary>>),
    io:format("==>Send 10004~n"),
    ok.%rec(Socket).

loadscene(Socket) ->
    L = byte_size(<<1:16, 12002:16, 1:8>>),
    gen_tcp:send(Socket, <<L:16, 12002:16, 1:8>>),
    io:format("==>Send 12002~n"),
    rec(Socket).

rec(Socket) ->
    receive
        {tcp, Socket, <<"<cross-domain-policy><allow-access-from domain='*' to-ports='*' /></cross-domain-policy>">>} ->
            io:format("revc : ~p~n", ["flash_file"]),
            rec(Socket);
        %%用户信息
        {tcp, Socket, <<_L:32, 10000:16, _Zip:8, _Code:32, _Num:8, _Career:8, _Time:32>>} ->
            io:format("==>10000~n", []);
        {tcp, Socket, <<_L:32, 10004:16, _Bin/binary>>} ->
            io:format("==>10004~n", []);
        {tcp, Socket, <<_L:32, 12002:16, _Bin/binary>>} ->
            io:format("==>12002~n", []),
            rec(Socket);
        {tcp, Socket, <<_L:16, Cmd:16, Bin/binary>>} ->
            [H1,H2,H3,H4,H5 | _] = integer_to_list(Cmd),
            Module = list_to_atom([$p,$t,H1,H2,H3,$_,$p,$b]), % atom = pt123_pb
            RecordName = list_to_atom([$p,$t,H1,H2,H3,H4,H5,$_,$t,$o,$c]), % atom = decode_pt12345_toc
            RecordData = Module:decode(RecordName, Bin),
            io:format("<==Rec Bin = ~p~n", [RecordData]),
            rec(Socket);
        {tcp, Socket, Bin} ->
            io:format("<== Bin = ~p~n", [Bin]),
            rec(Socket);
        {tcp_closed, Socket} ->
            io:format("==>Tcp close~n"),
            gen_tcp:close(Socket)
    end.

%% for(Max, Max, _F, X) ->
%%     X;
%% for(Min, Max, F, X) ->
%%     X1 = F(X),
%%     for(Min+1, Max, F, X1).

%%读取字符串
%% read_string(Bin) ->
%%     case Bin of
%%         <<Len:16, Bin1/binary>> ->
%%             case Bin1 of
%%                 <<Str:Len/binary-unit:8, Rest/binary>> ->
%%                     {binary_to_list(Str), Rest};
%%                 _R1 ->
%%                     {[],<<>>}
%%             end;
%%         _R1 ->
%%             {[],<<>>}
%%     end.
%%
%% %%打包字符串
%% write_string(S) when is_list(S)->
%%     SB = iolist_to_binary(S),
%%     L = byte_size(SB),
%%     <<L:16, SB/binary>>;
%%
%% write_string(S) when is_binary(S)->
%%     L = byte_size(S),
%%     <<L:16, S/binary>>.

pack(RecordData) ->
    RecordName = erlang:element(1, RecordData),
    [$p,$t,H1,H2,H3,H4,H5,$_,$t,$o,$s] = atom_to_list(RecordName), % H1-H5 是协议号
    Module = list_to_atom([$p,$t,H1,H2,H3,$_,$p,$b]),  % 根据协议号前3位取得功能号，组成模块名
    DBin = Module:encode(RecordData),
    Bin = erlang:iolist_to_binary(DBin),
    Len = byte_size(Bin) + 4,
    Cmd = list_to_integer([H1,H2,H3,H4,H5]),
    {ok, <<Len:16, Cmd:16, Bin/binary>>}.
