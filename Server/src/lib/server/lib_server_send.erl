%%%-----------------------------------
%%% @Module  : lib_server_send
%%% @Author  : xyao
%%% @Email   : jiexiaowen@gmail.com
%%% @Created : 2011.07.18
%%% @Description: 发送消息
%%%-----------------------------------
-module(lib_server_send).
-include("common.hrl").
-include("server.hrl").
-include("record.hrl").
-export([
        send_one/2,
        send_to_sid/2,
        send_to_sid/3,
        send_to_uid/2,
        send_to_scene/2,
        send_to_scene/3,
        send_to_area_scene/5,
		send_to_local_all/1,
        send_to_all/1,
        send_to_local_all/2,
        send_to_all/2
       ]).

%%发送信息给指定socket玩家.
%%S:socket
%%Bin:二进制数据.
send_one(S, Bin) ->
    gen_tcp:send(S, Bin).

%%发送信息给指定sid玩家.
%%sid:发送进程组
%%Bin:二进制数据.
send_to_sid(Sid, Bin) ->
    rand_to_process_by_sid(Sid) ! {send, Bin}.

%% 在指定的节点执行send_to_sid方法
send_to_sid(Node, Sid, Bin) ->
    case Node =/= none of
        true ->
            rpc:cast(Node, ?MODULE, send_to_sid, [Sid, Bin]);
        false ->
            send_to_sid(Sid, Bin)
    end.
    
%%给id玩家信息
%%id：玩家id
%%Bin：二进制数据.
send_to_uid(Id, Bin) ->
    case rand_to_process_by_id(Id) of
        undefined ->
            skip;
        Pid ->
            Pid ! {send, Bin}
    end.

%%给（任意节点）的场景发送信息
%%Sid:场景id
%%CopyId:副本ID
%%Bin:二进制数据.
send_to_scene(Sid, CopyId, Bin) ->
    mod_scene_agent:send_to_scene(Sid, CopyId, Bin).

%% 给（任意节点）的场景发送信息
send_to_scene(Sid, Bin) ->
    mod_scene_agent:send_to_scene(Sid, Bin).


%%给（任意节点）场景区域(9宫格区域)发信息
%%Sid:场景ID
%%CopyId:副本ID
%%X,Y坐标
%%Bin:数据
send_to_area_scene(Sid, CopyId, X, Y, Bin) ->
    mod_scene_agent:send_to_area_scene(Sid, CopyId, X, Y, Bin).

%% 给当前节点的玩家发信息
send_to_local_all(Bin) ->
    Node = mod_disperse:node_id(),
    case Node =/= 1 of
        true ->
            L = ets:match(?ETS_ONLINE, #ets_online{sid='$1', _='_'}),
            do_broadcast(L, Bin);
        _ -> skip
    end.

%% 给所有游戏节点的玩家发信息
send_to_all(Bin) ->
    send_to_local_all(Bin),
    [rpc:cast(Node#node.node, lib_server_send, send_to_local_all, [Bin]) || Node <- mod_disperse:node_list()].

%% -----------------------------------------------------------------
%% 广播消息给进程
%% -----------------------------------------------------------------
send_to_all(MsgType, Bin) ->
    send_to_local_all(MsgType, Bin),
    [rpc:cast(Node#node.node, lib_server_send, send_to_local_all, [MsgType, Bin]) || Node <- mod_disperse:node_list()].

%% 给当前节点的玩家发信息
send_to_local_all(MsgType, Bin) ->
    Pids = ets:match(?ETS_ONLINE, #ets_online{pid='$1', _='_'}),
    F = fun([P]) ->
        gen_server:cast(P, {MsgType, Bin})
    end,
    [F(Pid) || Pid <- Pids].
%% 对列表中的所有socket进行广播
do_broadcast(L, Bin) ->
    F = fun([S]) ->
        send_to_sid(S, Bin)
    end,
    [F(D) || D <- L].

rand_to_process_by_id(Id) ->
    misc:whereis_name(global, misc:player_send_process_name(Id, random:uniform(?SERVER_SEND_MSG))).

rand_to_process_by_sid(Sid) ->
    element(random:uniform(?SERVER_SEND_MSG), Sid).
