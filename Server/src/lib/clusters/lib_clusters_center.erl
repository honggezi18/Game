%%%-----------------------------------
%%% @Module  : lib_clusters_center
%%% @Author  : xyao
%%% @Email   : jiexiaowen@gmail.com
%%% @Created : 2012.10.29
%%% @Description: 只在跨服中心用的方法
%%%-----------------------------------
-module(lib_clusters_center).
-export([
        add_node_dict/2,
        send_to_uid/3,
        send_to_scene/3,
        send_to_scene/2,
        send_to_area_scene/5
	 ]).
-include("clusters.hrl").

%% 添加节点到dict结构
add_node_dict(D, Node) ->
    dict:store(Node, 1, D).

%% 给指定节点玩家发消息
send_to_uid(Node, Id, Bin) ->
    rpc:cast(Node, lib_server_send, send_to_uid, [Id, Bin]).

%%Sid:场景id
%%CopyId:副本ID
%%Bin:二进制数据.
send_to_scene(Sid, CopyId, Bin) ->
    mod_scene_agent:send_to_scene(Sid, CopyId, Bin).

%%Sid:场景id
%%Bin:二进制数据.
send_to_scene(Sid, Bin) ->
    mod_scene_agent:send_to_scene(Sid, Bin).

%%Sid:场景ID
%%CopyId:副本ID
%%X,Y坐标
%%Bin:数据
send_to_area_scene(Sid, CopyId, X, Y, Bin) ->
    mod_scene_agent:send_to_area_scene(Sid, CopyId, X, Y, Bin).
