%%%------------------------------------
%%% @Module  : mod_clusters_node_info
%%% @Author  : xyao
%%% @Email   : jiexiaowen@gmail.com
%%% @Created : 2012.10.29
%%% @Description: 跨服中心info处理
%%%------------------------------------
-module(mod_clusters_node_info).
-export([handle_info/2]).

-include("clusters.hrl").

%% 连接跨服中心
handle_info('ping_center', Status) ->
    mod_clusters_node:reconnect_node(),
    rpc:cast(Status#clusters_node.center_node, mod_clusters_center, add_node, [node()]),
    erlang:send_after(?CONNECT_CENTER_TIME, self(), 'ping_center'),
    {noreply, Status#clusters_node{link = 1}};

%% 默认匹配
handle_info(Info, Status) ->
    catch util:errlog("mod_clusters_node_info:handle_info not match: ~p", [Info]),
    {noreply, Status}.
