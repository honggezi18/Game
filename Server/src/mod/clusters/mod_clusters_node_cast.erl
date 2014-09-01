%%%------------------------------------
%%% @Module  : mod_clusters_node_cast
%%% @Author  : xyao
%%% @Email   : jiexiaowen@gmail.com
%%% @Created : 2012.10.29
%%% @Description: 跨服中心cast处理
%%%------------------------------------
-module(mod_clusters_node_cast).
-export([handle_cast/2]).

-include("clusters.hrl").

%% 统一模块+过程调用(cast)
handle_cast({'apply_cast', Module, Method, Args}, State) ->
    rpc:cast(State#clusters_node.center_node, Module, Method, Args),
    {noreply, State};

%% 默认匹配
handle_cast(Event, Status) ->
    catch util:errlog("mod_clusters_node_cast:handle_cast not match: ~p", [Event]),
    {noreply, Status}.


    
