%%%------------------------------------
%%% @Module  : mod_clusters_center_call
%%% @Author  : xyao
%%% @Email   : jiexiaowen@gmail.com
%%% @Created : 2012.10.29
%%% @Description: 跨服中心call处理
%%%------------------------------------
-module(mod_clusters_center_call).
-export([handle_call/3]).

-include("clusters.hrl").

%% 统一模块+过程调用(call)
handle_call({'apply_call', Node, Module, Method, Args}, _From, State) ->
    Data = case dict:is_key(Node, State#clusters_center.node_list) of
        true ->
            rpc:call(Node, Module, Method, Args);
        false ->
            util:errlog("mod_clusters_center_call node:~p not find, {~p, ~p, ~p}", [Node, Module, Method, Args]),
            false
    end,
    {reply, Data, State};

%% 获取所有节点
handle_call({'get_all_node'}, _From, State) ->
    {reply, dict:fetch_keys(State#clusters_center.node_list), State};

%% 默认匹配
handle_call(Event, _From, Status) ->
    util:errlog("mod_clusters_center_call:handle_call not match: ~p", [Event]),
    {reply, ok, Status}.
