%%%------------------------------------
%%% @Module  : mod_clusters_node_call
%%% @Author  : xyao
%%% @Email   : jiexiaowen@gmail.com
%%% @Created : 2012.10.29
%%% @Description: 跨服节点call处理
%%%------------------------------------
-module(mod_clusters_node_call).
-export([handle_call/3]).

-include("clusters.hrl").

%% 获取连接状态
handle_call({get_link_state}, _From, State) ->
    {reply, State#clusters_node.link, State};

%% 统一模块+过程调用(call)
handle_call({'apply_call', Module, Method, Args}, _From, State) ->
    {reply, rpc:call(State#clusters_node.center_node, Module, Method, Args), State};

%% 默认匹配
handle_call(Event, _From, Status) ->
    catch util:errlog("mod_clusters_node_call:handle_call not match: ~p", [Event]),
    {reply, ok, Status}.
