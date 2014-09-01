%%%------------------------------------
%%% @Module  : mod_clusters_center_cast
%%% @Author  : xyao
%%% @Email   : jiexiaowen@gmail.com
%%% @Created : 2012.10.29
%%% @Description: 跨服中心cast处理
%%%------------------------------------
-module(mod_clusters_center_cast).
-export([handle_cast/2]).

-include("clusters.hrl").

%% 添加跨服节点
handle_cast({add_node, [Node]}, State) ->
    lib_clusters:add_node(Node),
    NodeList = lib_clusters_center:add_node_dict(State#clusters_center.node_list, Node),
    {noreply, State#clusters_center{node_list= NodeList}};

%% 统一模块+过程调用(cast)
handle_cast({'apply_cast', Node, Module, Method, Args} , State) ->
    case dict:is_key(Node, State#clusters_center.node_list) of
        true ->
            rpc:cast(Node, Module, Method, Args);
        false ->
            util:errlog("mod_clusters_center_cast node:~p not find, {~p, ~p, ~p}", [Node, Module, Method, Args]),
            false
    end,
    {noreply, State};

%% 通知所有节点执行
handle_cast({'apply_to_all_node', Module, Method, Args} , State) ->
    AllNode = dict:fetch_keys(State#clusters_center.node_list),
    [ rpc:cast(Node, Module, Method, Args) || Node <- AllNode],
    {noreply, State};

%% 通知所有节点
%% timeout延迟发送
handle_cast({'apply_to_all_node', Module, Method, Args, TimeOut} , State) ->
    spawn(
        fun() ->
                AllNode = dict:fetch_keys(State#clusters_center.node_list),
                F = fun(Node) ->
                        rpc:cast(Node, Module, Method, Args),
                        timer:sleep(TimeOut)
                end,
                [F(Node) || Node <- AllNode]
        end
    ),
    {noreply, State};

%% 默认匹配
handle_cast(Event, Status) ->
    catch util:errlog("mod_clusters_center_cast:handle_cast not match: ~p", [Event]),
    {noreply, Status}.