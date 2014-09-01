%%%------------------------------------
%%% @Module  : mod_clusters_center
%%% @Author  : xyao
%%% @Email   : jiexiaowen@gmail.com
%%% @Created : 2012.10.29
%%% @Description: 跨服中心管理 
%%%------------------------------------
-module(mod_clusters_center). 
-behaviour(gen_server).
-export([
            start_link/0,
            add_node/1,
            %apply_call/4,
            apply_cast/4,
            apply_to_all_node/3,
            apply_to_all_node/4,
            get_all_node/0,
            cast_node_restart/1
        ]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("clusters.hrl").

%% 通过场景调用函数 - call(禁用这个方法，以免堵塞进程)
%apply_call(Node, Module, Method, Args) ->
%     rpc:call(Node, Module, Method, Args).
    %case catch gen:call(?MODULE, '$gen_call', {'apply_call', Node, Module, Method, Args}) of
    %    {ok, Res} ->
    %        Res;
    %    Reason ->
    %        util:errlog("ERROR mod_clusters_center apply_call/4 function: ~p Reason : ~p~n", [{Module, Method, Args}, Reason]),
    %        skip
    %end.

%% 通过场景调用函数 - cast
apply_cast(Node, Module, Method, Args) ->
    rpc:cast(Node, Module, Method, Args).
    %gen_server:cast(?MODULE, {'apply_cast', Node, Module, Method, Args}).

    %% 通知所有节点执行
apply_to_all_node(Module, Method, Args) ->
    gen_server:cast(?MODULE, {'apply_to_all_node', Module, Method, Args}).

%% 通知所有节点执行
%% TimeOut:毫秒
apply_to_all_node(Module, Method, Args, TimeOut) ->
    gen_server:cast(?MODULE, {'apply_to_all_node', Module, Method, Args, TimeOut}).

%% 添加跨服节点
add_node(Node) ->
   gen_server:cast(?MODULE, {add_node, [Node]}). 

%% 获取所有节点
get_all_node() ->
    gen_server:call(?MODULE, {'get_all_node'}). 

%% 通知重启
cast_node_restart(Node) ->
    case rpc:call(Node, init, restart, []) of
        ok ->
            1;
        _ ->
            0
    end.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    NodeList = case db:get_all(<<"select `node` from clusters">>) of
        [] ->
            dict:new();
        Server ->
            F = fun([Node], Data) ->
                    Node1 = list_to_atom(binary_to_list(Node)),
                    case net_adm:ping(Node1) of
                        pong ->
                            rpc:cast(Node1, mod_clusters_node, reconnect_node, []),
                            lib_clusters_center:add_node_dict(Data, Node1);
                        pang ->
                            lib_clusters:del_node(Node1),
                            Data
                    end
            end,
            lists:foldl(F, dict:new(), Server)
    end,
    State = #clusters_center{
        node_list = NodeList
    },
    {ok, State}.

handle_cast(R , State) ->
    case catch mod_clusters_center_cast:handle_cast(R, State) of
        {noreply, NewState} ->
            {noreply, NewState};
        Reason ->
            util:errlog("mod_clusters_center_cast error: ~p, Reason:=~p~n",[R, Reason]),
            {noreply, State}
    end.

handle_call(R, From, State) ->
    case catch mod_clusters_center_call:handle_call(R , From, State) of
        {reply, NewFrom, NewState} ->
            {reply, NewFrom, NewState};
        Reason ->
             util:errlog("mod_clusters_center_call error: ~p, Reason=~p~n",[R, Reason]),
             {reply, error, State}
    end.

handle_info(R, State) ->
    case catch mod_clusters_center_info:handle_info(R, State) of
        {noreply, NewState} ->
            {noreply, NewState};
        Reason ->
            util:errlog("mod_clusters_center_info error: ~p, Reason:=~p~n",[R, Reason]),
            {noreply, State}
    end.

terminate(R, State) ->
    util:errlog("mod_clusters_center is terminate:~p", [R]),
    {ok, State}.

code_change(_OldVsn, State, _Extra)->
    {ok, State}.

