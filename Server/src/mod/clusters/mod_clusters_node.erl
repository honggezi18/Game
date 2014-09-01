%%%------------------------------------
%%% @Module  : mod_clusters_node
%%% @Author  : xyao
%%% @Email   : jiexiaowen@gmail.com
%%% @Created : 2012.10.29
%%% @Description: 跨服节点管理 
%%%------------------------------------
-module(mod_clusters_node). 
-behaviour(gen_server).
-export([
            start_link/0,
            get_link_state/0,
            %apply_call/3,
            apply_cast/3,
            reconnect_node/0
        ]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("clusters.hrl").
-include("record.hrl").

%% 跨服节点调用跨服中心函数 - call (禁用这个方法，以免堵塞进程)
%apply_call(Module, Method, Args) ->
%    Node = mod_disperse:get_clusters_node(),
%    case Node =/= none of
%        true ->
%            case catch rpc:call(Node, gen_server, call, [?MODULE, {'apply_call', Module, Method, Args}]) of
%                {badrpc, Reason}->
%                    util:errlog("ERROR mod_clusters_node apply_call/4 function: ~p Reason : ~p~n", [{Module, Method, Args}, Reason]),
%                    none;
%                Data ->
%                    Data
%            end;
%        false ->
%            none
%    end.

%% 跨服节点调用跨服中心函数 - cast
apply_cast(Module, Method, Args) ->
    Node = mod_disperse:get_clusters_node(),
    case Node =/= none of
        true ->
            rpc:cast(Node, gen_server, cast, [?MODULE, {'apply_cast', Module, Method, Args}]);
        false ->
            none
    end.

%% 获取连接状态
get_link_state() ->
    gen_server:call(?MODULE, {get_link_state}).

%% 节点重连
reconnect_node() ->
    List = mod_disperse:node_all_list(),
    [erlang:disconnect_node(N#node.node) || N <-List],
    [net_adm:ping(N#node.node) || N <-List].

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    Cn = #clusters_node{
        center_node = config:get_cls_node()
    },
    erlang:set_cookie(config:get_cls_node(), config:get_cls_cookie()),
    net_adm:ping(config:get_cls_node()),
    erlang:send_after(10000, self(), 'ping_center'),
    {ok, Cn}.

handle_cast(R , State) ->
    case catch mod_clusters_node_cast:handle_cast(R, State) of
        {noreply, NewState} ->
            {noreply, NewState};
        Reason ->
            util:errlog("mod_clusters_node_cast error: ~p, Reason:=~p~n",[R, Reason]),
            {noreply, State}
    end.

handle_call(R, From, State) ->
    case catch mod_clusters_node_call:handle_call(R , From, State) of
        {reply, NewFrom, NewState} ->
            {reply, NewFrom, NewState};
        Reason ->
             util:errlog("mod_clusters_node_call error: ~p, Reason=~p~n",[R, Reason]),
             {reply, error, State}
    end.

handle_info(R, State) ->
    case catch mod_clusters_node_info:handle_info(R, State) of
        {noreply, NewState} ->
            {noreply, NewState};
        Reason ->
            util:errlog("mod_clusters_node_info error: ~p, Reason:=~p~n",[R, Reason]),
            {noreply, State}
    end.

terminate(R, State) ->
    util:errlog("mod_clusters_node is terminate:~p", [R]),
    {ok, State}.

code_change(_OldVsn, State, _Extra)->
    {ok, State}.

