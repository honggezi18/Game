%%%------------------------------------
%%% @author 严利宏 <542430172@qq.com>
%%% @copyright jieyou 2014.08.25
%%% @doc 游戏服务器开关.
%%% @end
%%%------------------------------------
-module(yx).
-export([server_start/0 % 开启游戏服务器
        ,server_stop/0  % 关闭游戏服务器
        ,node_stop/0    % 停止游戏节点
    ]).

-include("server.hrl").

-define(SERVER_APP, yx).

%% @doc 开启游戏服务器
server_start()->
    try
        ok = application:start(?SERVER_APP)
    catch 
        _E1:_E2 -> io:format("start server error : ~p~n", [[_E1, _E2]])
    end.

%% @doc 关闭游戏服务器
server_stop() ->
    io:format("close all lines ~n"),
    case mod_disperse:server_list() of
        [] ->
            [];
        Servers ->
            F = fun(SS) ->
                    Id = SS#server.id,
                    case Id == 0 orelse Id > 90 of
                        true->
                            ok;
                        false ->
                            case net_adm:ping(SS#server.node) of
                                pong ->
                                    case catch rpc:cast(SS#server.node, sd, server_stop, []) of
                                        _ ->
                                            ok
                                    end;
                                pang ->
                                    ok
                            end
                    end,
                    timer:sleep(3000)
                end,
            [F(S) || S <- Servers]
    end.

%% @doc 停止游戏节点
node_stop() ->
    io:format("close line [~p]~n", [mod_disperse:server_id()]),
    ok = mod_login:stop_all(),
    ok = application:stop(?SERVER_APP),
    erlang:halt().


