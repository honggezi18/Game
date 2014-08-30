%%%------------------------------------
%%% @author xyao <jiexiaowen@gmail.com>
%%% @copyright jieyou 2014.08.25
%%% @doc 客户端服务监控树.
%%% @end
%%%------------------------------------
%%% @private

-module(yx_tcp_client_sup).
-behaviour(supervisor).
-export([start_link/1, init/1]).

%%公共服务
start_link(unite) ->
    supervisor:start_link({local,?MODULE}, ?MODULE, [unite]);

%%游戏服务
start_link(server) ->
    supervisor:start_link({local,?MODULE}, ?MODULE, [server]).

init([unite]) ->
    {ok, {{simple_one_for_one, 10, 10},
          [{yx_reader, {yx_reader,start_link,[unite]},
            temporary, brutal_kill, worker, [yx_reader]}]}};

init([server]) ->
    {ok, {{simple_one_for_one, 10, 10},
          [{yx_reader, {yx_reader,start_link,[server]},
            temporary, brutal_kill, worker, [yx_reader]}]}}.

