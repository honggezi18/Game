%%%------------------------------------
%%% @author 严利宏 <542430172@qq.com>
%%% @copyright jieyou 2014.08.25
%%% @doc tcp acceptor 监控树.
%%% @end
%%%------------------------------------
%%% @private

-module(yx_tcp_acceptor_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local,?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{simple_one_for_one, 10, 10},
          [{yx_tcp_acceptor, {yx_tcp_acceptor, start_link, []},
            transient, brutal_kill, worker, [yx_tcp_acceptor]}]}}.
