%%%------------------------------------
%%% @author xyao <jiexiaowen@gmail.com>
%%% @copyright jieyou 2014.08.25
%%% @doc tcp listerner 监控树.
%%% @end
%%%------------------------------------
%%% @private

-module(yx_tcp_listener_sup).
-behaviour(supervisor).
-export([start_link/1, init/1]).

start_link(Port) ->
    supervisor:start_link(?MODULE, {10, Port}).

init({AcceptorCount, Port}) ->
    {ok,
        {{one_for_all, 10, 10},
            [
                {
                    yx_tcp_acceptor_sup,
                    {yx_tcp_acceptor_sup, start_link, []},
                    transient,
                    infinity,
                    supervisor,
                    [yx_tcp_acceptor_sup]
                },
                {
                    yx_tcp_listener,
                    {yx_tcp_listener, start_link, [AcceptorCount, Port]},
                    transient,
                    100,
                    worker,
                    [yx_tcp_listener]
                }
            ]
        }
    }.
