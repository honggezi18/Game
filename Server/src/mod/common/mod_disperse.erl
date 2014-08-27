%%%------------------------------------
%%% @author 严利宏 <542430172@qq.com>
%%% @copyright jieyou 2014.08.27
%%% @doc 线路管理服务.
%%% @end
%%%------------------------------------

-module(mod_disperse).
-behaviour(gen_server).
-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link(Ip, Port, Sid) ->
    gen_server:start_link({local,?MODULE}, ?MODULE, [Ip, Port, Sid], []).

init([Ip, Port, Sid]) ->
    {ok, [Ip, Port, Sid]}.

handle_cast(_R , State) ->
    {noreply, State}.

handle_call(_R , _FROM, State) ->
    {reply, ok, State}.

handle_info(_Reason, State) ->
    {noreply, State}.

terminate(_R, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra)->
    {ok, State}.

