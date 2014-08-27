%%%------------------------------------
%%% @author 严利宏 <542430172@qq.com>
%%% @copyright jieyou 2014.08.25
%%% @doc 时间生成. 游戏时间，每100ms更新一次ets缓存。游戏逻辑直接从这边获取当前时间。
%%% @end
%%%------------------------------------

-module(mod_time).
-behaviour(gen_server).

-export([now/0, now_seconds/0, cpu_time/0, start_link/0, info/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(CLOCK, 100).

%% ====================================================================
%% External functions
%% ====================================================================
now() -> 
	[{timer, {Now, _}}] = get_ets(),
	Now.

now_seconds()->
	[{timer, {Now, _}}] = get_ets(),
	{MegaSecs, Secs, _MicroSecs} = Now,
	lists:concat([MegaSecs, Secs]).

cpu_time() -> 
	[{timer, {_, Wallclock_Time_Since_Last_Call}}] = get_ets(),
	Wallclock_Time_Since_Last_Call.

info() ->
	[
	 ets:info(ets_sys_time), 
     ets:tab2list(ets_sys_time)
    ].

get_ets() ->
    case ets:lookup(ets_sys_time, timer) of
        [] ->
            [{timer, {erlang:now(), 0}}];
        [{timer, {Now, Wallclock_Time_Since_Last_Call}}] ->
            [{timer, {Now, Wallclock_Time_Since_Last_Call}}]
    end.

%% ====================================================================
%% Server functions
%% ====================================================================

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
	ets:new(ets_sys_time, [set, protected, named_table]),
	ets:insert(ets_sys_time, {timer, {erlang:now(), 0}}),
	erlang:send_after(?CLOCK, self(), {event, clock}),
	{ok, []}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {reply, State, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(_Msg, State) ->
	{noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({event, clock}, State) ->
%% 	{_Total_Wallclock_Time, Wallclock_Time_Since_Last_Call}= statistics(wall_clock),
 	{_Total_Run_Time, Time_Since_Last_Call} = statistics(runtime),
	ets:insert(ets_sys_time, {timer, {erlang:now(), Time_Since_Last_Call}}),
	erlang:send_after(?CLOCK, self(), {event, clock}),
	{noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ets:delete_all_objects(ets_sys_time),
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
