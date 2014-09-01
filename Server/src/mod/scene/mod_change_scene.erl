%%%------------------------------------
%%% @Module     : mod_change_scene
%%% @Author     : hc
%%% @Created    : 2010.12.31
%%% @Description: 换线排队
%%%------------------------------------
-module(mod_change_scene).
-behaviour(gen_fsm).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, code_change/4, terminate/3, heart/0]).
-export(
    [
        start_link/0,
        heartbeat/2,
        stop/0
    ]).

-define(TIMEOUT, 5*1000).
-define(MAX_PEOPLE, 20).
%%%------------------------------------
%%%             接口函数
%%%------------------------------------

%% 启动
start_link() ->
    gen_fsm:start_link({global, ?MODULE}, ?MODULE, [], []).

%% 停止
stop() ->
    gen_fsm:send_event(misc:get_global_pid(?MODULE), 'stop').

heart() -> 
    misc:get_global_pid(?MODULE) ! 'heartbeat'.

%%%------------------------------------
%%%             回调函数
%%%------------------------------------

init(_) ->
    process_flag(trap_exit, true),
    gen_fsm:send_event_after(?TIMEOUT, repeat),
    {ok, heartbeat, []}.

heartbeat(_R, StateData) ->
    NewState = change_line(StateData),
    gen_fsm:send_event_after(?TIMEOUT, repeat),
    {next_state, heartbeat, NewState}.

handle_event('stop', _StateName, StateData) ->
    {stop, normal, StateData};

handle_event(_Event, _StateName, StateData) ->
    {next_state, heartbeat, StateData}.

handle_sync_event(_Event, _From, _StateName, StateData) ->
    {next_state, heartbeat, StateData}.

code_change(_OldVsn, _StateName, State, _Extra) ->
    {ok, heartbeat, State}.

%% 排队切换场景报名
handle_info({'change_scene', PlayerId, Pid, SceneId, CopyId, X, Y, Value}, _StateName, State) ->
    NewState = case lists:keyfind(PlayerId, 1, State) of
        false -> State++[{PlayerId, Pid, SceneId, CopyId, X, Y, Value}];
        _ -> State
    end,
    {next_state, heartbeat, NewState};

%% 下线退出排队
handle_info({'offline', PlayerId}, _StateName, State) ->
    NewState = case lists:keyfind(PlayerId, 1, State) of
        false -> State;
        _ -> lists:keydelete(PlayerId, 1, State)
    end,
    {next_state, heartbeat, NewState};

handle_info('heartbeat', _StateName, StateData) ->
    NewState = change_line(StateData),
    gen_fsm:send_event_after(?TIMEOUT, repeat),
    {next_state, heartbeat, NewState};

handle_info(_Any, _StateName, State) ->
    {next_state, heartbeat, State}.

terminate(_Any, _StateName, _Opts) ->
    ok.

%换线处理
change_line(State) -> manger(State, 1).

manger([], _) -> [];
manger(List, Pre) when Pre >= ?MAX_PEOPLE -> List;
manger([{_PlayerId, Pid, SceneId, CopyId, X, Y, Value} | T], Pre) ->
    catch gen_server:cast(Pid, {'change_scene', [SceneId, CopyId, X, Y, Value]}),
    manger(T, Pre+1).
