%%%------------------------------------
%%% @Module     : mod_change_scene_cls_center
%%% @Author     : hc
%%% @Created    : 2010.12.31
%%% @Description: 换线排队
%%%------------------------------------
-module(mod_change_scene_cls_center).
-behaviour(gen_fsm).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, code_change/4, terminate/3, heart/0]).
-export(
    [
        start_link/0,
        heartbeat/2,
        stop/0,
        cls_cen_change_scene_queue/8,
        cls_node_change_scene_queue/6
    ]).

-define(TIMEOUT, 2*1000).
-define(MAX_PEOPLE, 30).
%%%------------------------------------
%%%             接口函数
%%%------------------------------------

%% 启动
start_link() ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

%% 停止
stop() ->
    gen_fsm:send_event(?MODULE, 'stop').

heart() -> 
    ?MODULE ! 'heartbeat'.

%% 开始进入排队传送到跨服节点
cls_cen_change_scene_queue(Node, PlayerId, Pid, SceneId, CopyId, X, Y, Value) -> 
    catch ?MODULE ! {'change_scene', Node, PlayerId, Pid, SceneId, CopyId, X, Y, Value}.

%% 进入场景,由跨服节点告诉游戏服执行
cls_node_change_scene_queue(Pid, SceneId, CopyId, X, Y, Value) -> 
    catch gen_server:cast(Pid, {'change_scene', [SceneId, CopyId, X, Y, Value]}).

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
handle_info({'change_scene', Node, PlayerId, Pid, SceneId, CopyId, X, Y, Value}, _StateName, State) ->
    NewState = case lists:keyfind(PlayerId, 2, State) of
        false -> State++[{Node, PlayerId, Pid, SceneId, CopyId, X, Y, Value}];
        _ -> State
    end,
    {next_state, heartbeat, NewState};

%% 下线退出排队
handle_info({'offline', PlayerId}, _StateName, State) ->
    NewState = case lists:keyfind(PlayerId, 2, State) of
        false -> State;
        _ -> lists:keydelete(PlayerId, 2, State)
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
change_line(State) -> 
    manger(State, 1).

manger([], _) -> [];
manger(List, Pre) when Pre >= ?MAX_PEOPLE -> List;
manger([{Node, _PlayerId, Pid, SceneId, CopyId, X, Y, Value} | T], Pre) -> %% 跨服排队
    catch mod_clusters_center:apply_cast(Node, mod_change_scene_cls_center, cls_node_change_scene_queue, [Pid, SceneId, CopyId, X, Y, Value]),
    manger(T, Pre+1).
