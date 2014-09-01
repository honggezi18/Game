%%%------------------------------------
%%% @Module  : mod_mon_agent
%%% @Author  : xyao
%%% @Email   : jiexiaowen@gmail.com
%%% @Created : 2012.05.21
%%% @Description: 怪物管理
%%%------------------------------------
-module(mod_mon_agent).
-behaviour(gen_server).

-export([
    start_link/2,
    start_mod_mon_agent/2,
    close_scene/2,
    apply_call/5,
    apply_cast/5,
    get_scene_mon_pid/2
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("server.hrl").
-include("scene.hrl").

apply_call(Sid, CopyId, Module, Method, Args) ->
    case get_scene_mon_pid(Sid, CopyId) of
        undefined ->
            skip;
        Pid ->
            case catch gen:call(Pid, '$gen_call', {'apply_call', Module, Method, Args}) of
                {ok, Res} ->
                    Res;
                _R ->
                    skip
            end
    end.

apply_cast(Sid, CopyId, Module, Method, Args) ->
    case get_scene_mon_pid(Sid, CopyId) of
        undefined ->
            skip;
        Pid ->
            gen_server:cast(Pid, {'apply_cast', Module, Method, Args})
    end.

%% 关闭指定的场景
close_scene(Sid, CopyId) ->
    MonProcessName = misc:mon_process_name(Sid, CopyId),
    case misc:whereis_name(local, MonProcessName) of
        Pid when is_pid(Pid) ->
            %% 关闭前取消注册
            misc:unregister(local, MonProcessName),
            case misc:is_process_alive(Pid) of
                true ->
                    gen_server:cast(Pid, {'close_scene'});
                false ->
                    skip
            end;
        _ ->
            skip
    end.

start_link(Scene, CopyId) ->
    gen_server:start(?MODULE, [Scene, CopyId], []).

%% Num:场景个数
%% WorkerId:进行标示
%% Scene:场景相关内容
init([Scene, CopyId]) ->
    process_flag(trap_exit, true),
    erlang:send_after(3600 * 1000, self(), {'close_scene'}),
    %设置战斗节点类型
    put("battle_node", 1),
    {ok, [Scene, CopyId]}.

%% 统一模块+过程调用(cast)
handle_cast({'apply_cast', Module, Method, Args}, State) ->
    case catch apply(Module, Method, Args) of
        {'EXIT', Info} ->
            util:errlog("mod_mon_agent_apply_cast error: Module=~p, Method=~p, Reason=~p", [Module, Method, Info]),
            error;
        DataRet -> DataRet
    end,
    {noreply, State};

%% 关闭进程
handle_cast({'close_scene'}, State) ->
    {stop, normal, State};

%% 默认匹配
handle_cast(_Event, State) ->
    {noreply, State}.

%% 统一模块+过程调用(call)
handle_call({'apply_call', Module, Method, Args}, _From, State) ->
    Reply =
        case catch apply(Module, Method, Args) of
            {'EXIT', Info} ->
                util:errlog("mod_mon_agent_apply_call error: Module=~p, Method=~p, Reason=~p", [Module, Method, Info]),
                error;
            DataRet -> DataRet
        end,
    {reply, Reply, State};

%% 默认匹配
handle_call(_Event, _From, State) ->
    {reply, ok, State}.

%% 默认匹配
handle_info({'close_scene'}, State) ->
    {stop, normal, State};

%% 默认匹配
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_R, State) ->
    %% 清理怪物
    clean(State),
    %% 抹掉进程字段
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ================== 私有函数 =================

%% @doc 清理
clean(State) ->
    clear_scene_mon(),
    [SceneID, CopyID] = State,
    MonProcessName = misc:mon_process_name(SceneID, CopyID),
    case misc:whereis_name(local, MonProcessName) =:= self() of
        true ->
            misc:unregister(local, MonProcessName);
        _ ->
            ignore
    end.

%% 清理怪物
clear_scene_mon() ->
    AllMon = lib_mon_agent:get_scene_mon(),
    [mod_mon_active:stop(Mon#ets_mon.aid) || Mon <- AllMon, is_pid(Mon#ets_mon.aid), is_process_alive(Mon#ets_mon.aid)].

%% 启动场景模块
start_mod_mon_agent(Id, CopyId) ->
    case start_link(Id, CopyId) of
        {ok, NewMonPid} ->
            MonProcessName = misc:mon_process_name(Id, CopyId),
            case misc:whereis_name(local, MonProcessName) of
                Pid when is_pid(Pid) ->
                    Pid;
                _ ->
                    misc:register(local, MonProcessName, NewMonPid),
                    NewMonPid
            end;
        R ->
            util:errlog("mod_mon_agent:id~p - ~p~n", [Id, R]),
            undefined
    end.

%% 动态加载某个怪物管理器 Id : 场景ID
get_scene_mon_pid(Id, CopyId) ->
    MonProcessName = misc:mon_process_name(Id, CopyId),
    case misc:whereis_name(local, MonProcessName) of
        Pid when is_pid(Pid) ->
            case misc:is_process_alive(Pid) of
                true ->
                    MonProcessName = misc:mon_process_name(Id, CopyId),
                    misc:whereis_name(local, MonProcessName);
                false ->
                    misc:unregister(local, MonProcessName),
                    exit(Pid, kill),
                    start_mod_mon_agent(Id, CopyId)
            end;
        _ ->
            start_mod_mon_agent(Id, CopyId)
    end.
