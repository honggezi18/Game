%%%------------------------------------
%%% @Module  : mod_scene_agent
%%% @Author  : huangcha
%%% @Email   : jiexiaowen@gmail.com
%%% @Created : 2011.07.15
%%% @Description: 场景管理 
%%%------------------------------------
-module(mod_scene_agent). 

-behaviour(gen_server).

-export([
    start_link/1,
    get_scene_pid/1,
    send_to_scene/3,
    send_to_scene/2,
    send_to_area_scene/5,
    join/1,
    leave/1,
    move/7,
    update/2,
    start_mod_scene_agent/1,
    get_scene_num/1,
    send_scene_info_to_uid/5,
    close_scene/1,
    clear_scene/1,
    apply_call/4,
    apply_cast/4,
    do_battle/2,
    update_to_clusters/2,
    load_scene/1,
    chat_to_uid/4,
    trans_join_data/1
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("common.hrl").
-include("server.hrl").
-include("scene.hrl").

%% 通过场景调用函数 - call
apply_call(Sid, Module, Method, Args) ->
    case get_scene_pid(Sid) of
        undefined ->
            skip;
        clusters -> %% 发到跨服中心去
            mod_clusters_node:apply_call(?MODULE, apply_call, [Sid, Module, Method, Args]);
        Pid ->
            case catch gen:call(Pid, '$gen_call', {'apply_call', Module, Method, Args}) of
                {ok, Res} ->
                    Res;
                Reason ->
                    erase({get_scene_pid, Sid}),
                    util:errlog("ERROR mod_scene_agent apply_call/4 sid: ~p function: ~p Reason : ~p~n", [Sid, [Module, Method, Args], Reason]),
                    skip
            end
    end.

%% 通过场景调用函数 - cast
apply_cast(Sid, Module, Method, Args) ->
    case get_scene_pid(Sid) of
        undefined ->
            skip;
        clusters -> %% 发到跨服中心去
            mod_clusters_node:apply_cast(?MODULE, apply_cast, [Sid, Module, Method, Args]);
        Pid ->
            gen_server:cast(Pid, {'apply_cast', Module, Method, Args})
    end.

%% 给场景所有玩家发送信息
send_to_scene(Sid, CopyId, Bin) ->
    case get_scene_pid(Sid) of
        undefined ->
            skip;
        clusters -> %% 发到跨服中心去
            mod_clusters_node:apply_cast(?MODULE, send_to_scene, [Sid, CopyId, Bin]);
        Pid ->
            gen_server:cast(Pid, {'send_to_scene', CopyId, Bin})
    end.

%% 给场景所有玩家发送信息
send_to_scene(Sid, Bin) ->
    case get_scene_pid(Sid) of
        undefined ->
            skip;
        clusters -> %% 发到跨服中心去
            mod_clusters_node:apply_cast(?MODULE, send_to_scene, [Sid, Bin]);
        Pid ->
            gen_server:cast(Pid, {'send_to_scene', Bin})
    end.


%% 给场景九宫格玩家发送信息
send_to_area_scene(Sid, CopyId, X, Y, Bin) ->
    case get_scene_pid(Sid) of
        undefined ->
            skip;
        clusters -> %% 发到跨服中心去
            mod_clusters_node:apply_cast(?MODULE, send_to_area_scene, [Sid, CopyId, X, Y, Bin]);
        Pid ->
            gen_server:cast(Pid, {'send_to_area_scene', CopyId, X, Y, Bin})
    end.

%% 给场景指定平台&服玩家发送消息
chat_to_uid(Sid, [Cmd, NodeSelf, SidSelf], [Id, Platform, ServerID], Bin) ->
	case get_scene_pid(Sid) of
		undefined ->
			skip;
		clusters -> %% 发到跨服中心去
			mod_clusters_node:apply_cast(?MODULE, chat_to_uid, [Sid, [Cmd, NodeSelf, SidSelf], [Id, Platform, ServerID], Bin]);
		Pid ->
			gen_server:cast(Pid, {'chat_to_uid', [Cmd, NodeSelf, SidSelf], [Id, Platform, ServerID], Bin})
	end.

%% 玩家加入场景
trans_join_data(PS) ->
    Node = mod_disperse:get_clusters_node(),
    BA = [],
    #ets_scene_user{
        id = PS#player_status.id,
        nickname = PS#player_status.nickname,
        sex = PS#player_status.sex,
        scene = PS#player_status.scene,
        copy_id = PS#player_status.copy_id,
        lv = PS#player_status.lv,
        sid = PS#player_status.sid,
        pid = PS#player_status.pid,
        x = PS#player_status.x,
        y = PS#player_status.y,
        hp = PS#player_status.hp,
        hp_lim = PS#player_status.hp_lim,
        mp = PS#player_status.mp,
        mp_lim = PS#player_status.mp_lim,
        career = PS#player_status.career,
        realm = PS#player_status.realm,
        speed = PS#player_status.speed,
        battle_attr = BA,
        image = PS#player_status.image,
        node = Node,
        platform = PS#player_status.platform,
        server_num = PS#player_status.server_num
    }.

join(PS) ->
    EtsSceneUser = trans_join_data(PS),
    case get_scene_pid(EtsSceneUser#ets_scene_user.scene) of
        undefined ->
            skip;
        clusters -> %% 发到跨服中心去
            mod_clusters_node:apply_cast(?MODULE, update_to_clusters, [EtsSceneUser#ets_scene_user.scene, {'join', EtsSceneUser}]);
        Pid ->
            gen_server:cast(Pid, {'join', EtsSceneUser})
    end.
    

%% 12002加载场景
load_scene(PS) ->
    EtsSceneUser = trans_join_data(PS),
    case get_scene_pid(EtsSceneUser#ets_scene_user.scene) of
        undefined ->
            skip;
        clusters -> %% 发到跨服中心去
            mod_clusters_node:apply_cast(?MODULE, update_to_clusters, [EtsSceneUser#ets_scene_user.scene, {'load_scene', EtsSceneUser}]);
        Pid ->
            gen_server:cast(Pid, {'load_scene', EtsSceneUser})
    end.

do_battle(Scene, Data) ->
    case get_scene_pid(Scene) of
        undefined ->
            skip;
        clusters -> %% 发到跨服中心去
            mod_clusters_node:apply_cast(?MODULE, do_battle, [Scene, Data]);
        Pid ->
            gen_server:cast(Pid, {'update', Data})
    end.

%% 更新场景管理器玩家信息
update(_, _) ->
    ok.

update_to_clusters(Scene, Args) ->
    gen_server:cast(get_scene_pid(Scene), Args).


%% 玩家离开场景
leave(PS) ->
    case get_scene_pid(PS#player_status.scene) of
        undefined ->
            skip;
        clusters -> %% 发到跨服中心去
            mod_clusters_node:apply_cast(?MODULE, update_to_clusters, [PS#player_status.scene, {'leave', PS#player_status.copy_id, [PS#player_status.id, PS#player_status.platform, PS#player_status.server_num], PS#player_status.x, PS#player_status.y}]);
        Pid ->
            gen_server:cast(Pid, {'leave', PS#player_status.copy_id, [PS#player_status.id, PS#player_status.platform, PS#player_status.server_num], PS#player_status.x, PS#player_status.y})
    end.


%%移动
move(X, Y, F, D1, D2, PS, Skill) ->
    case get_scene_pid(PS#player_status.scene) of
        undefined ->
            skip;
        clusters -> %% 发到跨服中心去
            mod_clusters_node:apply_cast(?MODULE, update_to_clusters, [PS#player_status.scene, {'move', [PS#player_status.copy_id, X, Y, F, PS#player_status.x, PS#player_status.y, [PS#player_status.id, PS#player_status.platform, PS#player_status.server_num], PS#player_status.mp, Skill]}]);
        Pid ->
            gen_server:cast(Pid,{'move', [PS#player_status.copy_id, X, Y, F,  D1, D2, PS#player_status.x, PS#player_status.y, [PS#player_status.id, PS#player_status.platform, PS#player_status.server_num], PS#player_status.mp, Skill]})
    end.

%%获取指定场景人数
get_scene_num(Sid) ->
    case get_scene_pid(Sid) of
        undefined ->
            skip;
        clusters -> %% 发到跨服中心去
            mod_clusters_node:apply_call(?MODULE, get_scene_num, [Sid]);
        Pid ->
            gen_server:call(Pid, {'scene_num'})
    end.

%%给指定用户发送场景信息
send_scene_info_to_uid(Key, Sid, CopyId, X, Y) ->
    case get_scene_pid(Sid) of
        undefined ->
            skip;
        clusters -> %% 发到跨服中心去
            mod_clusters_node:apply_cast(?MODULE, send_scene_info_to_uid, [Key, Sid, CopyId, X, Y]);
        Pid ->
            gen_server:cast(Pid, {'send_scene_info_to_uid', Key, CopyId, X, Y})
    end.

%%关闭指定的场景
close_scene(Sid) ->
    case get_scene_pid(Sid) of
        undefined ->
            skip;
        clusters -> %% 发到跨服中心去
            mod_clusters_node:apply_cast(?MODULE, close_scene, [Sid]),
            erase({get_scene_pid, Sid});
        Pid ->
            gen_server:cast(Pid, {'close_scene'}),
            erase({get_scene_pid, Sid})
    end.


%% 清理所有场景数据
clear_scene(Sid) ->
    case get_scene_pid(Sid) of
        undefined ->
            skip;
        clusters -> %% 发到跨服中心去
            mod_clusters_node:apply_cast(?MODULE, clear_scene, [Sid]);
        Pid ->
            gen_server:cast(Pid, {'clear_scene'})
    end.

start_link([Scene]) ->
    gen_server:start(?MODULE, [?SCENE_AGENT_NUM, Scene], []);
start_link([Num, Scene]) ->
    gen_server:start(?MODULE, [Num, Scene], []).

%% Num:场景个数
%% WorkerId:进行标示
%% Scene:场景相关内容
init([Num, Scene]) ->
    process_flag(trap_exit, true),
    SceneProcessName = misc:scene_process_name(Scene#ets_scene.id),
    set_process_pid(SceneProcessName),
    mod_scene:add_node_scene(Scene#ets_scene.id),
    Sid = list_to_tuple(lists:map(
            fun(_)->
                    spawn_link(fun()->do_msg() end)
            end,lists:seq(1, Num))
    ),
    State= Scene#ets_scene{worker = Sid},
    {ok, State}.

handle_cast(R , State) ->
    case catch mod_scene_agent_cast:handle_cast(R, State) of
        {noreply, NewState} ->
            {noreply, NewState};
        {stop, Normal, NewState} ->
            {stop, Normal, NewState};
        Reason ->
            util:errlog("mod_scene_agent_cast error: ~p, Reason:=~p~n",[R, Reason]),
            {noreply, State}
    end.

handle_call(R, From, State) ->
    case catch mod_scene_agent_call:handle_call(R , From, State) of
        {reply, NewFrom, NewState} ->
            {reply, NewFrom, NewState};
        Reason ->
             util:errlog("mod_scene_agent_call error: ~p, Reason=~p~n",[R, Reason]),
             {reply, error, State}
    end.

handle_info(R, State) ->
    case catch mod_scene_agent_info:handle_info(R, State) of
        {noreply, NewState} ->
            {noreply, NewState};
        Reason ->
            util:errlog("mod_scene_agent_info error: ~p, Reason:=~p~n",[R, Reason]),
            {noreply, State}
    end.

terminate(R, State) ->
    mod_scene:del_node_scene(State#ets_scene.id),
    del_process_pid(misc:scene_process_name(State#ets_scene.id)),
    catch util:errlog("mod_scene_agent is terminate, id is ~p, res:~p", [State#ets_scene.id, R]),
    {ok, State}.

code_change(_OldVsn, State, _Extra)->
    {ok, State}.

%% ================== 私有函数 =================

%%启动场景模块 (加锁保证全局唯一)
start_mod_scene_agent(Id) ->
    SceneProcessName = misc:scene_process_name(Id),
    set_process_lock(SceneProcessName),
	ScenePid = start_scene(Id),
    del_process_lock(SceneProcessName),
	ScenePid.

%% 启动场景模块
start_scene(Id) ->
    case data_scene:get(Id) of
        [] ->
            undefined;
        Scene ->
            {ok, NewScenePid} = start_link([Scene]),
            NewScenePid
    end.

%% 动态加载某个场景 Lv : 场景等级
get_scene_pid(Id) ->
    case get({get_scene_pid, Id}) of
        undefined ->
            case data_scene:get(Id) of
                [] ->
                    undefined;
                Scene ->
                    IsCreate = case Scene#ets_scene.type of
                        ?SCENE_TYPE_CLUSTERS -> %% 跨服场景
                            case config:get_cls_type() of
                                1 ->
                                    true;
                                _ ->
                                    false
                            end;
                        _ ->
                            true
                    end,
                    case IsCreate of
                        true ->
                            SceneProcessName = misc:scene_process_name(Id),
                            ScenePid = case get_process_pid(SceneProcessName) of
                                Pid when is_pid(Pid) ->
                                    case misc:is_process_alive(Pid) of
                                        true ->
                                            Pid;
                                        false ->
                                            del_process_pid(SceneProcessName),
                                            exit(Pid, kill),
                                            mod_scene_init:start_new_scene(Id)
                                    end;
                                _ ->
                                    mod_scene_init:start_new_scene(Id)
                            end,
                            case is_pid(ScenePid) of
                                true ->
                                    put({get_scene_pid, Id}, ScenePid),
                                    ScenePid;
                                false ->
                                    undefined
                            end;
                        false ->  %% 需要发送到跨服中心去
                            put({get_scene_pid, Id}, clusters),
                            clusters
                    end
            end;
        ScenePid ->
            ScenePid
    end.

%% 处理消息
do_msg() ->
    receive
        {apply, Module, Method, Args} ->
            apply(Module, Method, Args);
        _ ->
            do_msg()
    end.

%% 获取进程pid
get_process_pid(ProcessName) ->
    case config:get_cls_type() of
        1 ->
            %% 跨服中心启动
            misc:whereis_name(local, ProcessName);
        _ ->
            %% 跨服节点启动
            misc:whereis_name(global, ProcessName)
    end.

%% 获取进程pid
set_process_pid(ProcessName) ->
    case config:get_cls_type() of
        1 ->
            %% 跨服中心启动
            misc:register(local, ProcessName, self());
        _ ->
            %% 跨服节点启动
            misc:register(global, ProcessName, self())
    end.

del_process_pid(ProcessName) ->
    case config:get_cls_type() of
        1 ->
            %% 跨服中心启动
            misc:unregister(local, ProcessName);
        _ ->
            %% 跨服节点启动
            misc:unregister(global, ProcessName)
    end.

set_process_lock(ProcessName) ->
    case config:get_cls_type() of
        1 ->
            %% 跨服中心启动
            skip;
        _ ->
            %% 跨服节点启动
            global:set_lock({ProcessName, undefined})
    end.

del_process_lock(ProcessName) ->
    case config:get_cls_type() of
        1 ->
            %% 跨服中心启动
            skip;
        _ ->
            %% 跨服节点启动
            global:del_lock({ProcessName, undefined})
    end.
