%%%-----------------------------------
%%% @Module  : lib_scene
%%% @Author  : xyao
%%% @Email   : jiexiaowen@gmail.com
%%% @Created : 2010.05.08
%%% @Description: 场景信息
%%%-----------------------------------
-module(lib_scene).
-export([
    change_scene/5,
    change_scene_queue/6,
    player_change_scene_queue/6,
    change_scene_4_revive/6,
    change_speed/9,
    leave_scene/1,
    enter_scene/1,
    del_all_area/2,
    send_scene_info_to_uid/5,
    revive_to_scene/2,
    get_data/1,
    is_blocked/3,
    can_be_moved/3,
    check_enter/2,
    enter_normal_scene/3,
    get_scene_user/1,
    get_scene_user/2,
    get_scene_user_field/3,
    get_scene_mon/2,
    get_scene_mon_num/2,
    get_scene_mon_num_by_kind/3,
    is_dungeon_scene/1,
    refresh_npc_ico/1,
    get_scene_user_by_id/2,
    player_change_scene/6,
    save_scene_user_by_id/1,
    get_born_xy/1,
    repair_xy/4,
    is_broadcast_scene/1
]).

-include("server.hrl").
-include("scene.hrl").
-include("pt120_pb.hrl").

%% 游戏内更改玩家场景信息
%%@param Id 玩家ID
%%@param SceneId 目标场景ID
%%@param CopyId 房间号ID 为0时，普通房间，非0值切换房间。值相同，在同一个房间。
%%@param X 目标场景出生点X
%%@param Y 目标场景出生点Y
%%@param NeedOut 是否需要特殊处理场景   true|false（不需要）
player_change_scene(Id, SceneId, CopyId, X, Y, NeedOut) ->
    case misc:get_player_process(Id) of
        Pid when is_pid(Pid) ->
            gen_server:cast(Pid, {change_scene, SceneId, CopyId, X, Y, NeedOut});
        _ ->
            void
    end.

%%切换场景方法
%%@param PlayerStatus 玩家当前状态
%%@param SceneId 目标场景ID
%%@param CopyId 房间号ID
%%@param X 目标坐标X
%%@param Y 目标坐标Y
%%@param NeedOut 是否需要特殊处理场景   true|false（不需要）
%%@return 新玩家状态
change_scene(PlayerStatus, SceneId, CopyId, X, Y)->
    % 通知离开场景
    lib_scene:leave_scene(PlayerStatus),
            
    %%加载场景
    S = lib_scene:get_data(SceneId),
    {ok, Bin} = pt:pack(12005, #pt12005_toc{id = SceneId
                                            ,x = X
                                            ,y = Y
                                            ,name = S#ets_scene.name
                                            ,scene_id=S#ets_scene.id
                                        }),
    lib_server_send:send_to_sid(PlayerStatus#player_status.sid, Bin),

    NewPlayerStatus = PlayerStatus#player_status{scene = SceneId, copy_id = CopyId, x = X, y = Y},

    %% 死亡时被传送则自动复活
    NewPlayerStatus1 = case NewPlayerStatus#player_status.hp =< 0 of
        true ->
            NewPlayerStatus#player_status{hp = 100};
        false ->
            NewPlayerStatus
    end,
    NewPlayerStatus1.

%%切换场景方法--复活专用
%%@param PlayerStatus 玩家当前状态
%%@param SceneId 目标场景ID
%%@param CopyId 房间号ID
%%@param X 目标坐标X
%%@param Y 目标坐标Y
%%@param Revive_type (1原地 2回城, 4复活点复活)
%%@return 新玩家状态
change_scene_4_revive(PlayerStatus, SceneId, CopyId, X, Y, ReviveType)->
    %%判断是否要离开场景
    if
        ReviveType =:= 1 orelse ReviveType =:= 3 ->
            {ok, DataBin_12003} = pt:pack(12003, #pt12003_toc{player=pt:player_record(PlayerStatus)}),
            lib_server_send:send_to_scene(SceneId, CopyId, DataBin_12003);
        true ->
            % 通知离开场景
            lib_scene:leave_scene(PlayerStatus)
    end,

    %%加载场景
    S = lib_scene:get_data(SceneId),
    Att_protected = 0,
    {ok, Bin} = pt_120:write(12083, [
            ReviveType,                                     
            SceneId,
            X,
            Y,
            S#ets_scene.name,
            PlayerStatus#player_status.hp,
            PlayerStatus#player_status.mp,
            PlayerStatus#player_status.gold,
            PlayerStatus#player_status.bgold,
            Att_protected,
            PlayerStatus#player_status.speed
        ]
    ),

    NewPlayerStatus = PlayerStatus#player_status{scene = SceneId, copy_id = CopyId, x = X, y = Y},
    lib_server_send:send_to_sid(NewPlayerStatus#player_status.sid, Bin),

    NewPlayerStatus.

%% 游戏内更改玩家场景信息(公共线调用)
%%@param Id 玩家ID
%%@param SceneId 目标场景ID
%%@param CopyId 房间号ID 为0时，普通房间，非0值切换房间。值相同，在同一个房间。
%%@param X 目标场景出生点X
%%@param Y 目标场景出生点Y
%%@param Value lists() 用于在换场景后的数据处理,换线后会执行mod_server_cast:set_data_sub(Value, Status)
%%             | 0     不做处理
player_change_scene_queue(Id, SceneId, CopyId, X, Y, Value) ->
    case misc:get_player_process(Id) of
        Pid when is_pid(Pid) ->
            catch gen_server:cast(Pid, {'change_scene_sign', [SceneId, CopyId, X, Y, Value]});
        _ ->
            void
    end.

%%排队-切换场景方法
%%@param PlayerStatus 玩家当前状态
%%@param SceneId 目标场景ID
%%@param CopyId 房间号ID
%%@param X 目标坐标X
%%@param Y 目标坐标Y
%%@param Value 自定义结构,用于在换场景后的数据处理
%%@return  
change_scene_queue(PlayerStatus, SceneId, CopyId, X, Y, Value) -> 
    catch gen_server:cast(PlayerStatus#player_status.pid, {'change_scene_sign', [SceneId, CopyId, X, Y, Value]}).

%%改变速度
change_speed(Id, PlayerId, Platform, Scene, CopyId, _X, _Y, Speed, State)->
    {ok, BinData} = pt_120:write(12082, [State, Id, PlayerId, Platform, Speed]),
    lib_server_send:send_to_scene(Scene, CopyId, BinData).

%%离开当前场景
%%Sid:场景ID
%%player_status记录
leave_scene(Status) ->
    mod_scene_agent:leave(Status).

%%进入当前场景
%%player_status记录
enter_scene(Status) ->
    mod_scene_agent:join(Status).

%%给用户发送场景信息 - 12002内容
send_scene_info_to_uid(Key, Sid, CopyId, X, Y) ->
    mod_scene_agent:send_scene_info_to_uid(Key, Sid, CopyId, X, Y).

%% 删除怪物9宫格
del_all_area(SceneId, CopyId) ->
    mod_scene_agent:apply_cast(SceneId, lib_scene_agent, del_all_area, [CopyId]).

%% 复活进入场景
%% status复活前的状态，status1复活后的状态
revive_to_scene(Status1, Status2) ->
    %%告诉原来场景玩家你已经离开
    leave_scene(Status1),
    %%告诉复活点的玩家你进入场景进
    enter_scene(Status2),
    send_scene_info_to_uid([Status2#player_status.id, Status2#player_status.platform, Status2#player_status.server_num], Status2#player_status.scene, 
                           Status2#player_status.copy_id, Status2#player_status.x, 
                           Status2#player_status.y).

%% 获取场景信息，唯一id，区分是不是副本
get_data(_Id) ->
    [].
%%     data_scene:get(Id).


%% 进入场景条件检查
check_enter(Status, Id) ->
    %判断场景是否可以传送.
    case get_data(Id) of
        [] ->
            {false, data_scene_text:get_sys_msg(1)};
        Scene ->
            case check_requirement(Status, Scene#ets_scene.requirement) of
                {false, Reason} ->
                    {false, Reason};
                {true} ->
                    case Scene#ets_scene.type of
                        %0.普通场景.
                        ?SCENE_TYPE_NORMAL ->
                            enter_normal_scene(Id, Scene, Status);
                        _ ->
                            enter_normal_scene(Id, Scene, Status)
                end
           end
    end.

%% 逐个检查进入需求
check_requirement(_, []) ->
    {true};
check_requirement(Status, [{K, V} | T]) ->
    case K of
        lv -> %% 等级需求
            case Status#player_status.lv < V of
                true ->
                    Msg = data_scene_text:get_sys_msg(2)++integer_to_list(V)++data_scene_text:get_sys_msg(3),
                    {false, list_to_binary(Msg)};
                false ->
                    check_requirement(Status, T)
            end;
        _ ->
            check_requirement(Status, T)
    end.

%%进入普通场景
enter_normal_scene(SceneId, Scene, Status) ->
    [X, Y] = [Scene#ets_scene.x, Scene#ets_scene.y],
    {true, SceneId, X, Y, Scene#ets_scene.name, Scene#ets_scene.id, Status}.

%%获得当前场景用户
%%Q:场景ID
%%CopyId:副本id
get_scene_user(Q) ->
    mod_scene_agent:apply_call(Q, lib_scene_agent, get_scene_user, []).

%%获得当前场景用户
%%Q:场景ID
%%CopyId:副本id
get_scene_user(Q, CopyId) ->
    mod_scene_agent:apply_call(Q, lib_scene_agent, get_scene_user, [CopyId]).

%%获得当前场景用户字段
%%Q:场景ID
%%CopyId:副本id
%%Type:字段类型
%%return:[Pid1, Pid2]
get_scene_user_field(Q, CopyId, Type) ->
    mod_scene_agent:apply_call(Q, lib_scene_agent, get_scene_user_field, [Type, CopyId]).

%%获得当前场景信息
%%Q:场景ID
%%CopyId:副本id
get_scene_mon(Q, CopyId) ->
    mod_mon_agent:apply_call(Q, CopyId, lib_mon_agent, get_scene_mon, [CopyId]).

%%获得当前场景怪物数量
%%Q:场景ID
%%CopyId:副本id
get_scene_mon_num(Q, CopyId) ->
    mod_mon_agent:apply_call(Q, CopyId, lib_mon_agent, get_scene_mon_num, [CopyId]).

%%获得当前场景某种类型怪物数量
%%Q:场景ID
%%CopyId:副本id
get_scene_mon_num_by_kind(Q, CopyId, Kind) ->
    mod_mon_agent:apply_call(Q, CopyId, lib_mon_agent, get_scene_mon_num_by_kind, [CopyId, Kind]).

%% 是否为副本场景，唯一id，会检查是否存在这个场景
is_dungeon_scene(_Id) ->
    ok.

%% 判断在场景SID的[X,Y]坐标是否有障碍物
%% return:ture有障碍物,false无障碍物
is_blocked(Sid, X, Y) when X > 0 andalso Y > 0 ->
    Module = list_to_atom(lists:concat(["data_mask_", Sid])),
    case Module:get(Y) of
        [] ->
            true;
        MaskLine ->
            case catch lists:nth(X+1, MaskLine) of
                49 -> true;
                _ -> false
            end
    end;
is_blocked(_, _, _) ->
    true.

%% 刷新npc任务状态
refresh_npc_ico(Rid) when is_integer(Rid)->
    case lib_player:get_player_info(Rid) of
        Status when is_record(Status, player_status)->
            lib_scene:refresh_npc_ico(Status);
        R ->
            util:errlog("ps is error:~p~n", [R]),
            ok
    end;
        
refresh_npc_ico(_Status) ->
    ok.
%%     NpcList = lib_npc:get_scene_npc(Status#player_status.scene),
%%     F = fun(Npc) ->
%%             Id = case is_integer(Npc) of
%%                 true ->
%%                     Npc;
%%                 _ ->
%%                     Npc#ets_npc.id
%%             end,
%%             [Id, lib_task:get_npc_state(Id, Status)]
%%     end,
%%     L = lists:map(F, NpcList),
%%     {ok, BinData} = pt_120:write(12020, [L]),
%%     lib_server_send:send_to_sid(Status#player_status.sid, BinData).


%% 获取当前场景指定id用户信息
get_scene_user_by_id(Scene, Key) ->
    mod_scene_agent:apply_call(Scene, lib_scene_agent, get_user, [Key]).

%% 保存当前场景指定id用户信息
save_scene_user_by_id(SceneUser) ->
    mod_scene_agent:apply_cast(SceneUser#ets_scene_user.scene, lib_scene_agent, put_user, [SceneUser]).

%% 获取当前场景出生点
get_born_xy(_Sid) ->
    ok.

%% 修复坐标
repair_xy(_Lv, _Scene, _X, _Y) ->
    ok.

%% 是否需要场景广播
is_broadcast_scene(_Id) ->
   true.

%% 是否可以移动到(X,Y)坐标
%% Sid 场景id
%% Retrue: true 有异常，不能移动 | false 没异常，可以移动
can_be_moved(Sid, X, Y) -> 
    case is_blocked(Sid, X, Y) of %% 是否障碍物
        true -> true;
        false ->
            SceneWidth = 1000,
            Height = 1000,
            X =< 0 orelse Y =< 0 orelse SceneWidth div ?PX_LEN =< X orelse Height div ?PY_LEN =< Y %% 是否场景外
    end.

