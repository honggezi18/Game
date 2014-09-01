%%%------------------------------------
%%% @Module  : mod_scene_agent_cast
%%% @Author  : huangcha
%%% @Created : 2012.05.18
%%% @Description: 场景管理cast处理
%%%------------------------------------
-module(mod_scene_agent_cast).

-export([handle_cast/2]).

-include("scene.hrl").
-include("common.hrl").
-include("pt120_pb.hrl").

%% 移动
handle_cast({'move', [CopyId, X, Y, F, D1, D2, X2, Y2, Key, Mp, Skill]} , State) ->
    case lib_scene_agent:get_user(Key) of
        [] ->
            {noreply, State};
        SceneUser ->
            %广播场景：普通场景、多人副本
            case State#ets_scene.dun_type =:= 0 of
                true ->
                    Id = SceneUser#ets_scene_user.id,
                    {ok, BinData} = pt:pack(12001, #pt12001_toc{x = X
                                                                ,y = Y
                                                                ,id = Id
                                                                ,platform = SceneUser#ets_scene_user.platform
                                                                ,server_id = SceneUser#ets_scene_user.server_num
                                                                ,fly = F
                                                                ,d1 = D1
                                                                ,d2 = D2
                                                                ,skill = Skill
                                                            }),
                    case lib_scene:is_broadcast_scene(State#ets_scene.id) of
                        true ->
                            lib_scene_agent:send_to_local_scene(SceneUser#ets_scene_user.copy_id, BinData);
                        false ->
                            % 移除
                            {ok, BinData1} = pt:pack(12004, #pt12004_toc{id=Id, platform=SceneUser#ets_scene_user.platform, server_id=SceneUser#ets_scene_user.server_num}),
                            % 有玩家进入
                            Me = pt:player_record(SceneUser),
                            {ok, BinData2} = pt:pack(12003, #pt12003_toc{player=Me}),
                            lib_scene_calc:move_broadcast(State#ets_scene.id, CopyId, X, Y, X2, Y2, BinData, BinData1, BinData2, [SceneUser#ets_scene_user.node, SceneUser#ets_scene_user.sid])
                    end;
                _ -> skip
            end,

            %% 速度buff检查
            lib_scene_agent:put_user(SceneUser#ets_scene_user{x=X, y=Y, mp = Mp}),

            {noreply, State}
    end;

%% 玩家加入场景
handle_cast({'join', SceneUser} , State) ->
    %% 移除旧场景玩家
    #ets_scene_user{id = RoleID, platform = Platform, server_num = ServerNum} = SceneUser,
    Key = [RoleID, Platform, ServerNum],
    case lib_scene_agent:get_user(Key) of
        [] ->
            ignore;
        OldSceneUser ->
            #ets_scene_user{copy_id = OldCopyID, x = OldX, y = OldY} = OldSceneUser,
            {ok, LeaveBin} = pt:pack(#pt12004_toc{id=RoleID,platform=Platform,server_id=ServerNum}),
            IsBroadScene = lib_scene:is_broadcast_scene(State#ets_scene.id),
            if
                IsBroadScene ->
                    lib_scene_agent:send_to_local_scene(OldCopyID, LeaveBin);
                true ->
                    lib_scene_agent:send_to_local_area_scene(OldCopyID, OldX, OldY, LeaveBin)
            end,
            lib_scene_agent:del_user(Key)
    end,

    %% 通知所有玩家你进入了
    {ok, BinData} = pt:pack(#pt12003_toc{player=pt:player_record(SceneUser)}),
    case lib_scene:is_broadcast_scene(State#ets_scene.id) of
        true ->
            lib_scene_agent:send_to_local_scene(SceneUser#ets_scene_user.copy_id, BinData);
        false ->
            lib_scene_agent:send_to_local_area_scene(SceneUser#ets_scene_user.copy_id, SceneUser#ets_scene_user.x, SceneUser#ets_scene_user.y, BinData)
    end,

    lib_scene_agent:put_user(SceneUser),

    {noreply, State};


%% @doc 玩家离开场景
handle_cast({'leave', CopyId, Key, X, Y} , State) ->
    case lib_scene_agent:get_user(Key) of
        [] ->
            {noreply, State};
        SceneUser ->
            Id = SceneUser#ets_scene_user.id,
            {ok, BinData} = pt:pack(#pt12004_toc{id=Id,platform=SceneUser#ets_scene_user.platform, server_id=SceneUser#ets_scene_user.server_num}),
            IsBroadScene = lib_scene:is_broadcast_scene(State#ets_scene.id),
            if
                IsBroadScene ->
                    lib_scene_agent:del_user(Key),
                    lib_scene_agent:send_to_local_scene(CopyId, BinData);
                true ->
                    lib_scene_agent:del_user(Key),
                    lib_scene_agent:send_to_local_area_scene(CopyId, X, Y, BinData)
            end,
            {noreply, State}
    end;

%% 更战斗属性信息
handle_cast({'update', {battle_attr, Key, Hp, HpLim, Mp, MpLim, BA}} , State) ->
    case lib_scene_agent:get_user(Key) of
        [] ->
            {noreply, State};
        SceneUser ->
            lib_scene_agent:put_user(SceneUser#ets_scene_user{
                    hp = Hp,
                    hp_lim = HpLim,
                    mp = Mp,
                    mp_lim = MpLim,
                    battle_attr=BA
                }),
            {noreply, State}
    end;

%% 更新气血和内力
handle_cast({'update', {hp_mp, Key, {Hp, Mp}}} , State) ->
    case lib_scene_agent:get_user(Key) of
        [] ->
            {noreply, State};
        SceneUser ->
            lib_scene_agent:put_user(SceneUser#ets_scene_user{hp=Hp, mp=Mp}),
            {noreply, State}
    end;

%% 玩家头像
handle_cast({'update', {change_image, Key, {ImageId}}}, State) ->
    case lib_scene_agent:get_user(Key) of
        [] ->
            {noreply, State};
        SceneUser ->
            lib_scene_agent:put_user(SceneUser#ets_scene_user{image = ImageId}),
            {noreply, State}
    end;

%% 坐标
handle_cast({'update', {xy, Key, {X, Y}}}, State) ->
    case lib_scene_agent:get_user(Key) of
        [] ->
            {noreply, State};
        SceneUser ->
            lib_scene_agent:put_user(SceneUser#ets_scene_user{x = X, y = Y}),
            {noreply, State}
    end;

%% 速度
handle_cast({'update', {speed, Key, {Speed}}}, State) ->
    case lib_scene_agent:get_user(Key) of
        [] ->
            {noreply, State};
        SceneUser ->
            lib_scene_agent:put_user(SceneUser#ets_scene_user{speed = Speed}),
            {noreply, State}
    end;

%% 给场景所有玩家发送信息
handle_cast({'send_to_scene', CopyId, Bin} , State) ->
    lib_scene_agent:send_to_local_scene(CopyId, Bin),
    {noreply, State};

%% 给场景所有玩家发送信息
handle_cast({'send_to_scene', Bin} , State) ->
    lib_scene_agent:send_to_local_scene(Bin),
    {noreply, State};

%% 给场景九宫格玩家发送信息
handle_cast({'send_to_area_scene', CopyId, X, Y, Bin} , State) ->
    lib_scene_agent:send_to_local_area_scene(CopyId, X, Y, Bin),
    {noreply, State};

%% 给场景指定平台&服玩家发送消息
handle_cast({'chat_to_uid', [Cmd, NodeSelf, SidSelf], [Id, Platform, ServerID], Bin} , State) ->
    lib_scene_agent:chat_to_uid([Cmd, NodeSelf, SidSelf], [Id, Platform, ServerID], Bin),
    {noreply, State};

%% 加载场景
handle_cast({'load_scene', SceneUser}, State) ->
    %%通知所有玩家你进入了
    handle_cast({'join', SceneUser}, State),
    handle_cast({'send_scene_info_to_uid', [SceneUser#ets_scene_user.id, SceneUser#ets_scene_user.platform, SceneUser#ets_scene_user.server_num], SceneUser#ets_scene_user.copy_id, SceneUser#ets_scene_user.x, SceneUser#ets_scene_user.y}, State),
    {noreply, State};

%% 把场景信息发送给玩家
handle_cast({'send_scene_info_to_uid', Key, CopyId, X, Y}, State) ->
    case lib_scene_agent:get_user(Key) of
        [] ->
            {noreply, State};
        User ->
            [SceneUser, SceneMon] =
            case lib_scene:is_broadcast_scene(State#ets_scene.id) orelse CopyId =/= 0  of
                true ->
                    TmpSceneUser = lib_scene_agent:get_scene_user(CopyId),
                    TmpSceneMon = lib_mon:get_scene_mon(State#ets_scene.id, CopyId),
                    [TmpSceneUser, TmpSceneMon];
                false ->
                    TmpSceneUser = lib_scene_calc:get_broadcast_user(CopyId, X, Y),
                    TmpSceneMon = [],
                    [TmpSceneUser, TmpSceneMon]
            end,
            %%当前元素信息
            _SceneElem = State#ets_scene.elem,
            %%当前npc信息
            _SceneNpc = lib_npc:get_scene_npc(State#ets_scene.id),
            {ok, BinData} = pt:pack(#pt12002_toc{players=[pt:player_record(P)||P<-SceneUser], mons=[pt:mon_record(M)||M<-SceneMon]}),
            lib_server_send:send_to_sid(User#ets_scene_user.node, User#ets_scene_user.sid, BinData),
            {noreply, State}
    end;

%% 关闭场景
handle_cast({'close_scene'} , State) ->
%%     catch mod_mon_create:clear_scene_all_mon(State#ets_scene.id, 0, 0),
    {stop, normal, State};

%% 清理场景
handle_cast({'clear_scene'} , State) ->
%%     catch mod_mon_create:clear_scene_all_mon(State#ets_scene.id, 0, 0),
    lib_scene_agent:clear_all_process_dict(),
    {noreply, State};

%% 统一模块+过程调用(cast)
handle_cast({'apply_cast', Module, Method, Args} , State) ->
    ?TRY_CATCH(apply(Module, Method, Args)),
    {noreply, State};

%% 默认匹配
handle_cast(Event, Status) ->
    catch util:errlog("mod_scene_agent_cast:handle_cast not match: ~p", [Event]),
    {noreply, Status}.
