%%%-----------------------------------
%%% @Module  : lib_scene_calc
%%% @Author  : xyao
%%% @Email   : jiexiaowen@gmail.com
%%% @Created : 2011.07.22
%%% @Description: 场景广播算法
%%%-----------------------------------
-module(lib_scene_calc).
-export([
            get_xy/2,
            move_broadcast/10,
            is_area_scene/4,
            get_broadcast_user/3,
            get_broadcast_mon/4,
            get_broadcast_mech/4,
            get_the_area/2,
            move_mon_broadcast/4,
            get_rand_pos/4
        ]).
-include("common.hrl").
-include("scene.hrl").
-define(LENGTH, 20).     %% 九宫格单格长
-define(WIDTH, 15).      %% 九宫格单格宽

-define(MAP_LENGTH, 20). %% 地图每行九宫格数量

-define(MAP_LENGTH_M, 0).                   % 九宫格中间格子
-define(MAP_LENGTH_R, 1).                   % 九宫格右边格子
-define(MAP_LENGTH_L, -1).                  % 九宫格左边格子 
-define(MAP_LENGTH_U, -?MAP_LENGTH).        % 九宫格上边格子
-define(MAP_LENGTH_D, ?MAP_LENGTH).         % 九宫格下边格子
-define(MAP_LENGTH_LU, -(?MAP_LENGTH+1)).   % 九宫格左上边格子
-define(MAP_LENGTH_LD, (?MAP_LENGTH-1)).    % 九宫格左下边格子
-define(MAP_LENGTH_RU, -(?MAP_LENGTH-1)).   % 九宫格右上边格子
-define(MAP_LENGTH_RD, ?MAP_LENGTH+1).      % 九宫格右下边格子

%%  获取要广播的范围用户信息
get_broadcast_user(CopyId, X, Y) ->
    Area = get_the_area(X, Y),
    lib_scene_agent:get_all_area_user(Area, CopyId).

%%  获取要广播的范围怪物信息
get_broadcast_mon(Q, CopyId, X, Y) ->
    Area = get_the_area(X, Y),
    lib_mon:get_all_area_mon(Q, Area, CopyId).

%%  获取要广播的范围机甲信息
get_broadcast_mech(Q, CopyId, X, Y) ->
    Area = get_the_area(X, Y),
    lib_mech:get_all_area_mech(Q, Area, CopyId).

%%是否在9宫格内
is_area_scene(X1, Y1, X2, Y2) ->
    XY2 = get_xy(X2, Y2),
    XY = get_xy(X1, Y1),
    if
        XY == XY2 orelse XY == XY2 + ?MAP_LENGTH_R orelse XY == XY2 + ?MAP_LENGTH_L orelse XY == XY2 + ?MAP_LENGTH_U orelse XY == XY2 + ?MAP_LENGTH_D orelse XY == XY2 + ?MAP_LENGTH_LU orelse XY == XY2 + ?MAP_LENGTH_RD orelse XY == XY2 + ?MAP_LENGTH_RU  orelse XY == XY2 + ?MAP_LENGTH_LD ->
            true;
        true->
            false
    end.

%% 获取九宫格
get_the_area(X, Y) ->
    XY = get_xy(X, Y),
    [XY, XY + ?MAP_LENGTH_R, XY + ?MAP_LENGTH_L, XY + ?MAP_LENGTH_U, XY + ?MAP_LENGTH_D, XY + ?MAP_LENGTH_LU, XY + ?MAP_LENGTH_RD, XY + ?MAP_LENGTH_RU, XY + ?MAP_LENGTH_LD].

%% 随机一个点
get_rand_pos(SceneId, X, Y, Dis) ->
    List = [
                {0, [X + Dis, Y]}, {1, [X - Dis, Y]}, 
                {2, [X, Y + Dis]}, {3, [X, Y - Dis]}, 
                {4, [X + Dis, Y + Dis]}, {5, [X + Dis, Y - Dis]}, 
                {6, [X - Dis, Y - Dis]}, {7, [X - Dis, Y + Dis]}
            ],
    get_rand_pos_helper(SceneId, X, Y, List).

get_rand_pos_helper(_SceneId, X, Y, []) ->
    [X, Y];
get_rand_pos_helper(SceneId, X, Y, List) ->
    Len = length(List),
    Rnd = util:rand(1, Len),
    {Index, [X1, Y1]} = lists:nth(Rnd, List),
    case lib_scene:can_be_moved(SceneId, X1, Y1) of
        true ->
            List1 = lists:keydelete(Index, 1, List),
            get_rand_pos_helper(SceneId, X, Y, List1);
        false ->
            [X1, Y1]
    end.


%%--------------------------九宫格加载场景---------------------------
%% 根据每张地图实际大小，分成一个个10*15的大格子，并从左到右赋予编号。
%% 如把整个地图共有100*150个格子，0，0坐标点为原点，以10*15为一个格子，从左到右编号， 那么坐标50,50所在的格子的编号就是Y div 15 * 10 + X div 10 + 1.

%% 获取当前所在的格子的编号
get_xy(X, Y) ->
    Y div ?WIDTH * ?MAP_LENGTH + X div ?LENGTH + 1.

%%当人物或者怪物移动时候的广播
%%终点要X1，Y1，原点是X2,Y2
%%BinData走路协议包, BinData1移除玩家包 BinData2有玩家进入
move_broadcast(Q, CopyId, X1, Y1, X2, Y2, BinData, BinData1, BinData2, SendList) ->
    XY1 = get_xy(X1, Y1),
    XY2 = get_xy(X2, Y2),
    move_user_broadcast(CopyId, XY1, XY2, BinData, BinData1, BinData2, SendList),

    %% 这里排出怪物移动的调用
    case SendList =/= [] of
        true ->
            mod_mon_agent:apply_cast(Q, CopyId, lib_scene_calc, move_mon_broadcast, [CopyId, XY1, XY2, SendList]);
        false ->
            skip
    end.


%% 广播给玩家
move_user_broadcast(CopyId, XY1, XY2, BinData, BinData1, BinData2, SendList) ->
    [SceneUser1, SceneUser2] = if
        XY2 == XY1 -> %% 同一个格子内移动
            move_loop1([CopyId, XY1, XY2, BinData, BinData1, BinData2]);
        XY2 + ?MAP_LENGTH_R == XY1 -> %% 向右移动
            move_loop2([CopyId, XY1, XY2, BinData, BinData1, BinData2]);
        XY2 + ?MAP_LENGTH_L == XY1 -> %% 向左移动
            move_loop3([CopyId, XY1, XY2, BinData, BinData1, BinData2]);
        XY2 + ?MAP_LENGTH_U == XY1 -> %% 向上移动
            move_loop4([CopyId, XY1, XY2, BinData, BinData1, BinData2]);
        XY2 + ?MAP_LENGTH_D == XY1 -> %% 向下移动
            move_loop5([CopyId, XY1, XY2, BinData, BinData1, BinData2]);
        XY2 + ?MAP_LENGTH_LU == XY1 -> %% 向左上移动
            move_loop6([CopyId, XY1, XY2, BinData, BinData1, BinData2]);
        XY2 + ?MAP_LENGTH_LD == XY1 -> %% 向左下移动
            move_loop7([CopyId, XY1, XY2, BinData, BinData1, BinData2]);
        XY2 + ?MAP_LENGTH_RU == XY1 -> %% 向右上移动
            move_loop8([CopyId, XY1, XY2, BinData, BinData1, BinData2]);
        XY2 + ?MAP_LENGTH_RD == XY1 -> %% 向右下移动
            move_loop9([CopyId, XY1, XY2, BinData, BinData1, BinData2]);
        true -> %% 跨相邻九宫格移动
            move_loop10([CopyId, XY1, XY2, BinData, BinData1, BinData2])
    end,
    case SendList =:= [] orelse (SceneUser1 == [] andalso SceneUser2 == []) of
        true ->
            ok;
        false ->
            %%加入和移除玩家
            {ok, BinData3} = pt_120:write(12011, [SceneUser1, SceneUser2]),
            [Node, Sid] = SendList,
            lib_server_send:send_to_sid(Node, Sid, BinData3)
    end.

move_loop1([CopyId, _XY1, XY2, BinData, _BinData1, _BinData2]) ->
    Area = [XY2, XY2 + ?MAP_LENGTH_R, XY2 + ?MAP_LENGTH_L, XY2 + ?MAP_LENGTH_U, XY2 + ?MAP_LENGTH_D, XY2 + ?MAP_LENGTH_LU, XY2 + ?MAP_LENGTH_RD, XY2 + ?MAP_LENGTH_RU, XY2+ ?MAP_LENGTH_LD],
    lib_scene_agent:send_to_any_area(Area, CopyId, BinData),
    [[], []].

move_loop2([CopyId, XY1, XY2, BinData, BinData1, BinData2]) ->
    Area1 = [XY1 + ?MAP_LENGTH_R, XY1 + ?MAP_LENGTH_RD, XY1 + ?MAP_LENGTH_RU],
    User1 = lib_scene_agent:move_send_and_getuser(Area1, CopyId, BinData2),

    Area2 = [XY2 + ?MAP_LENGTH_L, XY2 + ?MAP_LENGTH_LU, XY2 + ?MAP_LENGTH_LD],
    User2 = lib_scene_agent:move_send_and_getid(Area2, CopyId, BinData1),

    Area3 = [XY2, XY2 + ?MAP_LENGTH_R, XY2 + ?MAP_LENGTH_U,  XY2 + ?MAP_LENGTH_D,  XY2 + ?MAP_LENGTH_RD, XY2 + ?MAP_LENGTH_RU],
    lib_scene_agent:send_to_any_area(Area3, CopyId, BinData),
    [User1, User2].

move_loop3([CopyId, XY1, XY2, BinData, BinData1, BinData2]) ->
    Area1 = [XY1 + ?MAP_LENGTH_L, XY1 + ?MAP_LENGTH_LU, XY1 + ?MAP_LENGTH_LD],
    User1 = lib_scene_agent:move_send_and_getuser(Area1, CopyId, BinData2),

    Area2 = [XY2 + ?MAP_LENGTH_R, XY2 + ?MAP_LENGTH_RD,  XY2 + ?MAP_LENGTH_RU],
    User2 = lib_scene_agent:move_send_and_getid(Area2, CopyId, BinData1),

    Area3 = [XY2, XY2 + ?MAP_LENGTH_LU, XY2 + ?MAP_LENGTH_L, XY2 + ?MAP_LENGTH_U, XY2 + ?MAP_LENGTH_D, XY2+ ?MAP_LENGTH_LD],
    lib_scene_agent:send_to_any_area(Area3, CopyId, BinData),

    [User1, User2].

move_loop4([CopyId, XY1, XY2, BinData, BinData1, BinData2]) ->
    Area1 = [XY1 + ?MAP_LENGTH_U, XY1 + ?MAP_LENGTH_LU, XY1 + ?MAP_LENGTH_RU],
    User1 = lib_scene_agent:move_send_and_getuser(Area1, CopyId, BinData2),

    Area2 = [XY2 + ?MAP_LENGTH_D, XY2 + ?MAP_LENGTH_RD, XY2 + ?MAP_LENGTH_LD],
    User2 = lib_scene_agent:move_send_and_getid(Area2, CopyId, BinData1),

    Area3 = [XY2, XY2 + ?MAP_LENGTH_R, XY2 + ?MAP_LENGTH_L, XY2 + ?MAP_LENGTH_U, XY2 + ?MAP_LENGTH_LU, XY2 + ?MAP_LENGTH_RU],
    lib_scene_agent:send_to_any_area(Area3, CopyId, BinData),

    [User1, User2].

move_loop5([CopyId, XY1, XY2, BinData, BinData1, BinData2]) ->
    Area1 = [XY1 + ?MAP_LENGTH_D, XY1 + ?MAP_LENGTH_RD, XY1 + ?MAP_LENGTH_LD],
    User1 = lib_scene_agent:move_send_and_getuser(Area1, CopyId, BinData2),

    Area2 = [XY2 + ?MAP_LENGTH_U, XY2 + ?MAP_LENGTH_LU, XY2 + ?MAP_LENGTH_RU],
    User2 = lib_scene_agent:move_send_and_getid(Area2, CopyId, BinData1),

    Area3 = [XY2, XY2 + ?MAP_LENGTH_R, XY2 + ?MAP_LENGTH_L, XY2 + ?MAP_LENGTH_D, XY2 + ?MAP_LENGTH_RD, XY2 + ?MAP_LENGTH_LD],
    lib_scene_agent:send_to_any_area(Area3, CopyId, BinData),

    [User1, User2].
    
move_loop6([CopyId, XY1, XY2, BinData, BinData1, BinData2]) ->
    Area1 = [XY1 + ?MAP_LENGTH_L, XY1 + ?MAP_LENGTH_LU, XY1 + ?MAP_LENGTH_U, XY1 + ?MAP_LENGTH_RU, XY1 + ?MAP_LENGTH_LD],
    User1 = lib_scene_agent:move_send_and_getuser(Area1, CopyId, BinData2),

    Area2 = [XY2 + ?MAP_LENGTH_R, XY2 + ?MAP_LENGTH_RD, XY2 + ?MAP_LENGTH_D, XY2 + ?MAP_LENGTH_LD, XY2 + ?MAP_LENGTH_RU],
    User2 = lib_scene_agent:move_send_and_getid(Area2, CopyId, BinData1),

    Area3 = [XY2, XY2 + ?MAP_LENGTH_LU, XY2 + ?MAP_LENGTH_L, XY2 + ?MAP_LENGTH_U],
    lib_scene_agent:send_to_any_area(Area3, CopyId, BinData),

    [User1, User2].

move_loop7([CopyId, XY1, XY2, BinData, BinData1, BinData2]) ->
    Area1 = [XY1 + ?MAP_LENGTH_LU, XY1 + ?MAP_LENGTH_L, XY1 + ?MAP_LENGTH_LD, XY1 + ?MAP_LENGTH_D, XY1 + ?MAP_LENGTH_RD],
    User1 = lib_scene_agent:move_send_and_getuser(Area1, CopyId, BinData2),

    Area2 = [XY2 + ?MAP_LENGTH_RD, XY2 + ?MAP_LENGTH_R, XY2 + ?MAP_LENGTH_RU, XY2 + ?MAP_LENGTH_U, XY2 + ?MAP_LENGTH_LU],
    User2 = lib_scene_agent:move_send_and_getid(Area2, CopyId, BinData1),

    Area3 = [XY2, XY2 + ?MAP_LENGTH_L, XY2 + ?MAP_LENGTH_D,  XY2 + ?MAP_LENGTH_LD],
    lib_scene_agent:send_to_any_area(Area3, CopyId, BinData),

    [User1, User2].

move_loop8([CopyId, XY1, XY2, BinData, BinData1, BinData2]) ->
    Area1 = [XY1 + ?MAP_LENGTH_R, XY1 + ?MAP_LENGTH_RD, XY1 + ?MAP_LENGTH_RU, XY1 + ?MAP_LENGTH_U, XY1 + ?MAP_LENGTH_LU],
    User1 = lib_scene_agent:move_send_and_getuser(Area1, CopyId, BinData2),

    Area2 = [XY2 + ?MAP_LENGTH_L, XY2 + ?MAP_LENGTH_LU, XY2 + ?MAP_LENGTH_LD, XY2 + ?MAP_LENGTH_D, XY2 + ?MAP_LENGTH_RD],
    User2 = lib_scene_agent:move_send_and_getid(Area2, CopyId, BinData1),

    Area3 = [XY2, XY2 + ?MAP_LENGTH_R, XY2 + ?MAP_LENGTH_U, XY2 + ?MAP_LENGTH_RU],
    lib_scene_agent:send_to_any_area(Area3, CopyId, BinData),

    [User1, User2].

move_loop9([CopyId, XY1, XY2, BinData, BinData1, BinData2]) ->
    Area1 = [XY1 + ?MAP_LENGTH_R, XY1 + ?MAP_LENGTH_LD, XY1 + ?MAP_LENGTH_D, XY1 + ?MAP_LENGTH_RD, XY1 + ?MAP_LENGTH_RU],
    User1 = lib_scene_agent:move_send_and_getuser(Area1, CopyId, BinData2),

    Area2 = [XY2 + ?MAP_LENGTH_L, XY2 + ?MAP_LENGTH_RU, XY2 + ?MAP_LENGTH_U, XY2 + ?MAP_LENGTH_LU, XY2 + ?MAP_LENGTH_LD],
    User2 = lib_scene_agent:move_send_and_getid(Area2, CopyId, BinData1),

    Area3 = [XY2, XY2 + ?MAP_LENGTH_R, XY2 + ?MAP_LENGTH_D, XY2 + ?MAP_LENGTH_RD],
    lib_scene_agent:send_to_any_area(Area3, CopyId, BinData),

    [User1, User2].

move_loop10([CopyId, XY1, XY2, BinData, BinData1, BinData2]) ->
    %% 目标位置全部九宫格格子编号
    AllArea1 = [
        XY1 + ?MAP_LENGTH_LU, XY1 + ?MAP_LENGTH_U, XY1 + ?MAP_LENGTH_RU,
        XY1 + ?MAP_LENGTH_L, XY1,  XY1 + ?MAP_LENGTH_R,
        XY1 + ?MAP_LENGTH_LD, XY1 + ?MAP_LENGTH_D, XY1 + ?MAP_LENGTH_RD],
    %% 原始位置全部九宫格格子编号
    AllArea2 = [
        XY2 + ?MAP_LENGTH_LU, XY2 + ?MAP_LENGTH_U, XY2 + ?MAP_LENGTH_RU,
        XY2 + ?MAP_LENGTH_L, XY2, XY2 + ?MAP_LENGTH_R,
        XY2 + ?MAP_LENGTH_LD, XY2 + ?MAP_LENGTH_D, XY2 + ?MAP_LENGTH_RD],

    %%  分别获取应该添加玩家，删除玩家，移动玩家的格子
    F =
        fun(E, {AddUserL, DelUserL, MoveUserL}) ->
            case lists:member(E, AllArea2) of
                true ->
                    {AddUserL, DelUserL -- [E], [E | MoveUserL]};
                false ->
                    {[E | AddUserL], DelUserL -- [E], MoveUserL}
            end
        end,

    {AL, DL, ML} = lists:foldl(F, {[], AllArea2, []}, AllArea1),
    %% 玩家进入
    User1 = lib_scene_agent:move_send_and_getuser(AL, CopyId, BinData2),
   %% 玩家移除
    User2 = lib_scene_agent:move_send_and_getid(DL, CopyId, BinData1),

    lib_scene_agent:send_to_any_area(ML, CopyId, BinData),

    [User1, User2].


%% =====================================
%% =============== 怪物 ================
%% =====================================
%% 注意：这里仅用于mod_mon_agent调用
%% 广播怪物
move_mon_broadcast(CopyId, XY1, XY2, SendList) ->
    [SceneMon1, SceneMon2] = if
        XY2 == XY1 -> %% 同一个格子内移动
            [[], []];
        XY2 + ?MAP_LENGTH_R == XY1 -> %% 向右移动
            move_mon_loop2(CopyId, XY1, XY2);
        XY2 + ?MAP_LENGTH_L == XY1 -> %% 向左移动
            move_mon_loop3(CopyId, XY1, XY2);
        XY2 + ?MAP_LENGTH_U == XY1 -> %% 向上移动
            move_mon_loop4(CopyId, XY1, XY2);
        XY2 + ?MAP_LENGTH_D == XY1 -> %% 向下移动
            move_mon_loop5(CopyId, XY1, XY2);
        XY2 + ?MAP_LENGTH_LU == XY1 -> %% 向左上移动
            move_mon_loop6(CopyId, XY1, XY2);
        XY2 + ?MAP_LENGTH_LD == XY1 -> %% 向左下移动
            move_mon_loop7(CopyId, XY1, XY2);
        XY2 + ?MAP_LENGTH_RU == XY1 -> %% 向右上移动
            move_mon_loop8(CopyId, XY1, XY2);
        XY2 + ?MAP_LENGTH_RD == XY1 -> %% 向右下移动
            move_mon_loop9(CopyId, XY1, XY2);
        true -> %% 跨相邻九宫格移动
            [[], []]
    end,

    case SceneMon1 == [] andalso SceneMon2 == [] of
        true ->
            ok;
        false ->
            %%加入和移除怪物
            {ok, BinData4} = pt_120:write(12094, [SceneMon1, SceneMon2]),
            [Node, Sid] = SendList,
            lib_server_send:send_to_sid(Node, Sid, BinData4)
    end.

move_mon_loop2(CopyId, XY1, XY2) ->
    Area1 = [XY1 + ?MAP_LENGTH_R, XY1 + ?MAP_LENGTH_RD, XY1 + ?MAP_LENGTH_RU],
    Mon1 = lib_mon_agent:get_all_area_mon(Area1, CopyId),

    Area2 = [XY2 + ?MAP_LENGTH_L, XY2 + ?MAP_LENGTH_LU, XY2 + ?MAP_LENGTH_LD],
    Mon2 = lib_mon_agent:get_all_area(Area2, CopyId),

    [Mon1, Mon2].

move_mon_loop3(CopyId, XY1, XY2) ->
    Area1 = [XY1 + ?MAP_LENGTH_L, XY1 + ?MAP_LENGTH_LU, XY1 + ?MAP_LENGTH_LD],
    Mon1 = lib_mon_agent:get_all_area_mon(Area1, CopyId),

    Area2 = [XY2 + ?MAP_LENGTH_R, XY2 + ?MAP_LENGTH_RD,  XY2 + ?MAP_LENGTH_RU],
    Mon2 = lib_mon_agent:get_all_area(Area2, CopyId),

    [Mon1, Mon2].

move_mon_loop4(CopyId, XY1, XY2) ->
    Area1 = [XY1 + ?MAP_LENGTH_U, XY1 + ?MAP_LENGTH_LU, XY1 + ?MAP_LENGTH_RU],
    Mon1 = lib_mon_agent:get_all_area_mon(Area1, CopyId),

    Area2 = [XY2 + ?MAP_LENGTH_D, XY2 + ?MAP_LENGTH_RD, XY2 + ?MAP_LENGTH_LD],
    Mon2 = lib_mon_agent:get_all_area(Area2, CopyId),

    [Mon1, Mon2].

move_mon_loop5(CopyId, XY1, XY2) ->
    Area1 = [XY1 + ?MAP_LENGTH_D, XY1 + ?MAP_LENGTH_RD, XY1 + ?MAP_LENGTH_LD],
    Mon1 = lib_mon_agent:get_all_area_mon(Area1, CopyId),

    Area2 = [XY2 + ?MAP_LENGTH_U, XY2 + ?MAP_LENGTH_LU, XY2 + ?MAP_LENGTH_RU],
    Mon2 = lib_mon_agent:get_all_area(Area2, CopyId),

    [Mon1, Mon2].
    
move_mon_loop6(CopyId, XY1, XY2) ->
    Area1 = [XY1 + ?MAP_LENGTH_L, XY1 + ?MAP_LENGTH_LU, XY1 + ?MAP_LENGTH_U, XY1 + ?MAP_LENGTH_RU, XY1 + ?MAP_LENGTH_LD],
    Mon1 = lib_mon_agent:get_all_area_mon(Area1, CopyId),

    Area2 = [XY2 + ?MAP_LENGTH_R, XY2 + ?MAP_LENGTH_RD, XY2 + ?MAP_LENGTH_D, XY2 + ?MAP_LENGTH_LD, XY2 + ?MAP_LENGTH_RU],
    Mon2 = lib_mon_agent:get_all_area(Area2, CopyId),

    [Mon1, Mon2].

move_mon_loop7(CopyId, XY1, XY2) ->
    Area1 = [XY1 + ?MAP_LENGTH_LU, XY1 + ?MAP_LENGTH_L, XY1 + ?MAP_LENGTH_LD, XY1 + ?MAP_LENGTH_D, XY1 + ?MAP_LENGTH_RD],
    Mon1 = lib_mon_agent:get_all_area_mon(Area1, CopyId),

    Area2 = [XY2 + ?MAP_LENGTH_RD, XY2 + ?MAP_LENGTH_R, XY2 + ?MAP_LENGTH_RU, XY2 + ?MAP_LENGTH_U, XY2 + ?MAP_LENGTH_LU],
    Mon2 = lib_mon_agent:get_all_area(Area2, CopyId),

    [Mon1, Mon2].

move_mon_loop8(CopyId, XY1, XY2) ->
    Area1 = [XY1 + ?MAP_LENGTH_R, XY1 + ?MAP_LENGTH_RD, XY1 + ?MAP_LENGTH_RU, XY1 + ?MAP_LENGTH_U, XY1 + ?MAP_LENGTH_LU],
    Mon1 = lib_mon_agent:get_all_area_mon(Area1, CopyId),

    Area2 = [XY2 + ?MAP_LENGTH_L, XY2 + ?MAP_LENGTH_LU, XY2 + ?MAP_LENGTH_LD, XY2 + ?MAP_LENGTH_D, XY2 + ?MAP_LENGTH_RD],
    Mon2 = lib_mon_agent:get_all_area(Area2, CopyId),

    [Mon1, Mon2].

move_mon_loop9(CopyId, XY1, XY2) ->
    Area1 = [XY1 + ?MAP_LENGTH_R, XY1 + ?MAP_LENGTH_LD, XY1 + ?MAP_LENGTH_D, XY1 + ?MAP_LENGTH_RD, XY1 + ?MAP_LENGTH_RU],
    Mon1 = lib_mon_agent:get_all_area_mon(Area1, CopyId),

    Area2 = [XY2 + ?MAP_LENGTH_L, XY2 + ?MAP_LENGTH_RU, XY2 + ?MAP_LENGTH_U, XY2 + ?MAP_LENGTH_LU, XY2 + ?MAP_LENGTH_LD],
    Mon2 = lib_mon_agent:get_all_area(Area2, CopyId),

    [Mon1, Mon2].

%move_mon_loop10(CopyId, XY1, XY2) -> 
%     Area1 = [XY1, XY1 + ?MAP_LENGTH_LU, XY1 + ?MAP_LENGTH_U, XY1 + ?MAP_LENGTH_RU, XY1 + ?MAP_LENGTH_L, XY1 + ?MAP_LENGTH_R, XY1 + ?MAP_LENGTH_LD, XY1 + ?MAP_LENGTH_D, XY1 + ?MAP_LENGTH_RD],
%    Mon1 = lib_mon_agent:get_all_area_mon(Area1, CopyId),
%
%    Area2 = [XY2, XY2 + ?MAP_LENGTH_LU, XY2 + ?MAP_LENGTH_U, XY2 + ?MAP_LENGTH_RU, XY2 + ?MAP_LENGTH_L, XY2 + ?MAP_LENGTH_R, XY2 + ?MAP_LENGTH_LD, XY2 + ?MAP_LENGTH_D, XY2 + ?MAP_LENGTH_RD],
%    Mon2 = lib_mon_agent:get_all_area(Area2, CopyId),
%
%    [Mon1, Mon2].

