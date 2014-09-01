%%%-----------------------------------
%%% @Module  : lib_scene_agent
%%% @Author  : xyao
%%% @Email   : jiexiaowen@gmail.com
%%% @Created : 2012.05.17
%%% @Description: 场景管理器
%%%-----------------------------------
-module(lib_scene_agent).
-export([
            get_scene_user/0,
            get_scene_user/1,
            get_scene_user_field/2,
            get_scene_user_for_battle/6,
			get_scene_user_for_battle/3,
            get_line_user_for_battle/10,
            get_scene_user_for_assist/8,
            get_scene_user_pid_area/4,
            get_scene_user_id_pid_area/4,
            send_to_local_area_scene/4,
            send_to_local_scene/2,
            send_to_local_scene/1,
            clear_all_process_dict/0,
            put_user/1,
            get_user/1,
            del_user/1,
            save_to_area/4,
            del_to_area/4,
            del_all_area/1,
            get_area/2,
            send_to_any_area/3,
            move_send_and_getuser/3,
            move_send_and_getid/3,
            get_all_area_user/2,
            get_att_target_info_by_id/1,
			chat_to_uid/3,
			set_ai_state/2,
            update_hp/2
        ]).
-include("scene.hrl").

%% 获取场景所有玩家数据
get_scene_user() ->
    AllUser = get(),
    [ User || {_Key, User} <- AllUser, is_record(User, ets_scene_user)].

%% 获取场景玩家数据
get_scene_user(CopyId) ->
    AllUser = get_id(CopyId),
    get_scene_user_helper(AllUser, CopyId, []).

get_scene_user_helper([], _, Data) ->
    Data;
get_scene_user_helper([Key | T], CopyId, Data) ->
    case get(Key) of
         undefined ->
             del_id(CopyId, Key),
             get_scene_user_helper(T, CopyId, Data);
         User ->
             get_scene_user_helper(T, CopyId, [User | Data])
     end. 

%% 获取场景所有玩家某个字段数据
%%CopyId:副本id
%%Type:字段类型
get_scene_user_field(pid, CopyId) ->
    AllUser = get_scene_user(CopyId),
    [User#ets_scene_user.pid || User <- AllUser];
get_scene_user_field(id, CopyId) ->
    AllUser = get_scene_user(CopyId),
    [ User#ets_scene_user.id || User <- AllUser];
get_scene_user_field(lv, CopyId) ->
    AllUser = get_scene_user(CopyId),
    [ User#ets_scene_user.lv || User <- AllUser];
get_scene_user_field(_, _) ->
    [].

get_scene_user_for_battle(CopyId, X, Y, Area, Id1, Group) ->
    AllArea = lib_scene_calc:get_the_area(X, Y),
    AllUser = get_all_area_user(AllArea, CopyId),
    X1 = X + Area,
    X2 = X - Area,
    Y1 = Y + Area,
    Y2 = Y - Area,
    [User || User <-AllUser, User#ets_scene_user.x >= X2 andalso User#ets_scene_user.x =< X1, User#ets_scene_user.y >= Y2 andalso User#ets_scene_user.y =< Y1, User#ets_scene_user.id /= Id1, User#ets_scene_user.hp > 0, (Group == 0 orelse User#ets_scene_user.group /= Group)].

get_scene_user_for_battle(CopyId, Id1, Group) -> 
	AllUser = get_scene_user(CopyId),
	[User || User <-AllUser, User#ets_scene_user.id /= Id1, User#ets_scene_user.hp > 0, (Group == 0 orelse User#ets_scene_user.group /= Group)].

%% 直线攻击目标
%% OX,OY 目标点坐标
%% X,Y   攻击者坐标
get_line_user_for_battle(CopyId, OX, OY, X, Y, Area, K, B, Id1, Group) -> 
     AllArea = lib_scene_calc:get_the_area(X, Y),
    AllUser = get_all_area_user(AllArea, CopyId),
    X1 = X + Area,
    X2 = X - Area,
    Y1 = Y + Area,
    Y2 = Y - Area,
    F = fun(UX, UY) ->
            TrueOrFalse1 = if
                OX == X -> UX == X orelse UX == X-1 orelse UX == X+1;
                true -> (round( UX * K + B) == UY 
                        orelse round(UX * K + B + 1) == UY
                        orelse round(UX * K + B - 1) == UY)
            end,
            TrueOrFalse2 = if
                OX > X andalso OY < Y -> UX >= X  andalso UX =< X1 andalso UY >= Y2 andalso UY =< Y;  %% 第一象限
                OX < X andalso OY < Y -> UX >= X2 andalso UX =< X  andalso UY >= Y2 andalso UY =< Y;  %% 第二象限
                OX < X andalso OY > Y -> UX >= X2 andalso UX =< X  andalso UY >= Y  andalso UY < Y1; %% 第三象限
                OX > X andalso OY > Y -> UX >= X  andalso UX =< X1 andalso UY >= Y  andalso UY < Y1; %% 第四象限
                OX == X andalso OY > Y -> UY >= Y andalso UY =< Y1;
                OX == X andalso OY < Y -> UY =< Y andalso UY >= Y2;
                OY == Y andalso OX > X -> UX >= X andalso UX =< X1;
                OY == Y andalso OX < X -> UX =< X andalso UX >= X2;
                true -> false
            end,
            TrueOrFalse1 andalso TrueOrFalse2
    end,
    [ User || User <-AllUser,  
        User#ets_scene_user.id /= Id1, 
        User#ets_scene_user.hp > 0, 
        (Group == 0 orelse User#ets_scene_user.group /= Group), F(User#ets_scene_user.x,  User#ets_scene_user.y)].

get_scene_user_for_assist(CopyId, X, Y, Area, TeamId, _KfTeamId, Group, Sign) ->
    AllArea = lib_scene_calc:get_the_area(X, Y),
    AllUser = get_all_area_user(AllArea, CopyId),
    X1 = X + Area,
    X2 = X - Area,
    Y1 = Y + Area,
    Y2 = Y - Area,
    [User || User <-AllUser, User#ets_scene_user.hp > 0, (Group == 0 orelse User#ets_scene_user.group == Group), User#ets_scene_user.x >= X2 andalso User#ets_scene_user.x =< X1, User#ets_scene_user.y >= Y2 andalso User#ets_scene_user.y =< Y1].

%% 获取一定范围内玩家的pid
get_scene_user_pid_area(CopyId, X, Y, Area) ->
    AllArea = lib_scene_calc:get_the_area(X, Y),
    AllUser = get_all_area_user(AllArea, CopyId),
    X1 = X + Area,
    X2 = X - Area,
    Y1 = Y + Area,
    Y2 = Y - Area,
    [ User#ets_scene_user.pid || User <-AllUser, User#ets_scene_user.x >= X2 andalso User#ets_scene_user.x =< X1, User#ets_scene_user.y >= Y2 andalso User#ets_scene_user.y =< Y1].

get_scene_user_id_pid_area(CopyId, X, Y, Area) ->
    AllArea = lib_scene_calc:get_the_area(X, Y),
    AllUser = get_all_area_user(AllArea, CopyId),
    X1 = X + Area,
    X2 = X - Area,
    Y1 = Y + Area,
    Y2 = Y - Area,
    [ [[User#ets_scene_user.id, User#ets_scene_user.platform, User#ets_scene_user.server_num], User#ets_scene_user.pid] || User <-AllUser, User#ets_scene_user.hp > 0, User#ets_scene_user.x >= X2 andalso User#ets_scene_user.x =< X1, User#ets_scene_user.y >= Y2 andalso User#ets_scene_user.y =< Y1].

%% 给场景玩家发信息
send_to_local_scene(CopyId, Bin) ->
    AllUser = get_id(CopyId),
    F = fun(Key) ->
        case get(Key) of
            undefined ->
                del_id(CopyId, Key);
            User ->
                send_to_sid(User, Bin)
        end
    end,
    [F(Key) || Key <- AllUser].

%% 给场景玩家发信息
send_to_local_scene(Bin) ->
    AllUser = get_scene_user(),
    [begin
         send_to_sid(User, Bin)
     end || User <- AllUser],
    ok.

%% 给区域玩家发信息
send_to_local_area_scene(CopyId, X, Y, Bin) ->
    Area = lib_scene_calc:get_the_area(X, Y),
    send_to_any_area(Area, CopyId, Bin).

%% 给场景指定平台&服玩家发送消息 
chat_to_uid([Cmd, NodeSelf, SidSelf], [Id, Platform, ServerID], Bin) ->
	lib_chat:kf_chat_to_uid([Cmd, NodeSelf, SidSelf], [Id, Platform, ServerID], Bin).	
%% 清理场景中所有进程字典数据
clear_all_process_dict() -> 
    AllPDict = get(),
    F = fun({Key, _}) -> 
            case Key of
                Key when is_list(Key) -> erase(Key); %%玩家数据
                {id, _} ->  erase(Key); 
                {_, _, _} ->  erase(Key);
                _ -> skip
            end
    end,
    [F(E)||E<-AllPDict],
    ok.

%% 保存玩家数据
put_user(SceneUser) ->
    Key = [SceneUser#ets_scene_user.id, SceneUser#ets_scene_user.platform, SceneUser#ets_scene_user.server_num],
    case get(Key) of
        undefined ->
            save_id(SceneUser#ets_scene_user.copy_id, Key),
            save_to_area(SceneUser#ets_scene_user.copy_id, SceneUser#ets_scene_user.x, SceneUser#ets_scene_user.y, Key);
        User ->
            XY1 = lib_scene_calc:get_xy(SceneUser#ets_scene_user.x, SceneUser#ets_scene_user.y),
            XY2 = lib_scene_calc:get_xy(User#ets_scene_user.x, User#ets_scene_user.y),
            if 
                XY1 =:= XY2 ->
                    skip;
                true ->
                    del_to_area(User#ets_scene_user.copy_id, XY2, Key),
                    save_to_area(SceneUser#ets_scene_user.copy_id, XY1, Key)
            end
    end,
    put(Key, SceneUser).

%% 获取玩家数据
get_user(Key) ->
     case get(Key) of
         undefined ->
             [];
         User ->
             User
     end.

%% 删除玩家数据
del_user(Key) ->
    case get(Key) of
         undefined ->
             [];
         User ->
             del_id(User#ets_scene_user.copy_id, Key),
             del_to_area(User#ets_scene_user.copy_id, User#ets_scene_user.x, User#ets_scene_user.y, Key),
             erase(Key)
     end.

%% 删除在九宫格
del_to_area(CopyId, XY, Key) ->
    case get(?TABLE_AREA(XY, CopyId)) of
        undefined ->
            skip;
        D ->
            put(?TABLE_AREA(XY, CopyId), dict:erase(Key, D))
    end.

del_to_area(CopyId, X, Y, Key) ->
    XY = lib_scene_calc:get_xy(X, Y),
    case get(?TABLE_AREA(XY, CopyId)) of
        undefined ->
            skip;
        D ->
            put(?TABLE_AREA(XY, CopyId), dict:erase(Key, D))
    end.

%% 删除9宫格数据
del_all_area(CopyId) ->
    Data = get(),
    F = fun({Key, _}) ->
        case Key of
            {_, _, Cid} when Cid =:= CopyId ->
                erase(Key);
            {_, Cid} when Cid =:= CopyId ->
                erase(Key);
            _ ->
                skip
        end
    end,
    lists:foreach(F, Data).

%% 添加在九宫格
save_to_area(CopyId, XY, Key) ->
    case get(?TABLE_AREA(XY, CopyId)) of
        undefined ->
            D1 = dict:new(),
            put(?TABLE_AREA(XY, CopyId), dict:store(Key, 0, D1));
        D2 ->
            put(?TABLE_AREA(XY, CopyId), dict:store(Key, 0, D2))
    end.

save_to_area(CopyId, X, Y, Key) ->
    XY = lib_scene_calc:get_xy(X, Y),
    case get(?TABLE_AREA(XY, CopyId)) of
        undefined ->
            D1 = dict:new(),
            put(?TABLE_AREA(XY, CopyId), dict:store(Key, 0, D1));
        D2 ->
            put(?TABLE_AREA(XY, CopyId), dict:store(Key, 0, D2))
    end.

%% 获取9宫格玩家
get_area(XY, CopyId) ->
    case get(?TABLE_AREA(XY, CopyId)) of
        undefined ->
            [];
        D ->
            dict:fetch_keys(D)
    end.

%% 保存id
save_id(CopyId, Key) ->
    case get({id, CopyId}) of
        undefined ->
            D1 = dict:new(),
            put({id, CopyId}, dict:store(Key, 0, D1));
        D2 ->
            put({id, CopyId}, dict:store(Key, 0, D2))
    end.

%% 获取id
get_id(CopyId) ->
    case get({id, CopyId}) of
        undefined ->
            [];
        D ->
            dict:fetch_keys(D)
    end.

%% 删除id
del_id(CopyId, Key) ->
    case get({id, CopyId}) of
        undefined ->
            skip;
        D ->
            put({id, CopyId}, dict:erase(Key, D))
    end.

%% 发送消息给部分区域
send_to_any_area(_Area, _CopyId, <<>>) -> [];
send_to_any_area(Area, CopyId, Bin) ->
    F1 = fun(Key, A) ->
            case get(Key) of
                undefined ->
                    del_id(CopyId, Key),
                    del_to_area(CopyId, A, Key);
                User ->
                    send_to_sid(User, Bin)
            end
    end,
    F2 = fun(A) ->
            List = get_area(A, CopyId),
            [F1(Key, A) || Key <- List]
    end,
    [ F2(A) || A <- Area].

%% 移动9宫格广播用
move_send_and_getuser(Area, CopyId, Bin) ->
    lists:foldl(
        fun(A, L) -> 
                List = get_area(A, CopyId),
                move_send_and_getuser_loop(List, {CopyId, A}, Bin, []) ++ L 
        end, 
    [], Area).

move_send_and_getuser_loop([], _, _,Data) ->
    Data;
move_send_and_getuser_loop([Key | T], {CopyId, A}, Bin, Data) ->
    case get(Key) of
        undefined ->
            del_id(CopyId, Key),
            del_to_area(CopyId, A, Key),
            move_send_and_getuser_loop(T, {CopyId, A}, Bin, Data);
        User ->
            send_to_sid(User, Bin),
            move_send_and_getuser_loop(T, {CopyId, A}, Bin, Data ++ [User]) 
    end.

%% 移动9宫格广播用
move_send_and_getid(Area, CopyId, Bin) ->
    F = fun(Key, A) ->
            case get(Key) of
                undefined ->
                    del_id(CopyId, Key),
                    del_to_area(CopyId, A, Key),
                    [0, "", 0];
                User ->
                    send_to_sid(User, Bin),
                    [User#ets_scene_user.id, User#ets_scene_user.platform, User#ets_scene_user.server_num]
            end
    end,
    lists:foldl(
        fun(A, L) -> 
                List = get_area(A, CopyId),
                List1 = [F(Key, A) || Key <- List],
                List1 ++ L 
        end, 
    [], Area).

%% 获取g宫格子玩家信息
get_all_area_user(Area, CopyId) ->
    List = lists:foldl(
        fun(A, L) -> 
                get_area(A, CopyId) ++ L 
        end, 
    [], Area),
    get_scene_user_helper(List, CopyId, []).

%% 怪物追踪目标时获取目标信息
get_att_target_info_by_id([MonAid, Key, AttType, MonGroup]) ->
    AttInfo = case get_user(Key) of
        Player when is_record(Player, ets_scene_user)-> 
            case MonGroup == 0 orelse MonGroup /= Player#ets_scene_user.group of
                true -> 
                    X0 = Player#ets_scene_user.x,
                    Y0 = Player#ets_scene_user.y,
                    Hp0 = Player#ets_scene_user.hp,
                    {true, X0, Y0, Hp0, Player};
                false -> false
            end;
        _ ->
            false
    end,
    mod_mon_active:trace_info_back(MonAid, AttType, AttInfo).

%% 设置AI信息(场景进程运行)
%% State ==> true | false
set_ai_state(CopyId, State) -> 
	case State of
		true ->
			put({CopyId, ai_state}, true);
		false ->
			erase({CopyId, ai_state})
	end.

update_hp(Key, Hp) ->
    case get(Key) of
        undefined ->
           [];
        User ->
           put(Key, User#ets_scene_user{hp = Hp})
    end.

%% @doc 发送协议给场景玩家
send_to_sid(User, Bin) ->
    lib_server_send:send_to_sid(User#ets_scene_user.node, User#ets_scene_user.sid, Bin).
