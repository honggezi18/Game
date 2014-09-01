%%%-----------------------------------
%%% @Module  : lib_mon_agent
%%% @Author  : xyao
%%% @Email   : jiexiaowen@gmail.com
%%% @Created : 2012.05.17
%%% @Description: 怪物场景管理器
%%%-----------------------------------
-module(lib_mon_agent).
-export([
            get_scene_mon/0,              %% 获取场景所有怪物
            get_scene_mon/1,              %% 获取场景的所有怪物
            get_scene_mon_num/1,          %% 获取场景的所有怪物数量
            get_scene_mon_num_by_kind/2,  %% 获得当前场景某种类型怪物数量
            get_scene_mon_by_mid/1,       %% 获取所有怪物（根据怪物mid）
			get_scene_mon_by_mid/2,       %% 获取怪物（根据怪物mid）
			get_mon/1,                    %% 获取怪物（根据怪物id）
            get_scene_mon_aid/0,          %% 获取场景所有怪物的进程id
            get_scene_mon_aid_by_id/1,    %% 获取指定id的怪物进程
            get_scene_mon_aid/1,          %% 获取场景所有怪物的进程id（根据副本id）
            get_scene_mon_aid/2,          %% 获取场景指定怪物的进程id（根据副本id）
            get_scene_mon_id_aid/1,       %% 获取场景所有怪物id和进程id	
            get_area_mon_id_aid_mid/5,    %% 获取场景所有怪物id,进程id,资源id
            get_mon_for_battle/5,         %% 获取战斗所需信息
            get_line_mon_for_battle/9,    %% 获取直线的怪物信息
            get_area/2,                   %% 获取格子怪物id
            get_all_area/2,               %% 获取九宫格子怪物id
            get_area_mon/2,               %% 获取格子怪物信息
            get_all_area_mon/2,           %% 获取九宫格子怪物信息
            get_ai/4,                     %% 获取怪物ai
			put_mon/1,                    %% 保存怪物数据
			save_to_area/4,               %% 添加在九宫格
			del_mon/1,                    %% 保存怪物数据
			del_ai/2,                     %% 删除怪物ai			
			del_to_area/4,                %% 删除在九宫格
            del_all_area/1,               %% 删除9宫格数据
            change_mon_attr/2,            %% 更新怪物属性
            get_att_target_info_by_id/1   %% 怪物追踪目标时获取目标信息
        ]).

-include("scene.hrl").

%% 获取场景的所有怪物（不分房间）
get_scene_mon() ->
    AllMon = get(),
    [ Mon || {Key, Mon} <- AllMon, is_integer(Key)].

%% 获取场景的所有怪物（按房间）
get_scene_mon(CopyId) ->
    AllMon = get_id(CopyId),
    get_scene_mon_helper(AllMon, CopyId, []).

%% 获取场景的所有怪物数量（按房间）
get_scene_mon_num(CopyId) ->
    length(get_id(CopyId)).

get_scene_mon_num_by_kind(CopyId, Kind)->
    AllMon = get_scene_mon(CopyId),
    length([0||Mon <- AllMon, Mon#ets_mon.kind == Kind]).

get_scene_mon_helper([], _, Data) ->
    Data;
get_scene_mon_helper([Id | T], CopyId, Data) ->
    case get(Id) of
         undefined ->
             del_id(CopyId, Id),
             get_scene_mon_helper(T, CopyId, Data);
         Mon ->
             get_scene_mon_helper(T, CopyId, [Mon | Data])
     end. 

%% 根据mid获取场景所有怪物信息（根据mid）（不分房间）
get_scene_mon_by_mid(Mid) ->
    AllMon = get(),
    [ Mon || {Key, Mon} <- AllMon, is_integer(Key), Mon#ets_mon.mid =:= Mid].

%% 根据mid获取场景怪物信息（根据mid）（按房间）
get_scene_mon_by_mid(CopyId, Mid) ->
    AllMon = get_scene_mon(CopyId),
    [ Mon || Mon <- AllMon, Mon#ets_mon.mid =:= Mid].	

%% 获取怪物数据
get_mon(Id) ->
     case get(Id) of
         undefined ->
             [];
         Mon ->
             Mon
     end.

%% 获取场景所有怪物的进程id
get_scene_mon_aid() ->
    AllMon = get(),
    [ Mon#ets_mon.aid || {Key, Mon} <- AllMon, is_integer(Key)].

%% 获取场景所有怪物的进程id
get_scene_mon_aid(CopyId) ->
    AllMon = get_scene_mon(CopyId),
    [ Mon#ets_mon.aid || Mon <- AllMon].

%% 获取场景指定房间指定怪物的进程id
%% Mids = lists() = 怪物资源id列表
get_scene_mon_aid(CopyId, Mids) ->
    AllMon = get_scene_mon(CopyId),
    F = fun(M) ->
            case lists:member(M#ets_mon.mid, Mids) of
                true -> [M#ets_mon.aid];
                false -> []
            end
    end,
    lists:flatmap(F, AllMon).

%% 获取指定id的怪物进程
get_scene_mon_aid_by_id(Id) ->
    case get_mon(Id) of
        [] ->
            0;
        Mon ->
            Mon#ets_mon.aid
    end.

%% 获取场景所有怪物id和进程id
get_scene_mon_id_aid(CopyId) ->
    AllMon = get_scene_mon(CopyId),
    [[Mon#ets_mon.id, Mon#ets_mon.aid] || Mon <- AllMon].

%% 获取场景区域内所有怪物id,进程id,资源id
get_area_mon_id_aid_mid(CopyId, X, Y, Area, Group) ->
    AllArea = lib_scene_calc:get_the_area(X, Y),
    AllMon = get_all_area_mon(AllArea, CopyId),
    X1 = X + Area,
    X2 = X - Area,
    Y1 = Y + Area,
    Y2 = Y - Area,
    [ [Mon#ets_mon.id, Mon#ets_mon.aid, Mon#ets_mon.mid] || Mon <- AllMon, Mon#ets_mon.x >= X2 andalso Mon#ets_mon.x =< X1, Mon#ets_mon.y >= Y2 andalso Mon#ets_mon.y =< Y1 , Mon#ets_mon.hp > 0, Mon#ets_mon.group /= Group].

%% 获取战斗所需信息
get_mon_for_battle(CopyId, X, Y, Area, Group) ->
    AllArea = lib_scene_calc:get_the_area(X, Y),
    AllMon = get_all_area_mon(AllArea, CopyId),
    X1 = X + Area,
    X2 = X - Area,
    Y1 = Y + Area,
    Y2 = Y - Area,
    [Mon || Mon <- AllMon, Mon#ets_mon.x >= X2 andalso Mon#ets_mon.x =< X1, Mon#ets_mon.y >= Y2 andalso Mon#ets_mon.y =< Y1 , Mon#ets_mon.hp > 0, (Mon#ets_mon.group /= Group orelse Group == 0), Mon#ets_mon.mid /= 40306]. %% 40306:70级boss召唤的火坑

get_line_mon_for_battle(CopyId, OX, OY, X, Y, Area, K, B, Group) -> 
    AllArea = lib_scene_calc:get_the_area(X, Y),
    AllMon = get_all_area_mon(AllArea, CopyId),
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
                OX < X andalso OY > Y -> UX >= X2 andalso UX =< X  andalso UY >= Y  andalso UY =< Y1; %% 第三象限
                OX > X andalso OY > Y -> UX >= X  andalso UX =< X1 andalso UY >= Y  andalso UY =< Y1; %% 第四象限
                OX == X andalso OY > Y -> UY >= Y andalso UY =< Y1;
                OX == X andalso OY < Y -> UY =< Y andalso UY >= Y2;
                OY == Y andalso OX > X -> UX >= X andalso UX =< X1;
                OY == Y andalso OX < X -> UX =< X andalso UX >= X2;
                true -> false
            end,
            TrueOrFalse1 andalso TrueOrFalse2
    end,
    [ Mon || Mon <- AllMon,Mon#ets_mon.hp > 0,  (Mon#ets_mon.group /= Group orelse Group == 0), Mon#ets_mon.mid /= 40306, F(Mon#ets_mon.x, Mon#ets_mon.y)].

%% 获取格子怪物id
get_area(XY, CopyId) ->
    case get(?TABLE_AREA(XY, CopyId)) of
        undefined ->
            [];
        D ->
            dict:fetch_keys(D)
    end.

%% 获取g宫格子怪物id
get_all_area(Area, CopyId) ->
    lists:foldl(
        fun(A, L) -> 
                get_area(A, CopyId) ++ L 
        end, 
    [], Area).

%% 获取格子怪物信息
get_area_mon(XY, CopyId) ->
    AllMon = get_area(XY, CopyId),
    get_scene_mon_helper(AllMon, CopyId, []).

%% 获取九宫格子怪物信息
get_all_area_mon(Area, CopyId) ->
    List = lists:foldl(
        fun(A, L) -> 
                get_area(A, CopyId) ++ L 
        end, 
    [], Area),
    get_scene_mon_helper(List, CopyId, []).

%% 获取怪物ai
get_ai(SceneId, CopyId, X, Y) -> 
	%% 查询是否触发AI
	case get({SceneId, CopyId, pos}) of
		undefined -> 
			[];
		D_pos -> 
			case dict:find({X, Y}, D_pos) of
				{ok, _Value} -> 
					case get({SceneId, CopyId, mon}) of 
						undefined ->
							[];
						AidList -> 
							AidList
					end;
				_ ->
					[]
			end
	end.

%% 保存怪物数据
put_mon(Mon) ->
    case get(Mon#ets_mon.id) of
        undefined ->
            save_id(Mon#ets_mon.copy_id, Mon#ets_mon.id),
            save_to_area(Mon#ets_mon.copy_id, Mon#ets_mon.x, Mon#ets_mon.y, Mon#ets_mon.id);
        _Mon ->
            XY1 = lib_scene_calc:get_xy(Mon#ets_mon.x, Mon#ets_mon.y),
            XY2 = lib_scene_calc:get_xy(_Mon#ets_mon.x, _Mon#ets_mon.y),
            if 
                XY1 =:= XY2 ->
                    skip;
                true ->
                    del_to_area(_Mon#ets_mon.copy_id, XY2, _Mon#ets_mon.id),
                    save_to_area(Mon#ets_mon.copy_id, XY1, Mon#ets_mon.id)
            end
    end,
    put(Mon#ets_mon.id, Mon).

%% 添加在九宫格
save_to_area(CopyId, XY, Id) ->
    case get(?TABLE_AREA(XY, CopyId)) of
        undefined ->
            D1 = dict:new(),
            put(?TABLE_AREA(XY, CopyId), dict:store(Id, 0, D1));
        D2 ->
            put(?TABLE_AREA(XY, CopyId), dict:store(Id, 0, D2))
    end.

save_to_area(CopyId, X, Y, Id) ->
    XY = lib_scene_calc:get_xy(X, Y),
    case get(?TABLE_AREA(XY, CopyId)) of
        undefined ->
            D1 = dict:new(),
            put(?TABLE_AREA(XY, CopyId), dict:store(Id, 0, D1));
        D2 ->
            put(?TABLE_AREA(XY, CopyId), dict:store(Id, 0, D2))
    end.

%% 删除怪物数据
del_mon(Id) ->
    case get(Id) of
        undefined ->
            [];
        Mon ->
            del_id(Mon#ets_mon.copy_id, Id),
            del_to_area(Mon#ets_mon.copy_id, Mon#ets_mon.x, Mon#ets_mon.y, Id),
            mod_scene_agent:apply_cast(Mon#ets_mon.scene, erlang, erase, [["1MOD_BATTLE_STATE", Id]]),
            erase(Id)
    end.

%% 删除怪物ai
del_ai(SceneId, CopyId) -> 
	%% 删除触发位置
    erase({SceneId, CopyId, pos}),
	%% 删除触发怪物
	erase({SceneId, CopyId, mon}),
	%% 修改场景进程对应AI状态
	mod_scene_agent:apply_cast(SceneId, lib_scene_agent, set_ai_state, [CopyId, false]).

del_to_area(CopyId, XY, Id) ->
    case get(?TABLE_AREA(XY, CopyId)) of
        undefined ->
            skip;
        D ->
            put(?TABLE_AREA(XY, CopyId), dict:erase(Id, D))
    end.

del_to_area(CopyId, X, Y, Id) ->
    XY = lib_scene_calc:get_xy(X, Y),
    case get(?TABLE_AREA(XY, CopyId)) of
        undefined ->
            skip;
        D ->
            put(?TABLE_AREA(XY, CopyId), dict:erase(Id, D))
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

%% 改变怪物属性
%% Id: 怪物唯一id
%% AtrrList: 怪物属性 [Tuple ...]
%%           Tuple = {group, Value} | {hp, Value} | {hp_lim, V} | {def, V}
%%                   | {skill, SkillList}, SkillList = [{技能id, 概率}...] 
%%                   | {att_area, V}
change_mon_attr(Id, AtrrList) ->
    case get_mon(Id) of
        []  -> skip;
        Mon -> Mon#ets_mon.aid ! {'change_attr', AtrrList}
    end.


%% 保存id
save_id(CopyId, Id) ->
    case get({id, CopyId}) of
        undefined ->
            D1 = dict:new(),
            put({id, CopyId}, dict:store(Id, 0, D1));
        D2 ->
            put({id, CopyId}, dict:store(Id, 0, D2))
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
del_id(CopyId, Id) ->
    case get({id, CopyId}) of
        undefined ->
            skip;
        D ->
            put({id, CopyId}, dict:erase(Id, D))
    end.

%% 怪物追踪目标时获取目标信息
get_att_target_info_by_id([MonAid, Key, AttType, _MonGroup]) ->
    ok.
%%     AttInfo = case get_mon(Key) of
%%         Mon when is_record(Mon, ets_mon)->
%%             X0 = Mon#ets_mon.x,
%%             Y0 = Mon#ets_mon.y,
%%             Hp0 = Mon#ets_mon.hp,
%%             {true, X0, Y0, Hp0, Mon};
%%         _ ->
%%             false
%%     end,
%%     mod_mon_active:trace_info_back(MonAid, AttType, AttInfo).
