%%%-----------------------------------
%%% @Module  : lib_mon
%%% @Author  : zhenghehe
%%% @Created : 2010.05.08
%%% @Description: 怪物
%%%-----------------------------------
-module(lib_mon).
-include("scene.hrl").
-include("server.hrl").

-export(
    [
        get_name_by_mon_id/1,
        remove_mon/1,
        remove_mon/3,
        remove_mon_mid/3,
        lookup/3,
        insert/1,
        insert/2,
        delete/1,
        get_scene_mon/2,
        get_scene_mon_aid/2,
        get_scene_mon_aid/3,
		get_scene_mon_by_mid/3,
        get_mon_by_mid/1,
        get_scene_mon_id_aid/2,
        get_area_mon_id_aid_mid/6,
        get_mon_for_battle/6,
        get_line_mon_for_battle/10,
        get_all_area_mon/3,
        get_ai/4,
        del_ai/2,
        del_all_area/2,
        create_mon/8,
        create_mon_cast/8,
        create_mon_arg/7,
        create_mon_arg_cast/7,
        clear_scene_mon_by_mid/4, %% 清除场景相同mid的怪物.
        clear_scene_mon_by_id/4,
        clear_scene_mon/3,
        klist/1,
        get_scene_mon_aid_by_id/3,
        change_mon_attr/4,
        change_mon_attr_scene/4
    ]
).

%% 获取指定id的怪物进程
get_scene_mon_aid_by_id(Scene, CopyId, Id) ->
    mod_mon_agent:apply_call(Scene, CopyId, lib_mon_agent, get_scene_mon_aid_by_id, [Id]).


%%获取本场景所有怪物信息按房间号
%%场景管理器内调用
get_scene_mon(Scene, CopyId) ->
    mod_mon_agent:apply_call(Scene, CopyId, lib_mon_agent, get_scene_mon, [CopyId]).

%% 获取本场景所有怪物进程id按房间号
%% 场景管理器内调用
get_scene_mon_aid(Scene, CopyId) ->
    mod_mon_agent:apply_call(Scene, CopyId, lib_mon_agent, get_scene_mon_aid, [CopyId]).

%% 获取本场景指定怪物进程id按房间号
%% 场景管理器内调用
get_scene_mon_aid(Scene, CopyId, Mids) ->
    mod_mon_agent:apply_call(Scene, CopyId, lib_mon_agent, get_scene_mon_aid, [CopyId, Mids]).

%% 获取本场景所有怪物id和进程id按房间号
%% 场景管理器内调用
get_scene_mon_id_aid(Scene, CopyId) ->
    mod_mon_agent:apply_call(Scene, CopyId, lib_mon_agent, get_scene_mon_id_aid, [CopyId]).

get_scene_mon_by_mid(Scene, CopyId, Mid) ->
    mod_mon_agent:apply_call(Scene, CopyId, lib_mon_agent, get_scene_mon_by_mid, [Mid]).

%% 获取mon名称用mon数据库id
get_name_by_mon_id(MonId)->
    "".
%%     case data_mon:get(MonId) of
%%         [] -> <<"">>;
%%         Mon -> Mon#ets_mon.name
%%     end.

%% 清除怪物
remove_mon(#ets_mon{id = MonId, aid = MonPid} = Mon) -> 
%%     case is_pid(MonPid) andalso misc:is_process_alive(MonPid) of
%%         true ->
%%             mod_mon_active:stop(MonPid);
%%         false ->
%%             delete(Mon)
%%     end,
%%     {ok, BinData} = pt_120:write(12006, [MonId]),
%%     lib_server_send:send_to_scene(Mon#ets_mon.scene, Mon#ets_mon.copy_id, BinData),
    ok.

remove_mon(Scene, CopyId, MonId) ->
    case lib_mon:lookup(Scene, CopyId, MonId) of
        [] -> 
            ok;
        Mon ->
            remove_mon(Mon)
    end.

remove_mon_mid(Scene, CopyId, Mid) ->
    MonList = get_scene_mon_by_mid(Scene, CopyId, Mid),
    [lib_mon:remove_mon(Mon) || Mon <- MonList].

%% 查找指定怪物信息
lookup(Scene, CopyId, Id) ->
    mod_mon_agent:apply_call(Scene, CopyId, lib_mon_agent, get_mon, [Id]).

%% 修改或者插入数据
insert(Mon) ->
    mod_mon_agent:apply_cast(Mon#ets_mon.scene, Mon#ets_mon.copy_id, lib_mon_agent, put_mon, [Mon]).
insert(Scene, Mon) ->
    mod_mon_agent:apply_cast(Scene, Mon#ets_mon.copy_id, lib_mon_agent, put_mon, [Mon]).

delete(Mon) ->
    mod_mon_agent:apply_cast(Mon#ets_mon.scene, Mon#ets_mon.copy_id, lib_mon_agent, del_mon, [Mon#ets_mon.id]).

get_mon_by_mid(MonId) ->
    case mod_scene_mon:lookup(MonId) of
        [] ->
            [];
        MonData ->
            MonData
    end.

get_mon_for_battle(Q, CopyId, X, Y, Area, Group) ->
    mod_mon_agent:apply_call(Q, CopyId, lib_mon_agent, get_mon_for_battle, [CopyId, X, Y, Area, Group]).

get_line_mon_for_battle(Q, CopyId, OX, OY, X, Y, Area, K, B, Group) ->
    mod_mon_agent:apply_call(Q, CopyId, lib_mon_agent, get_line_mon_for_battle, [CopyId, OX, OY, X, Y, Area, K, B, Group]).

get_all_area_mon(Q, Area, CopyId) ->
    mod_mon_agent:apply_call(Q, CopyId, lib_mon_agent, get_all_area_mon, [Area, CopyId]).

get_area_mon_id_aid_mid(Q, CopyId, X, Y, Area, Group) ->
    mod_mon_agent:apply_call(Q, CopyId, lib_mon_agent, get_area_mon_id_aid_mid, [CopyId, X, Y, Area, Group]).

%% 获取怪物ai
get_ai(SceneId, CopyId, X, Y) ->
    mod_mon_agent:apply_call(SceneId, CopyId, lib_mon_agent, get_ai, [SceneId, CopyId, X, Y]).

%% 删除怪物ai
del_ai(SceneId, CopyId) ->
    mod_mon_agent:apply_cast(SceneId, CopyId, lib_mon_agent, del_ai, [SceneId, CopyId]).

%% 删除怪物9宫格
del_all_area(SceneId, CopyId) ->
    mod_mon_agent:apply_cast(SceneId, CopyId, lib_mon_agent, del_all_area, [CopyId]).

%% 动态创建怪物
%%@param MonId 怪物资源ID
%%@param Scene 场景ID
%%@param X 坐标
%%@param X 坐标
%%@param Type 怪物战斗类型（0被动，1主动）
%%@param CopyId 房间号ID 为0时，普通房间，非0值切换房间。值相同，在同一个房间。
%%@param Lv 是否根据等级生成(0:不自动生成; 1,2..:自动生成)
%%@param Group 怪物阵营属性
%%@return MonAutoId 怪物自增ID，每个怪物唯一
%%                  对返回skip值容错
create_mon(MonId, Scene, X, Y, Type, CopyId, Lv, Group) -> 
    case mod_scene_agent:apply_call(Scene, mod_mon_create, create_mon_broadcast, [[MonId, Scene, X, Y, Type, CopyId, Lv, Group]]) of
		Id when is_integer(Id)->Id;
		_->0
	end.

%% 异步创建
create_mon_cast(MonId, Scene, X, Y, Type, CopyId, Lv, Group) ->
    create_mon_arg_cast(MonId, Scene, X, Y, Type, CopyId, [{auto_lv, Lv}, {group, Group}]).

%% 新创建怪物方法
%% Args:可变参数列表[Tuple1, Tuple2...]
%%            Tuple1 = tuple() = {auto_lv, V} | {group, V} | {cruise_info, V} | 
%%                               {owner_id, OwnerId} | {mon_name, MonName} |  {color, MonColor}
create_mon_arg(MonId, Scene, X, Y, Type, CopyId, Args) -> 
    case mod_scene_agent:apply_call(Scene, mod_mon_create, create_mon_arg, [[MonId, Scene, X, Y, Type, CopyId, 1, Args, 1]]) of
		Id when is_integer(Id)->Id;
		_->0
	end.

%% 新创建怪物方法
%% Args:可变参数列表[Tuple1, Tuple2...]
%%            Tuple1 = tuple() = {auto_lv, V} | {group, V} | {cruise_info, V} |
%%                               {owner_id, OwnerId} | {mon_name, MonName} |  {color, MonColor}
create_mon_arg_cast(MonId, Scene, X, Y, Type, CopyId, Args) ->
    mod_scene_agent:apply_cast(Scene, mod_mon_create, create_mon_arg_cast, [[MonId, Scene, X, Y, Type, CopyId, 1, Args, 1]]).

%% 清除场景相同mid的怪物
%% @param Scene 场景ID
%% @param CopyId 房间号ID 为0时，普通房间，非0值切换房间。值相同，在同一个房间。
%% @param Mid 怪物资源ID
%% @param 是否需要在清除的时候广播(0不广播，1广播)
clear_scene_mon_by_mid(SceneId, CopyId, Mid, BroadCast)->
    mod_scene_agent:apply_cast(SceneId, mod_mon_create, clear_scene_mon_by_mid, [SceneId, CopyId, Mid, BroadCast]).

%% 清除场景相同mid的怪物
%% @param Scene 场景ID
%% @param Id 怪物唯一id
%% @param 是否需要在清除的时候广播(0不广播，1广播)
clear_scene_mon(SceneId, CopyId, BroadCast)->
    mod_scene_agent:apply_cast(SceneId, mod_mon_create, clear_scene_mon_easy, [SceneId, CopyId, BroadCast]).


%% 清除场景所有怪物
%% @param Scene 场景ID
%% @param Id 怪物唯一id
%% @param 是否需要在清除的时候广播(0不广播，1广播)
clear_scene_mon_by_id(SceneId, CopyId, Id, BroadCast)->
    mod_scene_agent:apply_cast(SceneId, mod_mon_create, clear_scene_mon_by_id, [SceneId, CopyId, Id, BroadCast]).

%% 怪物伤害列表
klist(Aid) ->
    gen_fsm:sync_send_all_state_event(Aid, {'klist'}).


%% 场景线外部调用
change_mon_attr(Id, Scene, CopyID, AtrrList) ->
    mod_scene_agent:apply_cast(Scene, lib_mon, change_mon_attr_scene, [Id, Scene, CopyID, AtrrList]).

%% 改变怪物属性
%% Id: 怪物唯一id
%% AtrrList: list() = [Tuple ...], %% 怪物属性
%%           Tuple = {group, Value} | {hp, Value}
change_mon_attr_scene(Id, Scene, CopyID, AtrrList) ->
    mod_mon_agent:apply_cast(Scene, CopyID, lib_mon_agent, change_mon_attr, [Id, AtrrList]).
