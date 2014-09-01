%%%-----------------------------------
%%% @Module  : lib_npc
%%% @Author  : xyao
%%% @Email   : jiexiaowen@gmail.com
%%% @Created : 2010.05.05
%%% @Description: npc
%%%-----------------------------------
-module(lib_npc).
-include("scene.hrl").
-export(
    [
        get_name_by_npc_id/1,
        get_data/1,
        get_scene_by_npc_id/1,
        is_near_npc/4,
        check_npc/5,
        get_scene_npc/1,
        get_npc_info_by_id/1,
        delete/1
    ]
).

%% 获取npc信息
get_npc_info_by_id(Id) ->
    case mod_scene_npc:lookup(Id) of
        [] ->
            [];
        Npc ->
            Npc
    end.

%% 删除npc信息
delete(Id) ->
    mod_scene_npc:delete(Id).

%% 获取指定场景的npc列表
%% 每次生成一份有点浪费内存，后续需改进
get_scene_npc(SceneId) ->
	mod_scene_npc:match(SceneId).

%% 获取npc名称用npc数据库id
get_name_by_npc_id(NpcId)->
    case data_npc:get(NpcId) of
        [] -> <<"">>;
        Npc -> Npc#ets_npc.name
    end.

%% 获取信息
get_data(NpcId) ->
    case data_npc:get(NpcId) of
        [] -> ok;
        Npc -> Npc
    end.

%% 获取NPC当前场景信息
get_scene_by_npc_id(NpcId) ->
    case mod_scene_npc:lookup(NpcId) of
        [] -> [];
        Data -> [Data#ets_npc.scene, Data#ets_npc.x, Data#ets_npc.y, Data#ets_npc.sname]
    end.

%% 根据NPC资源ID检查是否在NPC附近
is_near_npc(NpcId, PlayerScene, PlayerX, PlayerY) ->
    case get_scene_by_npc_id(NpcId) of
        [] -> false;
        [Scene, X, Y, _SceneName] ->
            (Scene =:= PlayerScene
                andalso abs(X - PlayerX) =< 5
                andalso abs(Y - PlayerY) =< 5 )
    end.

%% 根据NPC唯一ID检查是否在NPC附近
check_npc(NpcId, _NpcTypeId, PlayerScene, PlayerX, PlayerY) ->
    case mod_scene_npc:lookup(NpcId) of
        %% NPC不存在
        [] -> {fail, 1};
        Npc -> 
            case ( Npc#ets_npc.scene =:= PlayerScene andalso abs(Npc#ets_npc.x - PlayerX) =< 5
                    andalso abs(Npc#ets_npc.y - PlayerY) =< 5 ) of
                %% NPC不在附近
                false -> {fail, 3};
                true -> ok
            end
    end.
