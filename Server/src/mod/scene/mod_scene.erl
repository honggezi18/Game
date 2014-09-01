%%%------------------------------------
%%% @Module  : mod_scene
%%% @Author  : zhenghehe
%%% @Created : 2010.05.13
%%% @Description: 场景管理(场景元素)
%%%------------------------------------
-module(mod_scene).
-behaviour(gen_server).
-export([start_link/0, 
         copy_scene/2,          %% 加载场景动态信息（怪物）
         copy_scene/3,          %% 加载场景动态信息（怪物）
         copy_dungeon_scene/4,  %% 加载副本场景动态信息（怪物）
         clear_scene/2,         %% 清除场景动态信息（怪物）  
         load_scene/1,          %% 场景初始化，加载场景静态信息
         add_node_scene/1,      %% 本节点生成的场景
         get_node_scene/0,      %% 获取节点场景
         del_node_scene/1,      %% 删除本节点场景
         dungeon_load_mon/7,    %% 副本加载怪物
         dungeon_load_mon_defend/4
        ]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("common.hrl").
-include("scene.hrl").

-record(state, {auto_sid, scene_list}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% 本节点生成的场景
add_node_scene(Sid) ->
    gen_server:cast(?MODULE, {add_node_scene, Sid}).

%% 删除本节点的场景
del_node_scene(Sid) ->
    gen_server:cast(?MODULE, {del_node_scene, Sid}).

%% 获取节点场景
get_node_scene() ->
    gen_server:call(?MODULE, get_node_scene).

%% 加载场景动态信息（怪物）
copy_scene(SceneId, CopyId) ->
    copy_scene(SceneId, CopyId, 0).

copy_scene(SceneId, CopyId, Lv) ->
    ok.
%%     case data_scene:get(SceneId) of
%%         [] ->
%%             ok;
%%         S ->
%%             load_mon(S#ets_scene.mon, SceneId, CopyId, Lv, 0)
%%     end.
    
%% 加载副本场景动态信息（怪物）
%% @spec copy_dungeon_scene(SceneId, CopyId, Lv, DropNum) -> ok
%% DropNum = 1.2.3... 改场景的怪物掉落计算次数
%% @end
copy_dungeon_scene(SceneId, CopyId, Lv, DropNum) ->
    ok.
%%     case data_scene:get(SceneId) of
%%         [] ->
%%             [];
%%         S ->
%%             %加载箱子等静态元素初步定为第100波怪物
%%             dungeon_load_mon(S#ets_scene.mon, SceneId, CopyId, Lv, DropNum, 100, []),
%%             dungeon_load_mon(S#ets_scene.mon, SceneId, CopyId, Lv, DropNum, 1, [])
%%     end.
    
%% 清除场景动态信息（怪物）
clear_scene(SceneId, CopyId) ->
    catch clear_scene_helper(SceneId, CopyId),
    erlang:erase({get_scene_mon_pid, {SceneId, CopyId}}),
    ok.

clear_scene_helper(SceneId, CopyId) ->
%%     mod_mon_create:clear_scene_mon(SceneId, CopyId),    %% 清除怪物
    ok.

init([]) ->
    process_flag(trap_exit, true),
    F = fun(Id) ->
            load_scene(Id),
            timer:sleep(1000)
    end,
    SceneList = [],
    spawn(fun()-> lists:map(F, SceneList) end),
    State = #state{auto_sid = 1000000, scene_list=[]},
    {ok, State}.

%% 获取节点场景
handle_call(get_node_scene, _From, State) ->
    {reply, State#state.scene_list, State};

handle_call(_Request, _From, State) ->
    {reply, State, State}.

%% 本节点生成的场景
handle_cast({add_node_scene, Sid}, State) ->
    List = State#state.scene_list ++ [Sid],
    {noreply, State#state{scene_list = List}};

%% 删除本节点的场景
handle_cast({del_node_scene, Sid}, State) ->
    List = State#state.scene_list -- [Sid],
    {noreply, State#state{scene_list = List}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% 场景初始化，加载场景静态信息
load_scene(SceneId) ->
    NpcList = [],
    SceneName = io_lib:format("测试~p", [util:unixtime()]),
    MonList = [],
    load_npc(NpcList, SceneId, SceneName),
    load_mon_link(MonList, SceneId, SceneName),
    ok.

%% 加载NPC
load_npc([], _, _) ->
    ok;
load_npc([[NpcId, X, Y] | T], SceneId, SceneName) ->
%%     case data_npc:get(NpcId) of
%%         [] ->
%%             ok;
%%         N ->
%%             N1 = N#ets_npc{
%%                 id = NpcId,
%%                 x = X,
%%                 y = Y,
%%                 scene = SceneId,
%%                 sname = SceneName
%%             },
%%             mod_scene_npc:insert(N1)
%%     end,
    load_npc(T, SceneId, SceneName).

%%% 加载怪物
load_mon([], _, _, _, _) ->
    ok;
load_mon([[MonId, X, Y, Type, Group, _Boci] | T], SceneId, CopyId, Lv, DropNum) ->
    mod_mon_create:create_mon_arg([MonId, SceneId, X, Y, Type, CopyId, 0, [{auto_lv, Lv},{group, Group},{drop_num, DropNum}], 1]),
    load_mon(T, SceneId, CopyId, Lv, DropNum).

%%% 加载怪物（副本专用，需要获得怪物ID返回值）
dungeon_load_mon([], _SceneId, _CopyId, _, _, _Boci, Ids) ->
    Ids;
dungeon_load_mon([[MonId, X, Y, Type, _Group, MBoci] | T], SceneId, CopyId, Lv, DropNum, Boci, Ids) ->
    Ids1 = case MBoci =:= Boci of
        true ->
            case Boci > 1 andalso Boci =/= 100 of
                true ->
                    mod_mon_create:create_mon_arg_cast([MonId, SceneId, X, Y, Type, CopyId, 1, [{putai, 0}], Lv]);
                _ ->
                    mod_mon_create:create_mon_arg_cast([MonId, SceneId, X, Y, Type, CopyId, 1, [], Lv])
            end,
            Ids ++ [MonId];
        _ ->
            Ids
    end,
    dungeon_load_mon(T, SceneId, CopyId, Lv, DropNum, Boci, Ids1).

dungeon_load_mon_defend(Scene, CopyId, Rule, Lv) ->
    spawn(fun() -> dungeon_load_mon_defend_helper(Rule, Scene, CopyId, Lv, []) end),
    Ids = [MonId||{MonId, _, _, _} <- Rule],
    Ids.

dungeon_load_mon_defend_helper([], _SceneId, _CopyId, _Lv, Ids) ->
    Ids;
dungeon_load_mon_defend_helper([T|H], SceneId, CopyId, Lv, Ids) ->
    [MonId1, X1, Y1, Timeout1] = case T of
        {MonId, X, Y} ->
            [MonId, X, Y, 500];
        {MonId, X, Y, Timeout} ->
            [MonId, X, Y, Timeout]
    end,
    case Timeout1 > 0 of
        true ->
            timer:sleep(1000);
        _ ->
            skip
    end,
    mod_mon_create:create_mon_arg_cast([MonId1, SceneId, X1, Y1, 1, CopyId, 1, [{putai, 0}, {trace_area, 10000}], Lv]),
    dungeon_load_mon_defend_helper(H, SceneId, CopyId, Lv, Ids ++ [MonId1]).



%% 加载怪物所在场景
load_mon_link([], _, _) -> ok;
load_mon_link([[MonId, X, Y | _] | T], SceneRes, Name) ->
    Data = data_mon:get(MonId),
    case Data =:= [] of
        true ->
            skip;
        false ->
            mod_scene_mon:insert(#ets_scene_mon{id = MonId, scene=SceneRes, 
						name = Name, lv = Data#ets_mon.lv, 
						mname= Data#ets_mon.name, 
						kind = Data#ets_mon.kind, 
						x = X, y = Y})
    end,
    load_mon_link(T, SceneRes, Name).
