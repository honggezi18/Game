%%%--------------------------------------
%%% @Module  : pp_scene
%%% @Author  : zhenghehe
%%% @Created : 2010.12.23
%%% @Description:  场景
%%%--------------------------------------
-module(pp_scene).
-export([
            handle/2,
            get_scene_info/2
        ]).
-include("server.hrl").
-include("scene.hrl").
-include("pt120_pb.hrl").

%%移动信息
handle(Status, #pt12001_tos{x=_X,y=_Y,fly=Fly,d1=D1,d2=D2,skill=Skill,scene=Scene}) ->
    #player_status{scene = CurScene} = Status,
    if
        CurScene =/= Scene -> %% 防止旧场景的走路坐标同步到新场景
            ignore;
        Fly == 1 orelse Fly == 2 ->
            if
                Status#player_status.lv =< 30 ->
                    X = _X,
                    Y = _Y;
                abs(Status#player_status.x - _X) > 30 orelse abs(Status#player_status.y - _Y) > 30 ->
                    X = Status#player_status.x,
                    Y = Status#player_status.y;
                true ->
                    X = _X,
                    Y = _Y
            end,
            mod_scene_agent:move(X, Y, Fly, D1, D2, Status, Skill),
            NewStatus = Status#player_status{x = X, y = Y},
            {ok, NewStatus};
        true ->
            if
                abs(Status#player_status.x - _X) > 30 orelse abs(Status#player_status.y - _Y) > 30 ->
                    X = Status#player_status.x,
                    Y = Status#player_status.y;
                true ->
                    X = _X,
                    Y = _Y
            end,
            mod_scene_agent:move(X, Y, Fly, D1, D2, Status, Skill),
            {ok, Status#player_status{x = X, y = Y}}
    end;

%%加载场景
handle(Status, #pt12002_tos{}) ->
    mod_scene_agent:load_scene(Status),

    %% 更新公共线的场景

    {ok, Status};

%%离开场景
%handle(Status, _) ->
%    lib_scene:leave_scene(Status);
 
%%切换场景
handle(Status, #pt12005_tos{scene_id =Id}) ->
    case Id == Status#player_status.scene of
        true ->
            [Id1, X, Y, Sid, Name] = case lib_scene:get_data(Status#player_status.scene) of
                S when is_record(S, ets_scene) ->
                    [Id, Status#player_status.x, Status#player_status.y, S#ets_scene.id, S#ets_scene.name];
                _ ->
                    case Status#player_status.lv >= 20 of
                        true ->
                            [50, 10 , 20, 50, <<"长安">>];
                        _ ->
                            [50, 10 , 20, 50, <<"花果山">>]
                    end
            end,
            {ok, BinData} = pt:pack(#pt12005_toc{id=Id1,x=X,y=Y,name=Name,scene_id=Sid}),
            lib_server_send:send_one(Status#player_status.socket, BinData),
            {ok, Status#player_status{scene = Id1, x = X, y = Y}};
        false ->
            case lib_scene:check_enter(Status, Id) of
                {false, Msg} ->
                    {ok, BinData} = pt:pack(#pt12005_toc{id=0,x=0,y=0,name=Msg,scene_id=0}),
                    lib_server_send:send_one(Status#player_status.socket, BinData);
                {true, Id1, X, Y, Name, Sid, Status1} ->
                    %% 场景特殊buff
                    %%告诉原来场景玩家你已经离开
                    lib_scene:leave_scene(Status),
                    {ok, BinData} = pt:pack(#pt12005_toc{id=Id1,x=X,y=Y,name=Name,scene_id=Sid}),
                    lib_server_send:send_one(Status1#player_status.socket, BinData),
                    Status2 = Status1#player_status{scene = Id1, x = X, y = Y},
                    {ok, hp_mp, Status2}
            end
    end;

handle(_Status, _Data) ->
    io:format("pp_scene handle no match : ~p~n", [[_Data]]),
    {error, "pp_scene no match"}.

get_scene_info([], InfoList) ->
    InfoList;
get_scene_info([SceneId|T], InfoList) ->
    case data_scene:get(SceneId) of
        [] ->
            get_scene_info(T, InfoList);
        Scene ->
            %%当前元素信息
            SceneElem = Scene#ets_scene.elem,
            %%当前npc信息
            SceneNpc = lib_npc:get_scene_npc(SceneId),
            get_scene_info(T, InfoList++[{SceneId, Scene#ets_scene.name, SceneElem, SceneNpc}])
    end.
