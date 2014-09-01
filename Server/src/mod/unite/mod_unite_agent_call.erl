%%%-------------------------------------------------------------------
%%% @author JoyHuang
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%     公共线数据管理(call)
%%% @end
%%% Created : 15. 八月 2014 16:44
%%%-------------------------------------------------------------------
-module(mod_unite_agent_call).

%% API
-export([handle_call/3]).
-include("unite.hrl").
-include("common.hrl").

handle_call({get_online_num, all}, _From, State) ->
    Data = get(),
	Num = length([{K, V} || {K, V} <-Data, is_integer(K)]),
    {reply, Num, State};

%% 查找数据
handle_call({lookup, Id}, _From, State) ->
    Data1 = case get(Id) of
        undefined ->
            [];
        Data ->
            [Data]
    end,
    {reply, Data1, State};

%%　多条件查找
%%　返回值由使用者自己定义
handle_call({match, Type, Info}, _From, State) ->
    Data = do_match(Type, Info),
    {reply, Data, State};

handle_call(Request, _From, State) ->
    ?ERR("unknown request ~w", [Request]),
    {reply, ok, State}.

%% ------------------------------
%%
%%    内部函数:查询
%%
%% ------------------------------
do_match(Type, Info) ->
	%% 获取所有的数据
	Data = get(),
	%% 查找
	case Type of
		all_ids_by_lv_gap -> %%所有符合等级段在线玩家
			[MinLv, MaxLv] = Info,
			[K || {K, V} <- Data, is_integer(K),V#ets_unite.lv >= MinLv andalso V#ets_unite.lv =< MaxLv];
		all_ids -> %%所有在线玩家
			[K || {K, _V} <- Data, is_integer(K)];
		match_name ->	%% 根据名字查找玩家信息
			[PlayerName] = Info,
			D = lists:filter(fun({K, OneData}) ->
								 case is_integer(K) andalso OneData#ets_unite.name =:= util:make_sure_list(PlayerName) of
									 true ->
										 true;
									 false ->
										 false
								 end
						 end, Data),
			case D of
				[{_, OneEu}] ->
					[OneEu];
				_ ->
					[]
			end;
		get_scene_role_num -> %% 获取场景人数
			[SceneId, CopyId] = Info,
			Lthis = lists:foldr(fun({K, OneData}, AccList) ->
								case is_integer(K) andalso OneData#ets_unite.scene =:= SceneId andalso OneData#ets_unite.copy_id =:= CopyId of
									true ->
										case erlang:length(AccList) =:= 0 of
											true ->
												[OneData#ets_unite.id];
											false ->
												[OneData#ets_unite.id | AccList]
										end;
									false ->
										AccList
								end
						 end, [], Data),
			length(Lthis);
        get_scene_role_list -> %% 获取场景人物ID列表
            [SceneId, CopyId] = Info,
            lists:foldr(fun({K, OneData}, AccList) ->
                        case is_integer(K) andalso OneData#ets_unite.scene =:= SceneId andalso OneData#ets_unite.copy_id =:= CopyId of
                            true ->
                                case erlang:length(AccList) =:= 0 of
                                    true ->
                                        [OneData#ets_unite.id];
                                    false ->
                                        [OneData#ets_unite.id | AccList]
                                end;
                            false ->
                                AccList
                        end
                end, [], Data);
        get_scene_all_role_list -> %% 获取场景人物ID列表
            SceneId = Info,
            lists:foldr(fun({K, OneData}, AccList) ->
                        case is_integer(K) andalso OneData#ets_unite.scene =:= SceneId of
                            true ->
                                case erlang:length(AccList) =:= 0 of
                                    true ->
                                        [OneData#ets_unite.id];
                                    false ->
                                        [OneData#ets_unite.id | AccList]
                                end;
                            false ->
                                AccList
                        end
                end, [], Data);
        online ->
            [{RoleID, RoleName} || {RoleID, #ets_unite{name = RoleName}} <- Data, is_integer(RoleID)];
		_ ->
			[]
	end.