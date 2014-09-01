%%%-----------------------------------
%%% @Module  : misc
%%% @Author  : xyao
%%% @Email   : jiexiaowen@gmail.com
%%% @Created : 2011.07.14
%%% @Description: 公共函数
%%%-----------------------------------
-module(misc).
-compile(export_all).

whereis_name(local, Atom) -> 
	erlang:whereis(Atom);

whereis_name(global, Atom) ->
	global:whereis_name(Atom).
 
register(local, Name, Pid) ->
	erlang:register(Name, Pid);

register(global, Name, Pid) ->
	global:re_register_name(Name, Pid).

unregister(local, Name) ->
	erlang:unregister(Name);

unregister(global, Name) ->
	global:unregister_name(Name).

is_process_alive(Pid) ->    
	try 
		if is_pid(Pid) ->
     			case node(Pid) =:= node() of
                    true ->
                        erlang:is_process_alive(Pid);
                    false ->
                        case rpc:call(node(Pid), erlang, is_process_alive, [Pid]) of
                            {badrpc, _Reason}  -> false;
                            Res -> Res
                        end
                end;
			true -> false
		end
	catch 
		_:_ -> false
	end.

%% 玩家进程名
player_process_name(PlayerId) ->
	list_to_atom(lists:concat([mod_player_, PlayerId])).

%% 公共线进程名
unite_process_name(PlayerId) ->
	list_to_atom(lists:concat([mod_unite_, PlayerId])).

%% 获取玩家进程
get_player_process(Id) ->
    misc:whereis_name(global, player_process_name(Id)).

%% 获取玩家公共线进程
get_unite_process(Id) ->
    misc:whereis_name(global, unite_process_name(Id)).

 %% 玩家发送进程名
player_send_process_name(PlayerId, Wid) ->
    list_to_atom(lists:concat([mls_, PlayerId, w_, Wid])).

 %% 场景管理进程名
scene_process_name(Sid) ->
    list_to_atom(lists:concat([mod_scene_agent_, Sid])).

 %% 场景管理进程名
mark_process_name(Sid) ->
    list_to_atom(lists:concat([mark_process_name_, Sid])).

 %% 怪物管理进程名
mon_process_name(Sid, CopyId) ->
    CopyId1 = case is_pid(CopyId) of
        true ->
              pid_to_list(CopyId);
        _ ->
          CopyId
    end,
    list_to_atom(lists:concat([mod_mon_agent_, Sid, CopyId1])).

%% 组队进程名
team_process_name(TeamId) -> 
	list_to_atom(lists:concat([mod_team_, TeamId])).

get_child_count(Atom) ->
	case whereis_name(local, Atom) of
		undefined -> 
			0;
		_ ->
			[_,{active, ChildCount},_,_] = supervisor:count_children(Atom),
			ChildCount
	end.

pg2_get_members(Pg2_name) ->
    L = case pg2:get_members(Pg2_name) of 
            {error, _} ->
                timer:sleep(100),
                pg2:get_members(Pg2_name);
            Other when is_list(Other) ->
                Other
        end,
    if  not is_list(L) -> [];
        true -> lists:usort(L)
    end.

%% 获取全局pid
%% 代替gen_server:casr({global, ?MODULE}, ...).
get_global_pid(Module) ->
    whereis_name(global, Module).
    %case get({global, Module}) of
    %    undefined ->
    %        Pid = whereis_name(global, Module),
    %        put({global, Module}, Pid),
    %        Pid;
    %    Pid ->
    %        Pid
    %end.

%% 监控玩家进程
monitor_pid(Key, Event) ->
    T = util:unixtime(),
    case get({monitor, Key}) of
        undefined ->
            put({monitor, Key}, {T, 1});
        {T1, C1}->
            case T - T1 > 0 of
                true ->
                    put({monitor, Key}, {T, 1});
                false ->
                    put({monitor, Key}, {T1, C1+1})
            end,
            case C1 > 200 of
                true ->
                    util:errlog("monitor_pid:~p~n", [Event]),
                    exit({unexpected_message, Event});
                false ->
                    skip
            end
    end.


%% TODO map
%% @doc 序列化record
serialize_record(R, FieldList) ->
    trans_rd_field(R, FieldList, 1, []).

trans_rd_field(_R, [], _Index, Acc) ->
    Acc;
trans_rd_field(R, [Field | Rest], Index, Acc) ->
    case catch erlang:element(Index + 1, R) of
        {'EXIT', _} ->
            trans_rd_field(R, Rest, Index + 1, Acc);
        V ->
            trans_rd_field(R, Rest, Index + 1, [{Field, V} | Acc])
    end.

%% @doc 反序列化
deserialize_record(R, FieldList, List) ->
    {_, KFields} = lists:foldl(fun(F, {Index, Acc}) ->
        {Index + 1, [{F, Index + 1} | Acc]}
    end, {0, []}, FieldList),
    trans_field_rd(KFields, R, List).

trans_field_rd(_KFields, R, []) ->
    R;
trans_field_rd(KFields, R, [{Key, Value} | Rest]) ->
    case lists:keyfind(Key, 1, KFields) of
        {_, Index} ->
            NewR = erlang:setelement(Index + 1, R, Value),
            trans_field_rd(KFields, NewR, Rest);
        _ ->
            trans_field_rd(KFields, R, Rest)
    end.
