%%%------------------------------------
%%% @Module  : mod_disperse
%%% @Author  : xyao
%%% @Email   : jiexiaowen@gmail.com
%%% @Created : 2010.08.18
%%% @Description: 线路分布
%%%------------------------------------
-module(mod_disperse).
-behaviour(gen_server).
-export([
            start_link/3,
            rpc_server_add/4,
            rpc_server_update/2,
            server_id/0,
            server_id_port/0,
            server_list/0,
            server_list_all/0,
            send_to_all/1,
            broadcast_to_world/2,
            rpc_cast_other_server/3,
            rpc_server_close/1,
            rpc_server_open/1,
            rpc_server_del/1,
            cast_to_close/0,
            cast_to_open/0,
            cast_to_del/0,
            rpc_to_chat/3,
            rpc_to_unite/3,
            rpc_to_cluster/3,
            rpc_to_cluster1/3,
            rpc_to_gate/3,
            rpc_cast_to_user/4,
            rpc_cast_by_id/4,
            rpc_call_by_id/4,
            get_before_line/1,
            save_before_line/1,
            open_quiz_sys/0,
            update_drop_factor/0,
            update_time_control/0,
            update_gift/0,
            open_falling_stone/1,
            update_notice/0,
            update_open_time/1,
            add_guild_award/2,
            server_show/0,
            refresh_sell/0,
            clear_daily_log/0,
            clear_td_mon/0,
            kill_td_mon/0,
	    clear_scene_mon/1,
            get_before_line_port/1,
            get_port/1,
            get_line/1
        ]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("common.hrl").
-include("record.hrl").
-record(state, {
        id,
        ip,
        port,
        node
    }
).

%%rpc调用聊天服务器
rpc_to_chat(Mod, Fun, Arg) ->
    case ets:lookup(?ETS_SERVER, 98) of
        []  -> false;
        [S] ->
             rpc:cast(S#server.node, Mod, Fun, Arg),
             ok
    end.

%%rpc调用公共服务器
rpc_to_unite(Mod, Fun, Arg) ->
    case ets:lookup(?ETS_SERVER, 99) of
        []  -> false;
        [S] ->
             rpc:cast(S#server.node, Mod, Fun, Arg),
             ok
    end.

%%rpc调用跨服服务器
rpc_to_cluster(Mod, Fun, Arg) ->
    case ets:lookup(?ETS_SERVER, 90) of
        []  -> false;
        [S] ->
             rpc:cast(S#server.node, Mod, Fun, Arg),
             ok
    end.

rpc_to_cluster1(Mod, Fun, Arg) ->
    case ets:lookup(?ETS_SERVER, 91) of
        []  -> false;
        [S] ->
             rpc:cast(S#server.node, Mod, Fun, Arg),
             ok
    end.

%%rpc调用网关服务器
rpc_to_gate(Mod, Fun, Arg) ->
    case ets:lookup(?ETS_SERVER, 0) of
        []  -> false;
        [S] ->
             rpc:cast(S#server.node, Mod, Fun, Arg),
             ok
    end.

%%rpc_cast调用到玩家线路
rpc_cast_to_user(Id, Mod, Fun, Arg) ->
    case ets:lookup(?ETS_CHAT, Id) of
        [] -> false;
        [Player] ->
             rpc_cast_by_id(Player#ets_chat.line, Mod, Fun, Arg)
    end.

%%rpc_cast调用指定线路
rpc_cast_by_id(Id, Mod, Fun, Arg) ->
    case ets:lookup(?ETS_SERVER, Id) of
        []  -> false;
        [S] ->
             rpc:cast(S#server.node, Mod, Fun, Arg)
    end.

%%rpc_call调用指定线路
rpc_call_by_id(Id, Mod, Fun, Arg) ->
    case ets:lookup(?ETS_SERVER, Id) of
        [S] -> rpc:call(S#server.node, Mod, Fun, Arg);
        _ -> []
    end.

%% rpc_cast调用其它所有线路
rpc_cast_other_server(Module, Fun, Args) ->
    ServerList = server_list(),
    rpc_cast_other_server(ServerList, Module, Fun, Args).

rpc_cast_other_server([], _Module, _Fun, _Args) -> ok;
rpc_cast_other_server([H | T], Module, Fun, Args) ->
    rpc:cast(H#server.node, Module, Fun, Args),
    rpc_cast_other_server(T, Module, Fun, Args).

%% 通知其他线路当前线路已经关闭了
cast_to_del() ->
    Sid = server_id(),
    case db_sql:get_all(<<"select `Id`,`node` from server">>) of
        [] ->
            [];
        Server ->
            F = fun([Id, Node]) ->
                    case Id =/= Sid of
                        true ->
                            Node1 = list_to_atom(binary_to_list(Node)),
                            case net_adm:ping(Node1) of
                                pong ->
                                    case catch rpc:cast(Node1, mod_disperse, rpc_server_del, [Sid]) of
                                        _ ->
                                            ok
                                    end;
                                pang ->
                                    ok
                            end;
                        false ->
                            ok
                    end
                end,
            [F(S) || S <- Server]
    end.

%%通知其他线路此线路暂时服务
cast_to_close() ->
    Sid = server_id(),
    db_sql:execute(io_lib:format(<<"update `server` set `state` = 1 where id = ~p">>,[Sid])),
    case db_sql:get_all(<<"select `id`, `node` from server">>) of
        [] ->
            [];
        Server ->
            F = fun(Node) ->
                    Node1 = list_to_atom(binary_to_list(Node)),
                    case net_adm:ping(Node1) of
                        pong ->
                            case catch rpc:cast(Node1, mod_disperse, rpc_server_close, [Sid]) of
                                _ ->
                                    ok
                            end;
                        pang ->
                            ok
                    end
                end,
            [F(Node) || [Id, Node] <- Server, Id /= 98,  Id /= 99]
    end.

%%显示线路
cast_to_open() ->
    Sid = server_id(),
    db_sql:execute(io_lib:format(<<"update `server` set `state` = 0 where id = ~p">>,[Sid])),
    case db_sql:get_all(<<"select `id`, `node` from server">>) of
        [] ->
            [];
        Server ->
            F = fun(Node) ->
                    Node1 = list_to_atom(binary_to_list(Node)),
                    case net_adm:ping(Node1) of
                        pong ->
                            case catch rpc:cast(Node1, mod_disperse, rpc_server_open, [Sid]) of
                                _ ->
                                    ok
                            end;
                        pang ->
                            ok
                    end
                end,
            [F(Node) || [Id, Node] <- Server, Id /= 98,  Id /= 99]
    end.

%% 查询当前战区ID号
%% 返回:int()
server_id() ->
    gen_server:call(?MODULE, get_server_id).

%% 查询当前战区ID和端口号
%% 返回:int()
server_id_port() ->
    gen_server:call(?MODULE, get_server_id_port).

%% 获取所有战区的列表(不包括当前战区, 0线的网关，99线的聊天)
%% 返回:[#server{} | ...]
server_list() ->
    Server = ets:tab2list(?ETS_SERVER),
    [S || S <- Server, S#server.id =/= 0, S#server.id < 89].

%% 获取显示线路数目:不包括本线路
server_show() ->
    Server = ets:tab2list(?ETS_SERVER),
    [S || S <- Server, S#server.id =/= 0, S#server.id < 89, S#server.state =:= 0, S#server.id =/= 88].

server_list_all() ->
    Server = ets:tab2list(?ETS_SERVER),
    [S || S <- Server].

%%获取先前线路
get_before_line(Accname) ->
    case ets:lookup(?BEFORE_LINE, Accname) of
        [] -> [1, 0, 0, 0];
        [RD] ->
%%            io:format("Accname:~p get Line = ~p, Scene = ~p, X = ~p, Y = ~p~n", [Accname, RD#before_line.line, RD#before_line.scene, RD#before_line.x, RD#before_line.y]),
            [RD#before_line.line, RD#before_line.scene, RD#before_line.x, RD#before_line.y]
    end.

%%获取先前线路和端口
get_before_line_port(Accname) ->
    Line = case ets:lookup(?BEFORE_LINE, Accname) of
               [] -> 1;
               [RD] -> RD#before_line.line
           end,
    Server = ets:lookup(?ETS_SERVER, Line),
    case Server =/= [] of
        true ->
            [S] = Server,
            {ok, Line, S#server.port};
        false->
            error
    end.

%%获取端口
get_port(Line) ->
    Server = ets:lookup(?ETS_SERVER, Line),
    case Server =/= [] of
        true ->
            [S] = Server,
            {ok, S#server.port};
        false->
            error
    end.

%%保存先前线路
save_before_line([Accname, Line]) ->
    R = #before_line{accname = Accname, line = Line},
    ets:insert(?BEFORE_LINE, R),
    ok;
save_before_line([Accname, Line, Scene, X, Y]) ->
    R = #before_line{accname = Accname, line = Line, scene = Scene, x = X, y = Y},
    ets:insert(?BEFORE_LINE, R),
    ok;
save_before_line(_) ->
    ok.

%% 接收其它战区的加入信息
rpc_server_add(Id, Node, Ip, Port) ->
    ?MODULE ! {rpc_server_add, Id, Node, Ip, Port}.

%%隐藏线路
rpc_server_close(Id) ->
    ?MODULE ! {rpc_server_close, Id}.

%%显示线路
rpc_server_open(Id) ->
    ?MODULE ! {rpc_server_open, Id}.

%%停止服务
rpc_server_del(Id) ->
    ?MODULE ! {rpc_server_del, Id}.

%% 广播到所有线路
send_to_all(Data) ->
    Servers = server_list(),
    broadcast_to_world(Servers, Data).

%% 接收其它战区的状态更新信息
rpc_server_update(Id, Num) ->
    ?MODULE ! {rpc_server_update, Id, Num}.

start_link(Ip, Port, Sid) ->
    gen_server:start_link({local,?MODULE}, ?MODULE, [Ip, Port, Sid], []).

init([Ip, Port, Sid]) ->
    %net_kernel:monitor_nodes(true),
    ets:new(?ETS_SERVER, [{keypos, #server.id}, named_table, public, set]),
    State = #state{id = Sid, ip = Ip, port = Port, node = node()},
    add_server([State#state.ip, State#state.port, State#state.id, State#state.node]),
    %% 获取并通知当前所有线路
    get_and_call_server(State),
    {ok, State}.

handle_cast(_R , State) ->
    {noreply, State}.

%% 获取战区ID号
handle_call(get_server_id, _From, State) ->
    {reply, State#state.id, State};

%% 获取战区ID和端口号
handle_call(get_server_id_port, _From, State) ->
    {reply, {State#state.id, State#state.port}, State};

handle_call(_R , _FROM, State) ->
    {reply, ok, State}.

%% 广播到其它战区的世界
handle_info({all, Data}, State) ->
    Servers = server_list(),
    broadcast_to_world(Servers, Data),
    {noreply, State};

%% 新线加入
handle_info({rpc_server_add, Id, Node, Ip, Port}, State) ->
    %case Id of
    %    0 -> skip;
    %    _ ->
    %        ets:insert(?ETS_SERVER, #server{id = Id, node = Node, ip = Ip, port = Port})
    %end,
    Line = get_last_line(),
    case Id > Line andalso Id =/= 88 of
        true ->
            ets:insert(?ETS_SERVER, #server{id = Id, node = Node, ip = Ip, port = Port, state = 1});
        false ->
            ets:insert(?ETS_SERVER, #server{id = Id, node = Node, ip = Ip, port = Port})
    end,
    {noreply, State};

%% 关闭线路
handle_info({rpc_server_del, Id}, State) ->
    %case Id of
    %    0 -> skip;
    %    _ ->
    %        ets:delete(?ETS_SERVER, Id),
    %        del_server(Id)
    %end,
    ets:delete(?ETS_SERVER, Id),
    del_server(Id),
    {noreply, State};

 %% 隐藏线路
handle_info({rpc_server_close, Id}, State) ->
    case ets:lookup(?ETS_SERVER, Id) of
        [S] ->
            ets:insert(?ETS_SERVER, S#server{state = 1});
            %db_sql:execute(io_lib:format(<<"update `server` set `state` = 1 where id = ~p">>,[Id]));
        _ -> skip
    end,
    {noreply, State};

 %% 显示线路
handle_info({rpc_server_open, Id}, State) ->
    case ets:lookup(?ETS_SERVER, Id) of
        [S] ->
            ets:insert(?ETS_SERVER, S#server{state = 0});
            %db_sql:execute(io_lib:format(<<"update `server` set `state` = 0 where id = ~p">>,[Id]));
        _ -> skip
    end,
    {noreply, State};

%% 其它线人数更新
handle_info({rpc_server_update, Id, Num}, State) ->
    case ets:lookup(?ETS_SERVER, Id) of
        [S] -> ets:insert(?ETS_SERVER, S#server{num = Num});
        _ -> skip
    end,
    {noreply, State};

%% 统计当前线路人数并广播给其它线路
handle_info(online_num_update, State) ->
    case State#state.id of
        0 -> skip;
        _ ->
            Num = ets:info(?ETS_ONLINE, size),
            ets:insert(?ETS_SERVER,
                #server{
                    id = State#state.id,
                    node = State#state.node,
                    ip = State#state.ip,
                    port = State#state.port,
                    num = Num
                }
            ),
            Servers = server_list(),
            broadcast_server_state(Servers, State#state.id, Num)
    end,
    {noreply, State};

%% 处理新节点加入事件
handle_info({nodeup, Node}, State) ->
    try
        rpc:cast(Node, mod_disperse, rpc_server_add, [State#state.id, State#state.node, State#state.ip, State#state.port])
    catch
        _:_ -> skip
    end,
    {noreply, State};

%% 处理节点关闭事件
handle_info({nodedown, Node}, State) ->
    %% 检查是否战区节点，并做相应处理
    case ets:match_object(?ETS_SERVER, #server{node = Node, _ = '_'}) of
        [_Z] ->
            ets:match_delete(?ETS_SERVER, #server{node = Node, _ = '_'});
        _ ->
            skip
    end,
    {noreply, State};

handle_info(_Reason, State) ->
    {noreply, State}.

terminate(_R, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra)->
    {ok, State}.


%% ----------------------- 私有函数 ---------------------------------

%% 广播到其它战区的世界频道
broadcast_to_world([], _Data) -> ok;
broadcast_to_world([H | T], Data) ->
    rpc:cast(H#server.node, lib_send, send_to_local_all, [Data]),
    broadcast_to_world(T, Data).

%% 广播当前在线给其它线
broadcast_server_state([], _Id, _Num) -> ok;
broadcast_server_state([H | T], Id, Num) ->
    rpc:cast(H#server.node, mod_disperse, rpc_server_update, [Id, Num]),
    broadcast_server_state(T, Id, Num).

%%加入服务器集群
add_server([Ip, Port, Sid, Node]) ->
    Line = get_last_line(),
    case Sid > Line andalso Sid =/= 88 of
        true ->
            db_sql:execute(<<"replace into `server` (`id`, `ip`, `port`, `node`, `state`) values(?, ?, ?, ?, ?)">>,[Sid, Ip, Port, Node, 1]);
        false ->
            db_sql:execute(<<"replace into `server` (id, ip, port, node) values(?, ?, ?, ?)">>,[Sid, Ip, Port, Node])
    end.

%%退出服务器集群
del_server(Sid) ->
    db_sql:execute(io_lib:format(<<"delete from `server` where id = ~p">>,[Sid])).

%%获取并通知所有线路信息
get_and_call_server(State) ->
    case db_sql:get_all(<<"select * from server">>) of
        [] ->
            [];
        Server ->
            F = fun([Id, Ip, Port, Node, S]) ->
                    Node1 = list_to_atom(binary_to_list(Node)),
                    Ip1 = binary_to_list(Ip),
                    case Id /= State#state.id of  % 自己不写入和不通知
                        true ->
                            case net_adm:ping(Node1) of
                                pong ->
                                    ets:insert(?ETS_SERVER,
                                        #server{
                                            id = Id,
                                            node = Node1,
                                            ip = Ip1,
                                            port = Port,
                                            state = S
                                        }
                                    ),
                                    %% 通知已有的线路加入当前线路的节点，包括线路0网关
                                    rpc:call(Node1, mod_disperse, rpc_server_add, [State#state.id, State#state.node, State#state.ip, State#state.port]);
                                pang ->
                                    del_server(Id)
                            end;
                        false ->
                            ok
                    end
                end,
            [F(S) || S <- Server]
    end.

%手动开启答题系统
open_quiz_sys() ->
    rpc_to_unite(mod_quiz_tick, cmd_start, []),
    ok.

%% 更新掉落系数
update_drop_factor() ->
    rpc_to_unite(goods_init, init_drop_factor, []).

%% 更新时间控制
update_time_control() ->
    lib_time_control:init(),
    rpc_cast_other_server(lib_time_control, init, []),
    rpc_to_unite(lib_time_control, init, []),
    rpc_to_cluster(lib_time_control, init, []),
    rpc_to_cluster1(lib_time_control, init, []).

%% 更新活动礼包表
update_gift() ->
    rpc_to_unite(pp_gift, refresh, []).

%% 手动开启天外来石活动
open_falling_stone(Type) ->
    rpc_to_gate(lib_falling_stone, start_falling, [Type]).

%% 更新活动礼包表
update_notice() ->
    rpc_to_chat(lib_notice, refresh_notice, []).

%% 添加帮派奖励
add_guild_award(GuildId, Bin) ->
    case goods_util:to_term(Bin) of
        [] -> skip;
        GoodsList when is_list(GoodsList) ->
            rpc_to_unite(lib_guild_award, add_award, [GuildId, GoodsList]),
            lib_guild:send_guild_all_server(GuildId, 'guild_battle_guild_award', [GuildId, <<>>, 0, <<>>]);
        _ -> skip
    end,
    ok.

%% 开服时间更新
update_open_time(Time) ->
    Open_time = util:get_open_time(),
    case Open_time =/= Time andalso Time >= 0 of
        true ->
            R = #server_status{ name=open_time, value=Time},
            ets:insert(?SERVER_STATUS, R),
            rpc_cast_other_server(ets, insert, [?SERVER_STATUS, R]),
            rpc_to_gate(ets, insert, [?SERVER_STATUS, R]),
            rpc_to_unite(lib_shop_limit, unite_update_open_time, [Time]);
        false -> skip
    end,
    ok.

%% 更新活动礼包表
refresh_sell() ->
    case server_id() of
        99 -> mod_sell:cast_sell_refresh();
        _ -> rpc_to_unite(mod_sell, cast_sell_refresh, [])
    end.

%% 获得最后关闭的线路
get_last_line() ->
    case db_sql:get_one("SELECT `open_num` FROM `log_online` order by id desc limit 1") of
        null  -> 2;
        Other -> case Other < 2 of true -> 2; false -> Other end
    end.

%% 清除每天计数日志
clear_daily_log() ->
    mod_daily:daily_clear(),
    %%通知线路更新
    Server = ets:tab2list(?ETS_SERVER),
    F = fun(S) ->
            rpc:cast(S#server.node, ets, delete_all_objects, [?ETS_DAILY])
    end,
    [F(S) || S <- Server, S#server.id =/= 0, S#server.id < 89],
    ok.

clear_td_mon() ->
    Lines = server_list_all(),
    F = fun(Line) ->
        case Line#server.id > 0 andalso Line#server.id =< 88 of
            true ->
                rpc_cast_by_id(Line#server.id, mod_disperse, kill_td_mon, []);
            _ ->
                ok
         end
    end,
    lists:foreach(F, Lines),
    ok.

kill_td_mon() ->
    List1 = ets:match(?ETS_MON, #ets_mon{id = '$1', scene_id = 660, _ = '_'}),
    List2 = ets:match(?ETS_MON, #ets_mon{id = '$1', scene_id = 662, _ = '_'}),
    F = fun([MonID]) ->
        lib_boss:kill_mid_mon(MonID)
   end,
   lists:foreach(F, List1 ++ List2),
   ok.

clear_scene_mon(ResId) ->
	List = ets:match(?ETS_MON, #ets_mon{id = '$1', scene_id = ResId, _ = '_'}),
    F = fun([MonID]) ->
        lib_boss:kill_mid_mon(MonID)
	end,
	lists:foreach(F, List),
	ok.

%% 查询指定玩家的线路
get_line(Id) ->
    case ets:lookup(?ETS_SERVER, 98) of
        []  -> false;
        [S] ->
             rpc:call(S#server.node, lib_chat, get_line, [Id])
    end.
