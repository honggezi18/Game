%%%------------------------------------
%%% @Module  : mod_disperse
%%% @Author  : xyao
%%% @Email   : jiexiaowen@gmail.com
%%% @Created : 2011.06.24
%%% @Description: 线路分布式管理
%%%------------------------------------
-module(mod_disperse).
-behaviour(gen_server).
-export([
            start_link/1,
            start_link/3,
            rpc_node_add/5,
            node_id/0,
            node_list/0,
            node_all_list/0,
            rpc_node_hide/1,
            rpc_node_show/1,
            rpc_node_del/1,
            cast_to_hide/0,
            cast_to_show/0,
            cast_to_del/0,
            rpc_cast_by_id/4,
            rpc_call_by_id/4,
            get_unite_info/0,
            send_other_server/3,
            send_rand_other_server/3,
            call_rand_other_server/3,
            send_one/3,
            send_one_all_server/3,
            call_to_unite/3,
            cast_to_unite/3,
            call_by_line_id/5,
            cast_by_line_id/5,
            send_one_unite/3,
            apply_to_all_server/3,
            get_data_from_server/3,
            get_clusters_node/0
        ]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("common.hrl").
-include("unite.hrl").
-include("record.hrl").
-include("server.hrl").
-record(state, {
    id,
    ip,
    port,
    node,
    cookie
}).

%%获取公共线服务器端口和ip
get_unite_info()->
    case ets:lookup(?ETS_NODE, 1) of
        [] -> ["", 0];
        [N] ->
            [N#node.ip, N#node.port]
    end.

%% 通知其他线路当前线路已经关闭了
cast_to_del() ->
    Sid = node_id(),
    case db:get_all(<<"select `Id`,`node` from node">>) of
        [] ->
            [];
        Server ->
            del_node(Sid),
            F = fun([Id, Node]) ->
                    case Id =/= Sid of
                        true ->
                            Node1 = list_to_atom(binary_to_list(Node)),
                            case net_adm:ping(Node1) of
                                pong ->
                                    case catch rpc:cast(Node1, mod_disperse, rpc_node_del, [Sid]) of
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

%%隐藏线路
cast_to_hide() ->
    Sid = node_id(),
    case db:get_all(<<"select `node` from node">>) of
        [] ->
            [];
        Server ->
            db:execute(io_lib:format(<<"update `node` set `state` = 1 where id = ~p">>,[Sid])),
            F = fun(Node) ->
                    Node1 = list_to_atom(binary_to_list(Node)),
                    case net_kernel:connect_node(Node1) of
                        true ->
                            case catch rpc:cast(Node1, mod_disperse, rpc_node_hide, [Sid]) of
                                _ ->
                                    ok
                            end;
                        false ->
                            ok
                    end
                end,
            [F(Node) || [Node] <- Server]
    end.

%%显示线路
cast_to_show() ->
    Sid = node_id(),
    db:execute(io_lib:format(<<"update `node` set `state` = 0 where id = ~p">>,[Sid])),
    case db:get_all(<<"select `node` from node">>) of
        [] ->
            [];
        Server ->
            F = fun(Node) ->
                    Node1 = list_to_atom(binary_to_list(Node)),
                    case net_adm:connect_node(Node1) of
                        true ->
                            case catch rpc:cast(Node1, mod_disperse, rpc_node_show, [Sid]) of
                                _ ->
                                    ok
                            end;
                        false ->
                            ok
                    end
                end,
            [F(Node) || [Node] <- Server]
    end.

%% 查询当前节点ID号
%% 返回:int()
node_id() ->
    case catch gen:call(?MODULE, '$gen_call', get_node_id) of
        {ok, Res} ->
            Res;
        _ ->
            0
    end.

%% 获取所有游戏节点的列表
%% 返回:[#node{} | ...]
node_list() ->
    Node = ets:tab2list(?ETS_NODE),
    [S || S <- Node, S#node.id >= 10].

%% 获取所有游戏节点的列表
%% 返回:[#node{} | ...]
node_all_list() ->
    Node = ets:tab2list(?ETS_NODE),
    [S || S <- Node].

%% 接收其它节点的加入信息
rpc_node_add(Id, Node, Ip, Port, Cookie) ->
    ?MODULE ! {rpc_node_add, Id, Node, Ip, Port, Cookie}.

%%隐藏线路
rpc_node_hide(Id) ->
    ?MODULE ! {rpc_node_hide, Id}.

%%显示线路
rpc_node_show(Id) ->
    ?MODULE ! {rpc_node_show, Id}.

%%停止服务
rpc_node_del(Id) ->
    ?MODULE ! {rpc_node_del, Id}.

start_link([Ip, Port, Sid]) ->
    start_link(Ip, Port, Sid).
start_link(Ip, Port, Sid) ->
    gen_server:start_link({local,?MODULE}, ?MODULE, [Ip, Port, Sid], []).

init([Ip, Port, Sid]) ->
    State = #state{id = Sid, ip = Ip, port = Port, node = node(), cookie = erlang:get_cookie()},
    add_node([State#state.ip, State#state.port, State#state.id, State#state.node]),
    %% 获取并通知当前所有线路
    get_and_call_node(State),
    case Sid =:= 0 of
        true ->
            mod_clusters_node:reconnect_node();
        false ->
            skip
    end,
    {ok, State}.

handle_cast(_R , State) ->
    {noreply, State}.

%% 获取节点ID号
handle_call(get_node_id, _From, State) ->
    {reply, State#state.id, State};

handle_call(_R , _FROM, State) ->
    {reply, ok, State}.

%% 新线加入
handle_info({rpc_node_add, Id, Node, Ip, Port, Cookie}, State) ->
    ets:insert(?ETS_NODE, #node{id = Id, node = Node, ip = Ip, port = Port, cookie = Cookie}),
    {noreply, State};

%% 关闭线路
handle_info({rpc_node_del, Id}, State) ->
    ets:delete(?ETS_NODE, Id),
    {noreply, State};

 %% 隐藏线路
handle_info({rpc_node_hide, Id}, State) ->
    case ets:lookup(?ETS_NODE, Id) of
        [S] -> 
            ets:insert(?ETS_NODE, S#node{state = 1});
        _ -> skip
    end,
    {noreply, State};

 %% 显示线路
handle_info({rpc_node_show, Id}, State) ->
    case ets:lookup(?ETS_NODE, Id) of
        [S] ->
            ets:insert(?ETS_NODE, S#node{state = 0});
        _ -> skip
    end,
    {noreply, State};

%% 处理新节点加入事件
handle_info({nodeup, Node}, State) ->
    try
        rpc:cast(Node, mod_disperse, rpc_node_add, [State#state.id, State#state.node, State#state.ip, State#state.port, State#state.cookie])
    catch
        _:_ -> skip
    end,
    {noreply, State};

%% 处理节点关闭事件
handle_info({nodedown, Node}, State) ->
    %% 检查是否节点，并做相应处理
    case ets:match_object(?ETS_NODE, #node{node = Node, _ = '_'}) of
        [_Z] ->
            ets:match_delete(?ETS_NODE, #node{node = Node, _ = '_'});
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

%%加入服务器集群
add_node([Ip, Port, Sid, Node]) ->
    Cookie = erlang:get_cookie(),
    db:execute(io_lib:format(<<"replace into `node` (id, ip, port, node, cookie) values(~p, '~s', ~p, '~s', '~s')">>,[Sid, Ip, Port, Node, Cookie])).

%%退出服务器集群
del_node(Sid) ->
    db:execute(io_lib:format(<<"delete from `node` where id = ~p">>,[Sid])).

%%获取并通知所有线路信息
get_and_call_node(State) ->
    case db:get_all(<<"select * from node order by id">>) of
        [] ->
            [];
        Server ->
            F = fun([Id, Ip, Port, Node, Cookie, S]) ->
                    Node1 = list_to_atom(binary_to_list(Node)),
                    Cookie1 = list_to_atom(binary_to_list(Cookie)),
                    Ip1 = binary_to_list(Ip),
                    case Id /= State#state.id of  % 自己不写入和不通知
                        true ->
                            case net_adm:ping(Node1) of
                                pong ->
                                    ets:insert(?ETS_NODE,
                                        #node{
                                            id = Id,
                                            node = Node1,
                                            ip = Ip1,
                                            port = Port,
                                            cookie = Cookie1,
                                            state = S
                                        }
                                    ),
                                    %% 通知已有的线路加入当前线路的节点，包括线路0网关
                                    rpc:call(Node1, mod_disperse, rpc_node_add, [State#state.id, State#state.node, State#state.ip, State#state.port, State#state.cookie]);
                                pang ->
                                    del_node(Id)
                            end;
                        false ->
                            ok
                    end
                end,
            [F(S) || S <- Server]
    end.

%% -----------------------------------------------------------------
%% 调用指定线路的模块函数
%% -----------------------------------------------------------------

%%rpc_cast调用指定线路
rpc_cast_by_id(Id, Mod, Fun, Arg) ->
    case ets:lookup(?ETS_NODE, Id) of
        [] -> false;
        [S] ->
             rpc:cast(S#node.node, Mod, Fun, Arg)
    end.

%%rpc_call调用指定线路
rpc_call_by_id(Id, Mod, Fun, Arg) ->
    case ets:lookup(?ETS_NODE, Id) of
        [S] -> rpc:call(S#node.node, Mod, Fun, Arg);
        _ -> []
    end.

%%call调用公共服务器
call_to_unite(Mod, Fun, Arg) ->
    case ets:lookup(?ETS_NODE, 1) of
	    [] -> 
            [];
        [S] -> 
            rpc:call(S#node.node, Mod, Fun, Arg)
    end.

%%cast调用公共服务器
cast_to_unite(Mod, Fun, Arg) ->
   case ets:lookup(?ETS_NODE, 1) of
        [] -> [];
	[S] -> rpc:cast(S#node.node, Mod, Fun, Arg)
   end.

%% call_by_line_id(RoleLine, LineId, Mod, Func, Args) -> {badrpc, Reason} | [] | Result
call_by_line_id(RoleLine, LineId, Mod, Func, Args) ->
    case RoleLine =:= LineId of
        true ->
            apply(Mod, Func, Args);
        false ->
            mod_disperse:rpc_call_by_id(LineId, Mod, Func, Args)
    end.

cast_by_line_id(RoleLine, LineId, Mod, Func, Args) ->
    case RoleLine =:= LineId of
        true ->
            apply(Mod, Func, Args);
        false ->
            mod_disperse:rpc_cast_by_id(LineId, Mod, Func, Args)
    end.

%% -----------------------------------------------------------------
%% 调用其它分线的模块函数
%% -----------------------------------------------------------------
send_other_server(Module, Fun, Args) ->
    ServerList = mod_disperse:node_list(),
    send_other_server_helper(ServerList, Module, Fun, Args).

send_other_server_helper([], _Module, _Fun, _Args) -> ok;
send_other_server_helper([H | T], Module, Fun, Args) ->
    rpc:cast(H#node.node, Module, Fun, Args),
    send_other_server_helper(T, Module, Fun, Args).

send_rand_other_server(Module, Fun, Args) ->
    case mod_disperse:node_list() of
        [] ->
            skip;
        NodeList ->
            N = util:rand(1, length(NodeList)),
            Node = lists:nth(N, NodeList),
            rpc:cast(Node#node.node, Module, Fun, Args)
    end.

call_rand_other_server(Module, Fun, Args) ->
    case mod_disperse:node_list() of
        [] ->
            [];
        NodeList ->
            N = util:rand(1, length(NodeList)),
            Node = lists:nth(N, NodeList),
            rpc:call(Node#node.node, Module, Fun, Args)
    end.

%% 更新数据到所有其他逻辑线路
apply_to_all_server(Mod, Fun, Args) ->
    ServerList = mod_disperse:node_list(),    %% 所有逻辑线路
    AllServerNodes = [Server#node.node || Server <- ServerList],
    rpc:eval_everywhere(AllServerNodes, Mod, Fun, Args).

%% 从游戏逻辑服务器获得信息
get_data_from_server(Module, Func, Args) ->
    case ets:select(ets_node, [{#node{id = '$1', node='$2', _='_'}, [{'>=', '$1', 10}], ['$2']}]) of
        [] ->
            [];
        NodeList ->
            F = fun
                ([], _G) ->
                    [];
                ([Node | Nodes], G) ->
                    case rpc:call(Node, Module, Func, Args) of
                        {badrpc, _} ->
                            G(Nodes, G);
                        Data ->
                            Data
                    end
            end,
            F(NodeList, F)
    end.


%% 获取跨服节点
get_clusters_node() ->
    case get("get_clusters_node") of
        undefined ->
            case ets:lookup(?ETS_NODE, 0) of
                [S] -> 
                    put("get_clusters_node", S#node.node),
                    S#node.node;
                _ -> 
                    case mod_disperse:node_id() =:= 0 of
                        true ->
                            put("get_clusters_node", node()),
                            node();
                        false ->
                            none
                    end
            end;
        Node ->
            Node
    end.
    

%% -----------------------------------------------------------------
%% 发送消息给单个成员
%% -----------------------------------------------------------------
send_one(PlayerId, MsgType, Bin) ->
    Pids = ets:match(?ETS_ONLINE, #ets_online{pid='$1', id=PlayerId, _='_'}),
    F = fun([P]) ->
            gen_server:cast(P, {MsgType, Bin})
        end,
    [F(Pid) || Pid <- Pids].

send_one_all_server(PlayerId, MsgType, Bin) ->
    send_one(PlayerId, MsgType, Bin),
    mod_disperse:send_other_server(mod_disperse, send_one, [PlayerId, MsgType, Bin]).

send_one_unite(PlayerId, MsgType, Bin) ->
    Pids = ets:match(?ETS_ONLINE, #ets_online{pid='$1', id=PlayerId, _='_'}),
    F = fun([P]) ->
            gen_server:cast(P, {MsgType, Bin})
        end,
    [F(Pid) || Pid <- Pids].
