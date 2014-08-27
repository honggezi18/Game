%%%------------------------------------
%%% @author 严利宏 <542430172@qq.com>
%%% @copyright jieyou 2014.08.25
%%% @doc 读取客户端.
%%% @end
%%%------------------------------------

-module(yx_reader).
-export([start_link/3, start_link/4, init/4, run/3, log/2, get_line/0]).
-include("common.hrl").
-include("record.hrl").
-define(TCP_TIMEOUT, 3000). % 解析协议超时时间
-define(HEART_TIMEOUT, 100000). % 心跳包超时时间
-define(HEART_TIMEOUT_TIME, 0). % 心跳包超时次数
-define(HEADER_LENGTH, 6). % 消息头长度

%%记录客户端进程
-record(client, {
        player = none,
        login  = 0,
        accid  = 0,
        accname = none,
        timeout = 0, % 超时次数
        link_node = none,
        link_port = 0,
        calc_node = none
    }).
start_link(Type, Node, Port) ->
    start_link(Type, Node, Port, []).

start_link(Type, Node, Port, CalcNode) ->
    {ok, proc_lib:spawn_link(?MODULE, init, [Type, Node, Port, CalcNode])}.

%%=================================================
%%gen_server init启动函数
%%Host:主机IP
%%Port:端口
%%=================================================
init(Type, Node, Port, CalcNode) ->
    process_flag(trap_exit, true),
    Client = #client{
        player = none,
        login  = 0,
        accid  = 0,
        accname = none,
        timeout = 0,
        link_node = Node,
        link_port =  Port,
        calc_node = CalcNode
    },
    case Type of
        gateway ->
            receive
                {go, Socket} ->
                    gateway_parse_packet(Socket)
            end;
        chat ->
            receive
                {go, Socket} ->
                    chat_parse_packet(Socket, Client)
            end;
        unite ->
            receive
                {go, Socket} ->
                    unite_parse_packet(Socket, Client)
            end;
        server ->
            receive
                {go, Socket} ->
                    login_parse_packet(Socket, Client)
            end;
        _ ->
            receive
                {go, Socket} ->
                    logic_parse_packet(Socket, Client)
            end
    end.


%%=============================================
%%网关服务器特定0线
%%发送要连接的IP和port到客户端，并关闭连接
%%=============================================
gateway_parse_packet(Socket) ->
    case gen_tcp:recv(Socket, ?HEADER_LENGTH) of
        {ok, ?FL_POLICY_REQ} ->
            Len = 23 - ?HEADER_LENGTH,
            gen_tcp:recv(Socket, Len, 1000),
            gen_tcp:send(Socket, ?FL_POLICY_FILE),
            clost_socket(Socket);

        {ok, <<_Len:16, 60000:32>>} ->
            List = pp_gateway:get_server_list(),
            {ok, Data} = pt_60:write(60000, List),
            gen_tcp:send(Socket, Data),
            clost_socket(Socket);

        {ok, <<Len:16, 60001:32>>} ->
            BodyLen = Len - ?HEADER_LENGTH,
            case gen_tcp:recv(Socket, BodyLen, 3000) of
                {ok, <<Bin/binary>>} ->
                    {Accname, _} = pt:read_string(Bin),
                    {ok, Data} = pt_60:write(60001, pp_gateway:is_create(Accname)),
                    gen_tcp:send(Socket, Data),
                    gateway_parse_packet(Socket);
                _ ->
                    clost_socket(Socket)
            end;

        _ ->
            clost_socket(Socket)
    end.

clost_socket(Socket) ->
    gen_tcp:close(Socket),
    exit({unexpected_message, "nromal_close"}).

%%================================================
%%聊天服务器特定98线
%%================================================
%%================================================
%%聊天服务器特定98线
%%================================================
chat_parse_packet(Socket, Client) ->
    Ref = async_recv(Socket, ?HEADER_LENGTH, ?HEART_TIMEOUT),
    receive
        %%flash安全沙箱
        {inet_async, Socket, Ref, {ok, ?FL_POLICY_REQ}} ->
            Len = 23 - ?HEADER_LENGTH,
            async_recv(Socket, Len, ?TCP_TIMEOUT),
            lib_send:send_one(Socket, ?FL_POLICY_FILE),
            gen_tcp:close(Socket);

        %%聊天处理
        {inet_async, Socket, Ref, {ok, <<Len:16, Cmd:32>>}} ->
            BodyLen = Len - ?HEADER_LENGTH,
            case BodyLen > 0 of
                true ->
                    Ref1 = async_recv(Socket, BodyLen, ?TCP_TIMEOUT),
                    receive
                        {inet_async, Socket, Ref1, {ok, Binary}} ->
                            case routing(Cmd, Binary) of
                                %%先验证登陆
                                {ok, login, Data} ->
                                    case pp_account:handle(10099, [Socket, 1], Data) of
                                        false ->
                                            {ok, BinData} = pt_10:write(10099, 0),
                                            lib_send:send_one(Socket, BinData),
                                            chat_login_lost(Socket, Client, 0, "login fail");
                                        Pid ->
                                            {ok, BinData} = pt_10:write(10099, 1),
                                            lib_send:send_one(Socket, BinData),
                                            Client1 = Client#client{player=Pid, login = 1},
                                            chat_parse_packet(Socket, Client1)
                                    end;

                                %%进入聊天逻辑
                                {ok, Data} ->
                                    case Client#client.login == 1 of
                                        true ->
                                            case catch gen:call(Client#client.player, '$gen_call', {'SOCKET_EVENT', Cmd, Data}) of
                                                {ok,_Res} ->
                                                    chat_parse_packet(Socket, Client);
                                                {'EXIT',Reason} ->
                                                    chat_login_lost(Socket, Client, Cmd, Reason)
                                            end;
                                        false ->
                                            %chat_login_lost(Socket, Client, Cmd, "not login")
                                            chat_parse_packet(Socket, Client)
                                    end;

                                _Other ->
                                    %chat_login_lost(Socket, Client, Cmd, "not login")
                                    chat_parse_packet(Socket, Client)
                            end;
                        Other ->
                            chat_login_lost(Socket, Client, 0, Other)
                    end;
                false ->
                    case Client#client.login == 1 of
                        true ->
                            gen:call(Client#client.player, '$gen_call', {'SOCKET_EVENT', Cmd, []}),
                            chat_parse_packet(Socket, Client);
                        false ->
                            chat_parse_packet(Socket, Client)
                    end
            end;

        %%超时处理
        {inet_async, Socket, Ref, {error,timeout}} ->
            case Client#client.timeout >= ?HEART_TIMEOUT_TIME of
                true ->
                    chat_login_lost(Socket, Client, 0, {error,timeout});

                false ->
                    chat_parse_packet(Socket, Client#client {timeout = Client#client.timeout+1})
            end;

        %%用户断开连接或出错
        Other ->
            chat_login_lost(Socket, Client, 0, Other)
    end.

%%断开连接
chat_login_lost(_Socket, Client, _Cmd, Reason) ->
    mod_chat:stop(Client#client.player),
    exit({unexpected_message, Reason}).


%%================================================
%%启动公共服务器 99线
%%================================================
unite_parse_packet(Socket, Client) ->
    Ref = async_recv(Socket, ?HEADER_LENGTH, ?HEART_TIMEOUT),
    receive
        %%flash安全沙箱
        {inet_async, Socket, Ref, {ok, ?FL_POLICY_REQ}} ->
            Len = 23 - ?HEADER_LENGTH,
            async_recv(Socket, Len, ?TCP_TIMEOUT),
            lib_send:send_one(Socket, ?FL_POLICY_FILE),
            gen_tcp:close(Socket);

        %%公共逻辑处理
        {inet_async, Socket, Ref, {ok, <<Len:16, Cmd:32>>}} ->
            BodyLen = Len - ?HEADER_LENGTH,
            case BodyLen > 0 of
                true ->
                    Ref1 = async_recv(Socket, BodyLen, ?TCP_TIMEOUT),
                    receive
                        {inet_async, Socket, Ref1, {ok, Binary}} ->
                            case routing(Cmd, Binary) of
                                %%先验证登陆
                                {ok, login, Data} ->
                                    case pp_account:handle(10099, [Socket, 2], Data) of
                                        false ->
                                            {ok, BinData} = pt_10:write(10099, 0),
                                            lib_send:send_one(Socket, BinData),
                                            unite_login_lost(Socket, Client, 0, "login fail");
                                        Pid ->
                                            {ok, BinData} = pt_10:write(10099, 1),
                                            lib_send:send_one(Socket, BinData),
                                            Client1 = Client#client{player=Pid, login = 1},
                                            unite_parse_packet(Socket, Client1)
                                    end;

                                %%进入逻辑
                                {ok, Data} ->
                                    case Client#client.login == 1 of
                                        true ->
                                            case catch gen:call(Client#client.player, '$gen_call', {'SOCKET_EVENT', Cmd, Data}) of
                                                {ok,_Res} ->
                                                    unite_parse_packet(Socket, Client);
                                                {'EXIT',Reason} ->
                                                    unite_login_lost(Socket, Client, Cmd, Reason)
                                            end;
                                        false ->
                                            unite_parse_packet(Socket, Client)
                                    end;

                                _Other ->
                                    unite_parse_packet(Socket, Client)
                            end;
                        Other ->
                            unite_login_lost(Socket, Client, 0, Other)
                    end;
                false ->
                    case Client#client.login == 1 of
                        true ->
                            catch gen:call(Client#client.player, '$gen_call', {'SOCKET_EVENT', Cmd, []}),
                            unite_parse_packet(Socket, Client);
                        false ->
                            unite_parse_packet(Socket, Client)
                    end
            end;

        %%超时处理
        {inet_async, Socket, Ref, {error,timeout}} ->
            case Client#client.timeout >= ?HEART_TIMEOUT_TIME of
                true ->
                    unite_login_lost(Socket, Client, 0, {error,timeout});

                false ->
                    unite_parse_packet(Socket, Client#client {timeout = Client#client.timeout+1})
            end;

        %%用户断开连接或出错
        Other ->
            unite_login_lost(Socket, Client, 0, Other)
    end.

%%断开连接
unite_login_lost(_Socket, Client, _Cmd, Reason) ->
    mod_unite:stop(Client#client.player),
    exit({unexpected_message, Reason}).

%%================================================
%%场景服务器 - 战斗，走路等 特定20000+线
%%================================================
logic_parse_packet(Socket, Client) ->
    Ref = async_recv(Socket, ?HEADER_LENGTH, ?HEART_TIMEOUT),
    receive
        %%flash安全沙箱
        {inet_async, Socket, Ref, {ok, ?FL_POLICY_REQ}} ->
            Len = 23 - ?HEADER_LENGTH,
            async_recv(Socket, Len, ?TCP_TIMEOUT),
            lib_send:send_one(Socket, ?FL_POLICY_FILE),
            gen_tcp:close(Socket);

        %%逻辑处理
        {inet_async, Socket, Ref, {ok, <<Len:16, Cmd:32>>}} ->
            BodyLen = Len - ?HEADER_LENGTH,
            case BodyLen > 0 of
                true ->
                    Ref1 = async_recv(Socket, BodyLen, ?TCP_TIMEOUT),
                    receive
                        {inet_async, Socket, Ref1, {ok, Binary}} ->
                            case routing(Cmd, Binary) of
                                %%先验证登陆
                                {ok, login, Data} ->
                                    case pp_account:handle(10100, [Socket, Client#client.link_node], Data) of
                                        false ->
                                            {ok, BinData} = pt_10:write(10100, 0),
                                            lib_send:send_one(Socket, BinData),
                                            logic_login_lost(Socket, Client, 0, "login fail");
                                        Pid ->
                                            {ok, BinData} = pt_10:write(10100, 1),
                                            lib_send:send_one(Socket, BinData),
                                            Client1 = Client#client{player=Pid, login = 1},
                                            logic_parse_packet(Socket, Client1)
                                    end;

                                %%进入逻辑
                                {ok, Data} ->
                                    case Client#client.login == 1 of
                                        true ->
                                            case catch gen:call(Client#client.player, '$gen_call', {'SOCKET_EVENT', Cmd, Data}) of
                                                {ok,_Res} ->
                                                    logic_parse_packet(Socket, Client);
                                                {'EXIT',Reason} ->
                                                    logic_login_lost(Socket, Client, Cmd, Reason)
                                            end;
                                        false ->
                                            logic_login_lost(Socket, Client, 0, "login fail")
                                    end;
                                _Other ->
                                    logic_parse_packet(Socket, Client)
                            end;
                        Other ->
                            logic_login_lost(Socket, Client, 0, Other)
                    end;
                false ->
                    case Client#client.login == 1 of
                        true ->
                            %% 心跳包
                            pp_account:handle(Cmd, Socket, []),
                            logic_parse_packet(Socket, Client);
                        false ->
                            logic_parse_packet(Socket, Client)
                    end
            end;

        %%超时处理
        {inet_async, Socket, Ref, {error,timeout}} ->
            case Client#client.timeout >= ?HEART_TIMEOUT_TIME of
                true ->
                    logic_login_lost(Socket, Client, 0, {error,timeout});

                false ->
                    logic_parse_packet(Socket, Client#client {timeout = Client#client.timeout+1})
            end;

        %%用户断开连接或出错
        Other ->
            logic_login_lost(Socket, Client, 0, Other)
    end.

%%断开连接
logic_login_lost(_Socket, Client, _Cmd, Reason) ->
    mod_logic:stop(Client#client.player),
    exit({unexpected_message, Reason}).


%%================================================
%%所有逻辑的服务器 特定30000+线
%%================================================
%%接收来自客户端的数据 - 先处理登陆
%%Socket：socket id
%%Client: client记录
login_parse_packet(Socket, Client) ->
    Ref = async_recv(Socket, ?HEADER_LENGTH, ?HEART_TIMEOUT),
    receive
        %%flash安全沙箱
        {inet_async, Socket, Ref, {ok, ?FL_POLICY_REQ}} ->
            Len = 23 - ?HEADER_LENGTH,
            async_recv(Socket, Len, ?TCP_TIMEOUT),
            lib_send:send_one(Socket, ?FL_POLICY_FILE),
            gen_tcp:close(Socket);

        %%登陆处理
        {inet_async, Socket, Ref, {ok, <<Len:16, Cmd:32>>}} ->
            BodyLen = Len - ?HEADER_LENGTH,
            case BodyLen > 0 of
                true ->
                    Ref1 = async_recv(Socket, BodyLen, ?TCP_TIMEOUT),
                    receive
                        {inet_async, Socket, Ref1, {ok, Binary}} ->
                            case routing(Cmd, Binary) of
                                %%先验证登陆
                                {ok, login, Data} ->
                                    case pp_account:handle(10000, [], Data) of
                                        true ->
                                            [Accid, Accname, _, _] = Data,
                                            Client1 = Client#client{
                                                login = 1,
                                                accid = Accid,
                                                accname = Accname
                                            },
                                            Uid = case lib_player:get_role_id_by_accname(Accname) of
                                                null ->
                                                    %% 创建页统计流失率
                                                    %%lib_account:count_reg(),
                                                    0;
                                                Id ->
                                                    Id
                                            end,
                                            {ok, BinData} = pt_10:write(10000, Uid),
                                            lib_send:send_one(Socket, BinData),
                                            login_parse_packet(Socket, Client1);
                                        false ->
                                            login_lost(Socket, Client, 0, "login fail")
                                    end;

                                %%读取玩家列表 
                                {ok, lists, _Data} ->
                                    case Client#client.login == 1 of
                                        true ->
                                            pp_account:handle(10002, Socket,  Client#client.accname),
                                            login_parse_packet(Socket, Client);
                                        false ->
                                            login_lost(Socket, Client, 0, "login fail")
                                    end;

                                %%删除角色 - 暂不需要，后续合服会用到，先留着
                                %{ok, delete, Id} ->
                                %    case Client#client.login == 1 of
                                %        true ->
                                %            pp_account:handle(10005, Socket, [Id, Client#client.accname]),
                                %            login_parse_packet(Socket, Client);
                                %        false ->
                                %            login_lost(Socket, Client, 0, "login fail")
                                %    end;

                                %%创建角色
                                {ok, create, Data} ->
                                    case Client#client.login == 1 of
                                        true ->
                                            Data1 = [Client#client.accid, Client#client.accname] ++ Data,
                                            pp_account:handle(10003, Socket, Data1),
                                            login_parse_packet(Socket, Client);
                                        false ->
                                            login_lost(Socket, Client, 0, "login fail")
                                    end;

                                %%进入游戏
                                {ok, enter, [Id, L]} ->
                                    case Client#client.login == 1 of
                                        true ->
                                            %% 获取IP
                                            Ip1 = case inet:peername(Socket) of
                                                        {ok, {Ip, _Port}} -> Ip;
                                                        {error, _Reason} -> {0,0,0,0}
                                                    end,
                                            %% 避免同时登陆账号
                                            case mod_disperse:rpc_call_by_id(0, mod_check_login, is_login, [Id, Ip1]) of
                                                1  ->
                                                    case mod_login:login(start, [Id, Client#client.accname, Client#client.link_node, Client#client.link_port, Ip1, Client#client.calc_node, L], Socket) of
                                                        {error, MLR} ->
                                                            %%告诉玩家登陆失败
                                                            {ok, BinData} = pt_59:write(59004, MLR),
                                                            lib_send:send_one(Socket, BinData),
                                                            login_lost(Socket, Client, 0, "login fail");
                                                        {ok, Pid, Server_id} ->
                                                            %%告诉玩家登陆成功
                                                            {ok, BinData} = pt_10:write(10004, [1, Server_id]),
                                                            lib_send:send_one(Socket, BinData),
                                                            login_parse_packet(Socket, Client#client {player = Pid})
                                                    end;
                                                LR ->
                                                    {ok, BinData} = pt_59:write(59004, LR),
                                                    lib_send:send_one(Socket, BinData),
                                                    login_lost(Socket, Client, 0, "login fail")
                                            end;
                                        false ->
                                            login_lost(Socket, Client, 0, "login fail")
                                    end;
                                {ok, doubt_check, Id} -> %% 小号验证 
                                    case Client#client.login == 1 of
                                        true ->
                                            pp_account:handle(10008, Socket, Id),
                                            %{ok, BinData} = pt_10:write(10008, 1),
                                            %lib_send:send_one(Socket, BinData),
                                            do_parse_packet(Socket, Client);
                                        false ->
                                            login_lost(Socket, Client, 0, "login fail")
                                    end;
                                _Other ->
                                    %%  其他协议接收不处理
                                    login_parse_packet(Socket, Client)
                            end;
                        Other ->
                            login_lost(Socket, Client, 0, Other)
                    end;
                false ->
                    case Client#client.login == 1 of
                        true ->
                            pp_account:handle(Cmd, Socket,  Client#client.accname),
                            login_parse_packet(Socket, Client);
                        false ->
                            login_parse_packet(Socket, Client)
                    end
            end;

        %%超时处理
        {inet_async, Socket, Ref, {error,timeout}} ->
            case Client#client.timeout >= ?HEART_TIMEOUT_TIME of
                true ->
                    login_lost(Socket, Client, 0, {error,timeout});

                false ->
                    login_parse_packet(Socket, Client#client {timeout = Client#client.timeout+1})
            end;

        %%用户断开连接或出错
        Other ->
            login_lost(Socket, Client, 0, Other)
    end.

%%接收来自客户端的数据 - 登陆后进入游戏逻辑
%%Socket：socket id
%%Client: client记录
do_parse_packet(Socket, Client) ->
    Ref = async_recv(Socket, ?HEADER_LENGTH, ?HEART_TIMEOUT),
    receive
        {inet_async, Socket, Ref, {ok, <<Len:16, Cmd:32>>}} ->
            BodyLen = Len - ?HEADER_LENGTH,
            case BodyLen > 0 of
                true ->
                    Ref1 = async_recv(Socket, BodyLen, ?TCP_TIMEOUT),
                    receive
                        {inet_async, Socket, Ref1, {ok, Binary}} ->
                            case routing(Cmd, Binary) of
                                %%这里是处理游戏逻辑
                                {ok, Data} ->
                                    case catch gen:call(Client#client.player, '$gen_call', {'SOCKET_EVENT', Cmd, Data}) of
                                        {ok,_Res} ->
                                            do_parse_packet(Socket, Client);
                                        {'EXIT',Reason} ->
                                            do_lost(Socket, Client, Cmd, Reason)
                                    end;
                                    %F1 = fun() -> gen_server:call(Client#client.player, {'SOCKET_EVENT', Cmd, Data}) end,
                                    %run(Cmd, Data, F1),
                                    %do_parse_packet(Socket, Client);
                                Other ->
                                    do_lost(Socket, Client, Cmd, Other)
                            end;
                        Other ->
                            do_lost(Socket, Client, Cmd, Other)
                    end;
                false ->
                    case routing(Cmd, <<>>) of
                        %%这里是处理游戏逻辑
                        {ok, Data} ->
                            case catch gen:call(Client#client.player, '$gen_call', {'SOCKET_EVENT', Cmd, Data}, 3000) of
                                {ok,_Res} ->
                                    do_parse_packet(Socket, Client);
                                {'EXIT',Reason} ->
                                    do_lost(Socket, Client, Cmd, Reason)
                            end;
                        Other ->
                            do_lost(Socket, Client, Cmd, Other)
                    end
            end;

        %%超时处理
        {inet_async, Socket, Ref, {error,timeout}} ->
            case Client#client.timeout >= ?HEART_TIMEOUT_TIME of
                true ->
                    do_lost(Socket, Client, 0, {error,timeout});
                false ->
                    do_parse_packet(Socket, Client#client {timeout = Client#client.timeout+1})            
            end;

        %%用户断开连接或出错
        Other ->
            do_lost(Socket, Client, 0, Other)
    end.


%%============================================
%%公共函数
%%============================================

%%断开连接
login_lost(Socket, Client, _Cmd, Reason) ->
    case is_pid(Client#client.player) of 
        true ->
            mod_login:logout(Client#client.player);
        false ->
            gen_tcp:close(Socket)
    end,
    exit({unexpected_message, Reason}).

%%退出游戏
do_lost(_Socket, Client, _Cmd, Reason) ->
    mod_login:logout(Client#client.player),
    exit({unexpected_message, Reason}).

%%路由
%%组成如:pt_10:read
routing(Cmd, Binary) ->
    %%取前面二位区分功能类型
    [H1, H2, _, _, _] = integer_to_list(Cmd),
    Module = list_to_atom("pt_"++[H1,H2]),
    Module:read(Cmd, Binary).

%% 接受信息
async_recv(Sock, Length, Timeout) when is_port(Sock) ->
    case prim_inet:async_recv(Sock, Length, Timeout) of
        {error, Reason} -> throw({Reason});
        {ok, Res}       -> Res;
        Res             -> Res
    end.

%%#####################队列########################
%queue_new() ->
%    queue:new().
%queue_put(Item, Q) ->
%    queue:cons(Item, Q).
%queue_get(Q) ->
%    case queue:is_empty(Q) of
%        true ->
%            {Q, []};
%        false ->
%            Data = queue:daeh(Q),
%            NewQ = queue:init(Q),
%            {NewQ, Data}
%    end.

%%####################测试#########################
%%性能测试
%%Fun:函数
%%Loop:执行次数
run(Cmd, Data, Fun) ->
    %statistics(wall_clock),
    L1 = util:longunixtime(),
    Fun(),
    %{_, T1} = statistics(wall_clock),
    L2 = util:longunixtime(),
    T1 = L2-L1,
    case T1 > 10 of
        true ->
            log("cmd:~p, data:=~p, time:~pms~n", [Cmd, Data, T1]);
        false ->
            ok
    end.

%% 日志记录函数
log(F, A) ->
    F3 = case get("file") of
        undefined ->
            {ok, Fl} = file:open("logs/prof.txt", [write, append]),
            put("file", Fl),
            Fl;
        F2 ->
            F2
    end,
    Format = list_to_binary(F ++ "\n~n"),
    io:format(F3, unicode:characters_to_list(Format), A).
    %file:close(Fl).

get_line() ->   
    case ets:info(?ETS_ONLINE, size) of
        undefined ->  
            0;
        N ->
            N
    end.
