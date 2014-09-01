%%%-----------------------------------
%%% @Module  : mod_reader
%%% @Author  : xyao
%%% @Email   : jiexiaowen@gmail.com
%%% @Created : 2011.06.14
%%% @Description: 读取客户端
%%%-----------------------------------
-module(mod_reader).
-export([start_link/0, init/0, run/3, log/2]).

-define(TCP_TIMEOUT, 3000). % 解析协议超时时间
-define(HEART_TIMEOUT, 100000). % 心跳包超时时间
-define(HEART_TIMEOUT_TIME, 0). % 心跳包超时次数
-define(HEADER_LENGTH, 4). % 消息头长度
-define(LIMITE_REQUEST, 80). % 限制次数


%%flash843安全沙箱
-define(FL_POLICY_REQ, <<"<pol">>).
-define(FL_POLICY_REQ_All, <<"<policy-file-request/>\0">>).
-define(FL_POLICY_FILE, <<"<cross-domain-policy><allow-access-from domain='*' to-ports='*' /></cross-domain-policy>">>).

%%记录用户初始数据
-record(player, {
        socket = none,      % socket
        pid = none,         % 玩家进程
        login  = 0,         % 是否登录
        accid  = 0,         % 账户id
        accname = none,     % 账户名
        timeout = 0,        % 超时次数
        req_count = 0,      % 请求次数
        req_list = [],      % 请求列表
        req_time = 0        % 请求时间
    }).

start_link() ->
    {ok, proc_lib:spawn_link(?MODULE, init, [])}.

init() ->
    process_flag(trap_exit, true),
    Client = #player{
        pid = none,
        login  = 0,
        accid  = 0,
        accname = none,
        timeout = 0
    },
    receive
        {go, Socket} ->
            login_parse_packet(Socket, Client#player{socket = Socket})
    end.

%%登录验证
%%Socket：socket id
%%Client: client记录
login_parse_packet(Socket, Client) ->
    Ref = async_recv(Socket, ?HEADER_LENGTH, ?HEART_TIMEOUT),
    receive
        %%flash安全沙箱
        {inet_async, Socket, Ref, {ok, ?FL_POLICY_REQ}} ->
            Len = 23 - ?HEADER_LENGTH,
            async_recv(Socket, Len, ?TCP_TIMEOUT),
            gen_tcp:send(Socket, ?FL_POLICY_FILE),
            gen_tcp:close(Socket);

        %%登录处理
        {inet_async, Socket, Ref, {ok, <<Len:16, Cmd:16>>}} ->
            BodyLen = Len - ?HEADER_LENGTH,
            case BodyLen >= 0 of
                true ->
                    case BodyLen > 0 of
                        true ->
                            Ref1 = async_recv(Socket, BodyLen, ?TCP_TIMEOUT),
                            receive
                                {inet_async, Socket, Ref1, {ok, Binary}} ->
                                    case routing(Cmd, Binary) of
                                        {ok, Data} ->
                                            %%先验证登陆
                                            case pp_login:handle(Client, Data) of
                                                {ok, Client1} ->
                                                    login_parse_packet(Socket, Client1);
                                                {ok, enter, Client1} -> %% 进入逻辑
                                                    do_parse_packet(Socket, Client1);
                                                _ ->
                                                    login_parse_packet(Socket, Client)
                                            end;
                                        _ ->
                                            login_parse_packet(Socket, Client)
                                    end;
                                Other ->
                                    login_lost(Socket, Client, 0, Other)
                            end;
                        false ->
                            case routing(Cmd, <<>>) of
                                {ok, Data} ->
                                    %%先验证登陆
                                    case pp_login:handle(Client, Data) of
                                        {ok, Client1} ->
                                            login_parse_packet(Socket, Client1);
                                        {ok, enter, Client1} -> %% 进入逻辑
                                            do_parse_packet(Socket, Client1);
                                        _ ->
                                            login_parse_packet(Socket, Client)
                                    end;
                                _ ->
                                    login_parse_packet(Socket, Client)
                            end
                    end;
                false ->
                    login_lost(Socket, Client, 0, "proto error")
            end;

        %%超时处理
        {inet_async, Socket, Ref, {error,timeout}} ->
            case Client#player.timeout >= ?HEART_TIMEOUT_TIME of
                true ->
                    login_lost(Socket, Client, 0, {error,timeout});

                false ->
                    login_parse_packet(Socket, Client#player {timeout = Client#player.timeout+1})
            end;

        %%用户断开连接或出错
        Other ->
            login_lost(Socket, Client, 0, Other)
    end.

%%进入逻辑
%%Socket：socket id
%%Client: client记录
do_parse_packet(Socket, Client) ->
    Ref = async_recv(Socket, ?HEADER_LENGTH, ?HEART_TIMEOUT),
    receive
        {inet_async, Socket, Ref, {ok, <<Len:16, Cmd:16>>}} ->
            BodyLen = Len - ?HEADER_LENGTH,
            case BodyLen >= 0 of
                true ->
                    case BodyLen > 0 of
                        true ->
                            Ref1 = async_recv(Socket, BodyLen, ?TCP_TIMEOUT),
                            receive
                                {inet_async, Socket, Ref1, {ok, Binary}} ->
                                    case limit_req_time(Cmd, Client) of
                                        {ok, Client1} ->
                                            case routing(Cmd, Binary) of
                                                %%这里是处理游戏逻辑
                                                {ok, Data} ->
                                                    case catch gen:call(Client1#player.pid, '$gen_call', {'SOCKET_EVENT', Cmd, Data}) of
                                                        {ok,_Res} ->
                                                            do_parse_packet(Socket, Client1);
                                                        {'EXIT',Reason} ->
                                                            do_lost(Socket, Client1, Cmd, Reason)
                                                    end;
                                                _ ->
                                                    do_parse_packet(Socket, Client1)
                                            end;
                                        {false, Client1} -> %% 发送次数太多忽略不处理
                                            do_parse_packet(Socket, Client1)
                                    end;
                                Other ->
                                    do_lost(Socket, Client, Cmd, Other)
                            end;
                        false ->
                            case limit_req_time(Cmd, Client) of
                                {ok, Client1} ->
                                    case routing(Cmd, <<>>) of
                                        %%这里是处理游戏逻辑
                                        {ok, Data} ->
                                            case catch gen:call(Client1#player.pid, '$gen_call', {'SOCKET_EVENT', Cmd, Data}) of
                                                {ok,_Res} ->
                                                    do_parse_packet(Socket, Client1);
                                                {'EXIT',Reason} ->
                                                    do_lost(Socket, Client1, Cmd, Reason)
                                            end;
                                        _ ->
                                            do_parse_packet(Socket, Client1)
                                    end;
                                {false, Client1} -> %% 发送次数太多忽略不处理
                                    do_parse_packet(Socket, Client1)
                            end
                    end;
                false ->
                    do_lost(Socket, Client, Cmd, "proto error")
            end;

        %%超时处理
        {inet_async, Socket, Ref, {error,timeout}} ->
            case Client#player.timeout >= ?HEART_TIMEOUT_TIME of
                true ->
                    do_lost(Socket, Client, 0, {error,timeout});
                false ->
                    do_parse_packet(Socket, Client#player {timeout = Client#player.timeout+1})
            end;

        %%用户断开连接或出错
        Other ->
            do_lost(Socket, Client, 0, Other)
    end.

%%断开连接
login_lost(Socket, _Client, _Cmd, Reason) ->
    gen_tcp:close(Socket),
    exit({unexpected_message, Reason}).

%%退出游戏
do_lost(_Socket, Client, _Cmd, Reason) ->
    %log("======~p===~p=====~n", [_Cmd, Reason]),
    %io:format("======~p===~p=====~n", [_Cmd, Reason]),
    mod_login:logout(Client#player.pid),
    exit({unexpected_message, Reason}).

%%============================================
%%公共函数
%%============================================

%%路由
%%组成如:pt_10:read
routing(Cmd, Binary) ->
    %%-----------
    %% 旧的代码：使用自定义二进制流
    %%取前面二位区分功能类型
    %[H1, H2, H3, _, _] = integer_to_list(Cmd),
    %Module = list_to_atom("pt_"++[H1,H2,H3]),
    %Module:read(Cmd, Binary). % 不容错了，发错协议就断开
    %catch Module:read(Cmd, Binary).
    %%-----------
    %% 使用protobuf
    [H1,H2,H3,H4,H5 | _] = integer_to_list(Cmd),
    Module = list_to_atom([$p,$t,H1,H2,H3,$_,$p,$b]), % atom = pt123_pb
    RecordName = list_to_atom([$p,$t,H1,H2,H3,H4,H5,$_,$t,$o,$s]), % atom = decode_pt12345_toc
    RecordData = Module:decode(RecordName, Binary),
    {ok, RecordData}.

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
    case get("prof") of
        undefined ->
            {ok, Fl} = file:open("../logs/prof.txt", [write, append]),
            put("prof", Fl),
            F3 = Fl;
        F2 ->
            F3 = F2
    end,
    {{Y, M, D},{H, I, S}} = erlang:localtime(),
    Format = list_to_binary("#prof" ++ " ~s \r\n" ++ F ++ "\r\n~n"),
    Date = list_to_binary([integer_to_list(Y),"-", integer_to_list(M), "-", integer_to_list(D), " ", integer_to_list(H), ":", integer_to_list(I), ":", integer_to_list(S)]),
    io:format(F3, unicode:characters_to_list(Format), [Date] ++ A).

%% 限制请求次数
limit_req_time(Cmd, Client) ->
    List = case lists:keyfind(Cmd, 1, Client#player.req_list) of
        false ->
            Client#player.req_list ++ [{Cmd, 1}];
        {_, _V} ->
            lists:keydelete(Cmd, 1, Client#player.req_list) ++ [{Cmd, _V+1}]
    end,
    Time = util:unixtime(),
    Count = Client#player.req_count + 1,
    case Count > ?LIMITE_REQUEST of
        true ->
            case Time - Client#player.req_time > 0 of
                true ->
                    {ok, Client#player{req_time = Time, req_count = 0, req_list=[]}};
                false ->
                    {false, Client#player{req_count = Count, req_list = List}}
            end;
        false ->
            case Time - Client#player.req_time > 0 of
                true ->
                    {ok, Client#player{req_time = Time, req_count = 0, req_list=[]}};
                false ->
                    case is_over_proto(List, Client#player.accname) of
                        true ->
                            {false, Client#player{req_count = Count, req_list = List}};
                        lose -> %%断线处理
                            do_lost(Client#player.socket, Client, Cmd, 0),
                            {false, Client#player{req_count = Count, req_list = List}};
                        false ->
                            {ok, Client#player{req_count = Count, req_list = List}}
                    end
            end
    end.

%% 是否发多次协议
is_over_proto([], _) ->
    false;
is_over_proto([{_Cmd, V} | T], Accname) ->
    if
        V > 14 ->
            true;
        true ->
            is_over_proto(T, Accname)
    end.

%%测试服用
%is_over_proto([], _) ->
%    false;
%is_over_proto([{Cmd, V} | T], Accname) ->
%    if
%        Cmd =:= 12001 ->
%            is_over_proto(T, Accname);
%        V > 14 ->
%            log("-cmd:~p, times:~p,acc:~p~n", [Cmd, V, Accname]),
%            lose;
%        true ->
%            is_over_proto(T, Accname)
%    end.

