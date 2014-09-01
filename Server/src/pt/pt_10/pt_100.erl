%%%-----------------------------------
%%% @Module  : pt_100
%%% @Author  : xyao
%%% @Email   : jiexiaowen@gmail.com
%%% @Created : 2010.04.29
%%% @Description: 注册登录系统
%%%-----------------------------------
-module(pt_100).
-export([read/2, write/2]).

%%
%%客户端 -> 服务端 ----------------------------
%%

%%登陆
read(10000, <<Accid:32, Tstamp:32, Bin/binary>>) ->
    {Accname, Bin1} = pt:read_string(Bin),
    {Ticket, _} = pt:read_string(Bin1),
    {ok, [Accid, Accname, Tstamp, Ticket]};

%%读取列表
read(10002, _R) ->
    {ok, []};

%%创建角色
read(10003, <<Realm:8, Career:8, Sex:8, Bin/binary>>) ->
    {Name, _} = pt:read_string(Bin),
    {ok, [Realm, Career, Sex, Name]};

%%选择角色进入游戏
read(10004, <<Id:32, Time:32, Bin/binary>>) ->
    {Ticket, _} = pt:read_string(Bin),
    {ok, [Id, Time, Ticket]};

%%删除角色
read(10005, <<Id:32>>) ->
    {ok, Id};

%%心跳包
read(10006, _) ->
    {ok, heartbeat};

%%检查名字
read(10010, <<Bin/binary>>) ->
    {Name, _} = pt:read_string(Bin),
    {ok, [Name]};

read(_Cmd, _R) ->
    {error, no_match}.

%%
%%服务端 -> 客户端 ------------------------------------
%%

%%登陆返回
write(10000, [Code, Num, Career, Time]) ->
    {ok, pt:pack(10000, <<Code:32, Num:8, Career:8, Time:32>>)};

%% 打包角色列表
write(10002, []) ->
    N = 0,
    LB = <<>>,
    {ok, pt:pack(10002, <<N:16, LB/binary>>)};
write(10002, L) ->
    N = length(L),
    F = fun([Pid, Status, Name, Sex, Lv, Career, Realm]) ->
            Name1 = pt:write_string(Name),
            <<Pid:32, Status:8, Career:8, Sex:8, Lv:16, Name1/binary, Realm:8>>
    end,
    LB = list_to_binary([F(X) || X <- L]),
    {ok, pt:pack(10002, <<N:16, LB/binary>>)};

%%创建角色
write(10003, [Code, Id]) ->
    Data = <<Code:8, Id:32>>,
    {ok,  pt:pack(10003, Data)};

%%选择角色进入游戏
write(10004, Code) ->
    Data = <<Code:8>>,
    {ok, pt:pack(10004, Data)};

%%删除角色
write(10005, Code) ->
    Data = <<Code:16>>,
    {ok, pt:pack(10005, Data)};

%%心跳包
write(10006, _) ->
    {ok, pt:pack(10006, <<>>)};

%%检查名字
write(10010, [Code]) ->
    Data = <<Code:8>>,
    {ok, pt:pack(10010, Data)};

write(_Cmd, _R) ->
    {ok, pt:pack(0, <<>>)}.
