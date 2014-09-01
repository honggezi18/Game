%%%-----------------------------------
%%% @Module  : config
%%% @Author  : xyao
%%% @Email   : jiexiaowen@gmail.com
%%% @Created : 2011.06.12
%%% @Description: 获取 .config里的配置信息
%%%-----------------------------------
-module(config).
-export([
        get_log_level/0,
        get_log_path/0,
        get_ticket/0,
        get_mysql/0,
        get_card/0,
        get_server_id/0,
        get_phone_gift/0,
        get_cls_type/0,
        get_cls_node/0,
        get_cls_cookie/0,
        get_platform/0,
        get_server_num/0,
        get_server_ids/0
    ]).

%% 日志系统配置文件
get_log_level() ->
    case application:get_env(gs, log_level) of
	{ok, LogLevel} -> LogLevel;
	_ -> 3
    end.

get_log_path() ->
    case application:get_env(gs, log_path) of
	{ok, LogPath} -> LogPath;
	_ -> "gs_alarm.log"
    end.

%% 私钥
get_ticket() ->
    case application:get_env(gs, ticket) of
	{ok, Ticket} -> Ticket;
	_ -> "ticket"
    end.

%% 获取服务器id
%% 格式如：合服前["S1"] 或 合服后["S1", "S2"]
get_server_id() ->
    case application:get_env(gs, card_server) of
	    {ok, _Ser} -> _Ser;
        _ -> []
    end.

%% 获取新手卡
get_card() ->
    Key = case application:get_env(gs, card_key) of
	{ok, _Key} -> _Key;
	_ -> "key"
    end,

    Ser = case application:get_env(gs, card_server) of
	{ok, _Ser} -> _Ser;
    _ -> []
    end,
    {Key, Ser}.

%% 手机钱包开关(0关，1开)
get_phone_gift() ->
    case application:get_env(gs, phone_gift) of
	{ok, Gift} -> Gift;
	_ -> 0
    end.


%% 获取mysql参数
get_mysql() ->
    Host1 = case application:get_env(gs, db_host) of
	{ok, Host} -> Host;
	_ -> "localhost"
    end,
    Port1 = case application:get_env(gs, db_port) of
	{ok, Port} -> Port;
	_ -> 3306
    end,
    User1 = case application:get_env(gs, db_user) of
	{ok, User} -> User;
	_ -> "root"
    end,
    Pass1 = case application:get_env(gs, db_pass) of
	{ok, Pass} -> Pass;
	_ -> "root"
    end,
    Name1 = case application:get_env(gs, db_name) of
	{ok, Name} -> Name;
	_ -> "test"
    end,
    Encode1 = case application:get_env(gs, db_encode) of
	{ok, Encode} -> Encode;
	_ -> utf8
    end,
    [Host1, Port1, User1, Pass1, Name1, Encode1].

%% 跨服类型
get_cls_type() ->
    case application:get_env(gs, cls_type) of
	{ok, Type} -> Type;
	_ -> 0
    end.

%% 跨服节点
get_cls_node() ->
    case application:get_env(gs, cls_node) of
	{ok, Node} -> Node;
	_ -> 'cls@127.0.0.1'
    end.

%% 跨服节点
get_cls_cookie() ->
    case application:get_env(gs, cls_cookie) of
	{ok, Cookie} -> Cookie;
	_ -> erlang:get_cookie()
    end.

%% 获取平台名
get_platform() ->
    case application:get_env(gs, platform) of
	{ok, Platform} -> Platform;
	_ -> ""
    end.

%% 获取当前所在的服名
get_server_num() ->
    case application:get_env(gs, server_num) of
	{ok, ServerNum} -> ServerNum;
	_ -> 0
    end.

%% 获取该服包含的所有合服服务器id
%% 返回 : 整数列表, 如[1, 6, 34]
get_server_ids() ->
    [list_to_integer(StrId) || [_S | StrId] <- get_server_id()].
