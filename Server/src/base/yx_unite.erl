%%%------------------------------------
%%% @author 严利宏 <542430172@qq.com>
%%% @copyright jieyou 2014.08.25
%%% @doc 公共线启动服务.
%%% @end
%%%------------------------------------
%%% @private

-module(yx_unite).
-export([start/1, stop/0]).

% 公共线中需要开启的开启服务
start([Ip, Port, Sid]) ->
    % 开启底层服务
    ok = start_disperse([Ip, Port, Sid]),
    ok = start_sys_time(),
    ok = start_client(),
    ok = start_tcp(Port),
    % 开启公共逻辑服务
    ok = yx_unite_base:start(),
    ok.

%% 关闭服务器时需停止
stop() ->
    io:format("关闭~w...~n", [?MODULE]),
    ok.

%% 开启时间生成器
start_sys_time() ->
    {ok,_} = supervisor:start_child(
        sd_sup,
        {mod_time,
            {mod_time, start_link,[]},
            permanent, 10000, supervisor, [mod_time]}),
    ok.

%% 开启线路管理服务
start_disperse([Ip, Port, Sid]) ->
    {ok,_} = supervisor:start_child(
        sd_sup,
        {mod_disperse,
            {mod_disperse, start_link,[Ip, Port, Sid]},
            permanent, 10000, supervisor, [mod_disperse]}),
    ok.

%% 开启客户端监控树
start_client() ->
    {ok,_} = supervisor:start_child(
        sd_sup,
        {sd_tcp_client_sup,
            {sd_tcp_client_sup, start_link,[unite]},
            transient, infinity, supervisor, [sd_tcp_client_sup]}),
    ok.

%%开启tcp listener监控树
start_tcp(Port) ->
    {ok,_} = supervisor:start_child(
        sd_sup,
        {sd_tcp_listener_sup,
            {sd_tcp_listener_sup, start_link, [Port]},
            transient, infinity, supervisor, [sd_tcp_listener_sup]}),
    ok.
