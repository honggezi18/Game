%%%------------------------------------
%%% @author 严利宏 <542430172@qq.com>
%%% @copyright jieyou 2014.08.25
%%% @doc 游戏线启动服务.
%%% @end
%%%------------------------------------

-module(yx_server).
-export([start/1, stop/0]).

% 游戏线中需要开启的开启服务
start([Ip, Port, Sid]) ->
    % 开启底层服务
    ok = start_disperse([Ip, Port, Sid]),
    ok = start_sys_time(),
    ok = start_client(),
    ok = start_tcp(Port),
    % 开启游戏逻辑服务
    ok = yx_server_base:start(),
    ok.

%% 关闭服务器时需停止
stop() ->
    io:format("关闭~w...~n", [?MODULE]),
    supervisor:terminate_child(sd_sup, sd_tcp_listener_sup),
    supervisor:terminate_child(sd_sup, timer_frame),
    supervisor:terminate_child(sd_sup, timer_day),
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
            {sd_tcp_client_sup, start_link,[server]},
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
