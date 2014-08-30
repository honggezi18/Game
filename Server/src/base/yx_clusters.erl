%%%------------------------------------
%%% @author xyao <jiexiaowen@gmail.com>
%%% @copyright jieyou 2014.08.25
%%% @doc 跨服线启动服务.
%%% @end
%%%------------------------------------
%%% @private

-module(yx_clusters).
-export([start/1, stop/0]).

%% @doc 跨服线中需要开启的开启服务
start([Ip, Port, Sid]) ->
    % 开启底层服务
    ok = start_disperse([Ip, Port, Sid]),
    % 开启跨服逻辑服务（区分跨服节点和跨服中心）
    ok = yx_clusters_base:start(),
    ok.

%% @doc 关闭服务器时需停止
stop() ->
    io:format("关闭~w...~n", [?MODULE]),
    ok.

%% @doc 开启线路管理服务
start_disperse([Ip, Port, Sid]) ->
    {ok,_} = supervisor:start_child(
        yx_sup,
        {mod_disperse,
            {mod_disperse, start_link,[Ip, Port, Sid]},
            permanent, 10000, supervisor, [mod_disperse]}),
    ok.
