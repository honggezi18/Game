%%%------------------------------------
%%% @author 严利宏 <542430172@qq.com>
%%% @copyright jieyou 2014.08.25
%%% @doc 打包程序.
%%% @end
%%%------------------------------------
%%% @private

-module(yx_app).
-behaviour(application).
-export([start/2, stop/1]).

start(normal, []) ->
    %%从启动参数-extra换取参数 (IP，端口, 节点Id)
    [E_Ip, E_Port, E_Sid | _] = init:get_plain_arguments(),
    Ip  = E_Ip,
    Port= list_to_integer(E_Port),
    Sid = list_to_integer(E_Sid),
    {ok, SupPid} = yx_sup:start_link(),

    %% 按节点Id(线路号)区分逻辑
    case Sid of
        0 -> % 跨服节点
            yx_clusters:start([Ip, Port, Sid]);
        1 -> % 公共线节点
            yx_unite:start([Ip, Port, Sid]);
        _ -> % 游戏线节点
            yx_server:start([Ip, Port, Sid])
    end,
    {ok, SupPid}.

stop(_State) ->
    void.

