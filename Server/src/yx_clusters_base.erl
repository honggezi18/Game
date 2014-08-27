%%%------------------------------------
%%% @author 严利宏 <542430172@qq.com>
%%% @copyright jieyou 2014.08.25
%%% @doc 跨服线基本逻辑服务启动.
%%% @end
%%%------------------------------------

-module(yx_clusters_base).
-export([start/0, stop/0]).

start() ->
    %% 跨服公用服务启动
    case config:get_cls_type() of
        1 ->
            %% 只在跨服中心启动
            cls_center();
        _ ->
            %% 只在跨服节点启动
            cls_node()
    end,
	ok.

%% 只在跨服中心启动
cls_center() ->
    ok.

%% 只在跨服节点启动
cls_node() ->
    ok.

stop() ->
    ok.

