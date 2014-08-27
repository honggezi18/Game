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
    start_scene_init(),
    start_scene_mon(),
    start_scene_npc(),
    start_mon(),
    %start_scene_mark(),
    start_scene(),
    start_mod_online(),
	
    ok.

%% 只在跨服节点启动
cls_node() ->
    ok.

stop() ->
    ok.


%%场景数据初始化
start_scene_init() ->
    {ok,_} = supervisor:start_child(
        gs_sup,
        {mod_scene_init,
            {mod_scene_init, start_link,[]},
            permanent, 10000, supervisor, [mod_scene_init]}),
    ok.

%%开启场景监控树
start_scene() ->
    {ok,_} = supervisor:start_child(
        gs_sup,
        {mod_scene,
            {mod_scene, start_link,[]},
            permanent, 10000, supervisor, [mod_scene]}),
    ok.

%%开启场景mark监控树
%start_scene_mark() ->
%    {ok,_} = supervisor:start_child(
%        gs_sup,
%        {mod_scene_mark,
%            {mod_scene_mark, start_link,[]},
%            permanent, 10000, supervisor, [mod_scene_mark]}),
%    ok.

%%场景怪物种类
start_scene_mon() ->
    {ok,_} = supervisor:start_child(
        gs_sup,
        {mod_scene_mon,
            {mod_scene_mon, start_link,[]},
            permanent, 10000, supervisor, [mod_scene_mon]}),
    ok.

%%场景npc种类
start_scene_npc() ->
    {ok,_} = supervisor:start_child(
        gs_sup,
        {mod_scene_npc,
            {mod_scene_npc, start_link,[]},
            permanent, 10000, supervisor, [mod_scene_npc]}),
    ok.

%%开启怪物监控树
start_mon() ->
    {ok,_} = supervisor:start_child(
        gs_sup,
        {mod_mon_create,
            {mod_mon_create, start_link,[]},
            permanent, 10000, supervisor, [mod_mon_create]}),
    ok.



%% 统计在线
start_mod_online() ->
    {ok,_} = supervisor:start_child(
               gs_sup,
               {mod_online,
                {mod_online, start_link,[]},
                permanent, 10000, supervisor, [mod_online]}),
    ok.

%% 跨服中心换线排队
start_change_line_queue() ->
    {ok,_} = supervisor:start_child(
               gs_sup,
               {mod_change_scene_cls_center,
                {mod_change_scene_cls_center, start_link,[]},
                permanent, 10000, supervisor, [mod_change_scene_cls_center]}),
    ok.

%% 排行榜服务进程
start_mod_rank_cls() ->
    {ok,_} = supervisor:start_child(
               gs_sup,
               {mod_rank_cls,
                {mod_rank_cls, start_link,[]},
                permanent, 10000, supervisor, [mod_rank_cls]}),
    ok.


