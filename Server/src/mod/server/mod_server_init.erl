%%%------------------------------------
%%% @Module  : mod_server_init
%%% @Author  : xyao
%%% @Email   : jiexiaowen@gmail.com
%%% @Created : 2011.06.14
%%% @Description: 数据初始化
%%%------------------------------------
-module(mod_server_init).
-behaviour(gen_server).
-export([
            start_link/0,
            init_mysql/0
        ]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("common.hrl").
-include("record.hrl").
-include("server.hrl").

start_link() ->
    gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

init([]) ->
    %%初始ets表
    ok = init_ets(),
    %%初始mysql
    ok = init_mysql(),
    ok = init_server_status(),
    {ok, ?MODULE}.

handle_cast(_R , Status) ->
    {noreply, Status}.

handle_call(_R , _FROM, Status) ->
    {reply, ok, Status}.

handle_info(_Reason, Status) ->
    {noreply, Status}.

terminate(_Reason, Status) ->
    {ok, Status}.

code_change(_OldVsn, Status, _Extra)->
    {ok, Status}.

%% ================== 私有函数 =================
%% mysql数据库连接初始化
init_mysql() ->
    [DbHost, DbPort, DbUser, DbPass, DbName, DbEncode] = config:get_mysql(),
    db:start(DbHost, DbPort, DbUser, DbPass, DbName, DbEncode, 15), 
    ok.

%%初始ETS表
init_ets() ->
    %% 节点
    ets:new(?ETS_NODE, [{keypos, #node.id}, named_table, public, set]),
    ets:new(?SERVER_STATUS, [{keypos,#server_status.name}, named_table, public, set]), %%服务器信息
    %% 玩家在线数据
    ets:new(?ETS_ONLINE, [{keypos,#ets_online.id}, named_table, public, set]),
    ok.

init_server_status() ->
    %% 服务器开服时间
    Now_time = util:unixdate(),
%%     Open_time = case db:get_row(<<"select `reg_time` from `player_login` where 1 order by `id` limit 30,1 ">>) of
%%                     [] ->
%%                         Now_time;
%%                     [Reg_time] when is_number(Reg_time) ->
%%                         Time = util:unixdate(Reg_time),
%%                         Time;
%%                     _ ->
%%                         0
%%                 end,
    Open_time = Now_time,
    ets:insert(?SERVER_STATUS, #server_status{name=open_time, value=Open_time}),
    ok.
