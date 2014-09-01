%%%------------------------------------
%%% @Module  : mod_unite_init
%%% @Author  : xyao
%%% @Email   : jiexiaowen@gmail.com
%%% @Created : 2011.06.30
%%% @Description: 公共线数据初始化
%%%------------------------------------
-module(mod_unite_init).
-behaviour(gen_server).
-export([
            start_link/0,
            init_mysql/0
        ]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("common.hrl").
-include("unite.hrl").
-include("record.hrl").

start_link() ->
    gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

init([]) ->
    %%初始mysql
    ok = init_mysql(),
    ok = init_ets(),
    {ok, ?MODULE}.

handle_cast(_R , Status) ->
    {noreply, Status}.

handle_call(_R , _FROM, Status) ->
    {reply, ok, Status}.

handle_info(_Reason, Status) ->
    {noreply, Status}.

terminate(Reason, Status) ->
	util:errlog("mod_unite_init__terminate reason: ~p ~n", [Reason]),
    {ok, Status}.

code_change(_OldVsn, Status, _Extra)->
    {ok, Status}.

%% ================== 私有函数 =================
%% mysql数据库连接初始化
init_mysql() ->
    [DbHost, DbPort, DbUser, DbPass, DbName, DbEncode] = config:get_mysql(),
    db:start(DbHost, DbPort, DbUser, DbPass, DbName, DbEncode, 30),
    ok.

%%初始ETS表
init_ets() ->
    %% 节点
    ets:new(?ETS_NODE, [{keypos, #node.id}, named_table, public, set]),
    ets:new(?SERVER_STATUS, [{keypos,#server_status.name}, named_table, public, set]),  %%服务器信息
	ok.
