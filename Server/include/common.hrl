%%%------------------------------------------------
%%% File    : common.hrl
%%% Author  : xyao
%%% Created : 2011-06-14
%%% Description: 公共定义
%%%------------------------------------------------

%%错误处理
-define(DEBUG(F, A), util:log("debug", F, A, ?MODULE, ?LINE)).
-define(INFO(F, A), util:log("info", F, A, ?MODULE, ?LINE)).
-define(ERR(F, A), util:log("error", F, A, ?MODULE, ?LINE)).
-define(DEBUG1(F, A), util:errlog("debug", F, A, ?MODULE, ?LINE)).
-define(INFO1(F, A), util:errlog("info", F, A, ?MODULE, ?LINE)).
-define(ERR1(F, A), util:errlog("error", F, A, ?MODULE, ?LINE)).

%% 捕获异常
-define(TRY_CATCH(M, E),
    try M
    catch
        _:E ->
            ?ERR1("error ~w, stacktrace ~w", [E, erlang:get_stacktrace()])
    end).

-define(TRY_CATCH(M),
    ?TRY_CATCH(M, E)).

-define(HANDLE_CALL(M, Req, From, State),
    try
        M:handle_call(Req, From, State)
    catch
        _:E ->
            ?ERR1("error ~w, req ~w, state ~w, stacktrace ~w", [E, Req, State, erlang:get_stacktrace()]),
            {reply, error, State}
    end).

-define(HANDLE_CAST(M, Req, State),
    try
        M:handle_cast(Req, State)
    catch
        _:E ->
            ?ERR1("error ~w, req ~w, state ~w, stacktrace ~w", [E, Req, State, erlang:get_stacktrace()]),
            {noreply, State}
    end).

-define(HANDLE_INFO(M, Info, State),
    try
        M:handle_info(Info, State)
    catch
        _:E ->
            ?ERR1("error ~w, info ~w, state ~w, stacktrace ~w", [E, Info, State, erlang:get_stacktrace()]),
            {noreply, State}
    end).

%% 获取配置
-define(CONF_FIND(Conf, Key), Conf:get(Key)).

%%数据库
-define(DB, gs_mysql_conn).

%%游戏逻辑用户发送进程个数
-define(SERVER_SEND_MSG, 3).
%%聊天用户发送进程个数
-define(CHAT_SEND_MSG, 3).
%%公共服务用户发送进程个数
-define(UNITE_SEND_MSG, 3).

%%每个场景管理个数
-define(SCENE_AGENT_NUM, 10).

-define(DIFF_SECONDS_1970_1900, 2208988800).
-define(DIFF_SECONDS_0000_1900, 62167219200).
-define(ONE_DAY_SECONDS,        86400).

%% 线路定义
-define(UNITE, 1).
-define(MAIN, 10).

%% ETS
-define(SERVER_STATUS, server_status).                          %% 服务器信息
-define(ETS_NODE, ets_node).                                    %% 节点列表
-define(ETS_RELA_INFO, ets_rela_info).                          %% 好友资料
-define(ETS_HP_BAG, ets_hp_bag).                                %% 玩家血包ETS
-define(SECONDARY_PASSWORD, secondary_password).                %% 二级密码表

%%PK状态切换时间
-define(PK_CHANGE_TIME, 3600*1).
