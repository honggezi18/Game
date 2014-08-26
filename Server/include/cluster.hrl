%%%--------------------------------------
%%% @Module  : cluster.hrl
%%% @Author  : shebiao
%%% @Email   : shebiao@126.com
%%% @Created : 2011.07.27
%%% @Description : 跨服战集群公共包含文件
%%%--------------------------------------

-record(cluster_client_state, {platform          = "",
                               server_host       = undefined,
                               server_port       = 0,
                               server_id         = 0,
                               node_name         = undefined,
                               read_pid          = undefined,
                               regist_flag       = 0,
                               regist_time       = 0,
                               registry_key      = "",
                               socket            = undefined,
                               last_heartbeat_time = 0,
                               connect_state     = 0,
                               connect_state_time= 0,
                               heartbeat_timer   = undefined,
                               check_connect_timer = undefined,
                               reconnect_timer   = undefined}).

-record(cluster_server_state, {id              = "",
                               platform        = "",
                               server_id       = 0,
                               node_name       = undefined,
                               client_ip       = undefined,
                               client_port     = 0,
                               pid             = undefined,
                               read_pid        = undefined,
                               regist_flag     = 0,
                               registry_key    = "",
                               regist_time     = 0,
                               create_time     = 0,
                               socket          = undefined,
                               last_heartbeat_time = 0}).

-record(ets_cluster_node,     {id           = "",
                               platform     = "",
                               server_id    = 0,
                               node         = undefined,
                               ip           = undefined,
                               port         = 0,
                               socket       = undefied,
                               pid          = undefined,
                               read_pid     = undefined,
                               regist_flag  = 0,
                               regist_time  = 0,
                               create_time  = 0}).

-define(CLUSTER_HEADER_LEN,         8).
-define(ETS_CLUSTER,                ets_cluster).
-define(ETS_CLUSTER_NODE,           ets_cluster_node).

% 心跳间隔
-define(HEARTBEAT_INTERVAL,         15).
% 链路检测间隔
-define(CHECK_CONNECT_INTERVAL,     30).
% 链路失效间隔
-define(CONNECT_INVALID_INTERVAL,   45).
% 链路重连间隔
-define(RECONNECT_INTERVAL,         10).

-define(BATTLE_TYPE_KFZ_FRIEND,         1).
-define(BATTLE_TYPE_KFZ_ARENA,          2).
-define(BATTLE_TYPE_KFZ_3V3,            3).
-define(BATTLE_TYPE_KFZ_RANK,           4).
-define(BATTLE_TYPE_KFZ_TEAM_RANK,      5).
-define(BATTLE_TYPE_KFZ_CHAT,           6).
-define(BATTLE_TYPE_KFZ_MAIL,           7).
-define(BATTLE_TYPE_KFZ_FLOWER,         8).
-define(BATTLE_TYPE_KFZ_BOSS,           9).
-define(BATTLE_TYPE_KFZ_LEAGUE,        10).
-define(BATTLE_TYPE_KFZ_LEAGUE1,       11).
-define(BATTLE_TYPE_KFZ_MONUMENT,      12).
-define(BATTLE_TYPE_KFZ_ALLIANCE,      13).
-define(BATTLE_TYPE_KFZ_ALLIANCE1,     14).
-define(BATTLE_TYPE_KFZ_ACTIVITY_RANK, 15).
-define(BATTLE_TYPE_KFZ_QUIZ,          16).     %% 跨服文举赛
-define(BATTLE_TYPE_KFZ_COUNTRY_TREASURE, 17).  %% 国家宝藏
-define(BATTLE_TYPE_KFZ_RPC,           18).     %% 跨服远程调用
-define(BATTLE_TYPE_KFZ_WAR,           19).     %% 跨服国战
-define(BATTLE_TYPE_KFZ_STAR,          20).     %% 跨服天梯明星赛
-define(BATTLE_TYPE_KFZ_TERRITORY,     21).     %% 跨服领土战
-define(BATTLE_TYPE_PRIVILEGE,         22).     %% 国战官职特权

%% DataType
%% 排行榜模块相关跨服DataType
-define(BATTLE_DATA_TYPE_KFZ_RANK_UPDATE,          1).  %% 跨服榜数据更新(跨服1)
-define(BATTLE_DATA_TYPE_KFZ_ROLE_LOGIN,           2).  %% 名人堂玩家登录(跨服1)
-define(BATTLE_DATA_TYPE_KFZ_RANK_UPLOAD,          3).  %% 跨服榜数据上传(跨服1)
-define(BATTLE_DATA_TYPE_KFZ_ACTIVITY_RANK_UPDATA, 4).  %% 风云榜数据更新(跨服2)
-define(BATTLE_DATA_TYPE_KFZ_ACTIVITY_RANK_UPLOAD, 5).  %% 风云榜数据上传(跨服2)
-define(BATTLE_DATA_TYPE_KFZ_FIRST_HONOR_UPLOAD,   6).  %% 远征荣誉榜上传(跨服2)

%% 纪念碑模块DataType
-define(BATTLE_DATA_TYPE_KFZ_MONUMENT_DEVOTE, 1).   %% 纪念碑贡献建设
-define(BATTLE_DATA_TYPE_KFZ_MONUMENT_QUERY,  2).   %% 纪念碑查询
-define(BATTLE_DATA_TYPE_KFZ_MONUMENT_SYNC,   3).   %% 纪念碑数据同步
