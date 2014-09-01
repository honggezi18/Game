%%%------------------------------------------------
%%% File    : unite.hrl
%%% Author  : xyao
%%% Created : 2011-06-13
%%% Description: 公共线record
%%%------------------------------------------------

%-define(ETS_UNITE, ets_unite).                                  %% 公共线列表

%% 聊天信息
-record(ets_unite, {
        id,
        name,				%% 用户名
        gm = 0,				%% GM号
        talk_lim = 0,       %% 禁言
        talk_lim_time = 0,  %% 禁言截止时间
        scene = 0,			%% 场景
        copy_id = 0,		%% 房间标示
        sex,				%% 性别
        realm,				%% 阵营
        career,				%% 职业
        lv,					%% 等级
        image = 0,			%% 角色头像
        sid = [],           %% sid
        socket = none,      %% socket
        last_login_time = 0,%%上次登录时间
        pid = none		    %% 玩家进程ID
    }
).
