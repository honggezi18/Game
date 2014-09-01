%%%------------------------------------------------
%%% File    : server.hrl
%%% Author  : xyao
%%% Created : 2011-06-13
%%% Description: 游戏系统record_
%%%------------------------------------------------

-define(ETS_ONLINE, ets_online).            % 在线列表

%% 只为玩家统计用的，不要加别的字段进来了
-record(ets_online, {
        id = 0,								% 角色ID
        pid = 0,                            % 玩家进程
        sid = {},                           % 发送进程
        tid = none                          % 任务进程
    }).

%% 记录用户一些常用信息
%% 此处，模块数据, 尽量运用进程字典保存数据, put, get, 如一些活动的数据
%% 由于现在整个框架都是，函数的传参很多都是传整个结构体，会造成性能上的一些消耗
%% 因此，此结构体越小越好
-record(player_status, {
        id = 0,                             %% 用户ID
        accname = [],						%% 平台账号
        nickname = [],					    %% 玩家名
        last_login_time = 0,				%% 最后登陆时间
        sex = 0,							%% 性别 1男 2女
        realm = 0,						    %% 阵营
        platform = "",                      %% 平台标示
        server_num = 0,                     %% 所在的服标示
        career = 0,						    %% 职业
        lv = 1,							    %% 等级
        gm = 0,							    %% 是否GM
        gold = 0,							%% 元宝
        bgold = 0,						    %% 绑定元宝
        coin = 0,						    %% 铜钱
        bcoin = 0,						    %% 绑定铜钱
        scene = 0,						    %% 场景id
        copy_id = 0,						%% 同一场景不同房间标示（可以数字，字符串任意）
        y = 0,
        x = 0,
        socket,							    %% socket
        pid = none,						    %% 玩家服务进程
		hp = 200,							%% 气血
        hp_lim = 200,						%% 气血上限
		mp = 0,                        		%% 内力
        mp_lim = 0,                    		%% 内力上限
        att_speed = 800,                    %% 攻击速度
        speed = 0,                          %% 移动速度
        base_speed = 300,                   %% 基础移动速度
        exp = 0,
        exp_lim = 0,
        talk_lim = 0,                       %% 禁言，0为正常，1为禁言
        talk_lim_time = 0,                  %% 禁言截止时间
        talk_lim_right = 0,                 %% 禁言权限 0无权限,1有权限
        quickbar = [],                      %% 快捷栏
        cell_num = 0,                       %% 背包格子数
        storage_num = 12,                   %% 仓库格子数
        battle_status = [],                 %% 战斗状态
        online_flag = 0,
        image = 0,
        is_pay = false,                     %% 是否有充值，true为有充值
        reg_time = 0,
        sid = {},
        dailypid = none                     %% 玩家日常进程
}).