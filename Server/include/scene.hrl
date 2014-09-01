%%%------------------------------------------------
%%% File    : scene.hrl
%%% Author  : xyao
%%% Created : 2011-07-15
%%% Description: 场景管理器
%%%------------------------------------------------

%格子长度-X
-define(PX_LEN, 30).
%格子长度-Y
-define(PY_LEN, 30).

-define(TABLE_AREA(Id, CopyId),  {Id, 0, CopyId}).          %% 9宫格保存

%% 场景类型定义.
-define(SCENE_TYPE_NORMAL, 0).      %% 大厅场景

-define(SCENE_TYPE_CLUSTERS, 100).  %% 跨服场景.

%% 场景用户数据
%% 只保留场景所需的信息
-record(ets_scene_user, {
        id = 0,                        %% 用户ID
        nickname = [],                 %% 玩家名
        sex = 0,                       %% 性别 1男 2女
        lv = 1,                        %% 等级
        scene = 0,                     %% 场景id
        copy_id = 0,                   %% 副本id
        x = 0,                         %% X坐标
        y = 0,                         %% Y坐标
        node=none,                     %% 来自节点
        platform = "",                 %% 平台标示
        server_num = 0,                %% 所在的服标示
        sid = {},                      %% 玩家发送消息进程
        pid = 0,                       %% 玩家进程
        group = 0,                     %% 阵营
        hp = 0,                        %% 气血
        hp_lim = 0,                    %% 气血上限
        mp = 0,                        %% 内力
        mp_lim = 0,                    %% 内力上限
        career = 0,                    %% 职业
        realm = 0,                     %% 国家，阵营
        speed = 0,                     %% 玩家速度
        figure = 0,                    %% 形象
		image =0,                      %% 头像
        battle_attr = []               %% 战斗属性
    }).

%% 场景数据结构
-record(ets_scene, {
        id = 0,              %% 场景ID包括资源id
        worker = 0,          %% 工作进程编号
        name = <<>>,         %% 场景名称
        type = 0,            %% 场景类型(0:安全场景, 1:野外场景, 2:副本场景)
        x = 0,               %% 默认开始点
        y = 0,               %% 默认开始点
        elem=[],             %% 场景元素
        requirement = [],    %% 进入需求
        mask = "",
        npc = [],
        mon = [],
        sid = 0,
        width = 0,
        height = 0,
        dun_type = 0         %% 副本类型
    }).


%% 场景怪物 - 任务用
-record(ets_scene_mon, {
        id = 0,              %% 怪物ID
        scene = 0,           %% 场景
        name = <<>>,         %% 场景名称
        mname = <<>>,        %% 怪物名字
        kind = 0,            %% 怪物类型
        x = 0,               %% X坐标
        y = 0,               %% Y坐标
        lv = 0,              %% 怪物等级
		out = 0              %% 是否挂机
    }).


-record(ets_mon, {
        id = 0,
        name = <<>>,
        kind = 0,        %%怪物类型
        boss = 0,        %%BOSS类型
        career = 0,
        auto,            %% 0主动，1自动生成
        scene,           %%所属场景唯一
        copy_id,         %%副本id
        mid,             %%怪物类型ID
        icon = 0,        %% 资源
        group = 0,       %% 阵营
        lv,
        hp,
        hp_lim,
        mp,
        mp_lim,
        hp_num,          %% 回血数值
        mp_num,          %% 回魔数值
        att,             %% 攻击
        def,             %% 防御值
        speed,           %% 移动速度
        att_speed,       %% 攻击速度
        hit = 0,         %% 命中
        dodge = 0,       %% 躲避
        crit = 0,        %% 暴击
        ten = 0,         %% 坚韧

        skill = [],      %% 技能
        skill_pro = [],  %% 被动技能
        att_area,        %% 攻击范围
        trace_area,      %% 追踪范围
        beat_back,       %% 击退
        x,
        y,
        pixel_x,         %% 像素坐标X
        pixel_y,         %% 像素坐标Y
        d_x,             %% 默认出生X
        d_y,             %% 默认出生y
        aid = none,      %% 怪物活动进程
        retime,          %% 重生时间
        type = 0,        %% 怪物战斗类型（0被动，1主动）
        exp=0,           %% 怪物经验
        battle_status = [],   %% 战斗状态
        att_type = 0,    %% 0近战，1远程
        path = [],       %% 自动行走路径
        color = 0        %% 颜色属性
    }).

-record(ets_npc, {
        id = 0,           %% 是唯一id又是资源id
        func = 0,         %% 功能
        icon = 0,         %% 资源
        image = 0,        %% 头像
        name,
        scene,
        sname = <<>>,
        x,
        y,
        talk,
        realm = 0
    }).

%% 怪物活动进程state
-record(mon_act, {
        att = [],            %% 攻击对象[Id, Pid, AttType] AttType: 1怪物; 2玩家
        minfo=[],            %% 怪物信息 ets_mon{}
        klist=[],            %% 伤害列表
        hate=[],             %% 仇恨列表
        clist = [],          %% 采集列表
        ref=[],              %% 普通定时器引用
        ready_ref=[],        %% 等待场景信息返回定时器引用
        eref = [],           %% 事件定时器引用
        last_att_player_id = 0, %% 最后攻击的玩家id(用于计算怪物被怪物杀死时的掉落)
        create_time = 0,     %% 创建时间
        start_move_time = 0, %% 开始移动时间
        move2xy = [],        %% 移动目的地
        round_time = 0       %% 游击到期时间
    }).



