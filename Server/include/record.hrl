%%%------------------------------------------------
%%% File    : record.hrl
%%% Author  : xyao
%%% Created : 2010-04-15
%%% Description: record
%%%------------------------------------------------

%% 服务器信息
-record(server_status, {
        name,
        value
    }).


%% 所有线路记录
-record(server, {
        id,
        ip,
        port,
        node,
        num = 0,
        state = 0  %是否开放 0开放 1关闭
    }).

%% 用户登陆
-record(user_login, {
        id,
        time
    }).

%% 聊天信息
-record(ets_chat, {
        id,
        name,        %% 用户名
        socket,      %%
        line = 0,    %% 线路
        ban = 0,     %% 禁言
        gm = 0,      %% GM号
        vip = 0,     %% Vip等级
        %% VIP特权
        vip_lv = 0,  %% VIP特权等级
        vip_expire_time = 0,        % VIP特权过期时间
        scene,       %% 场景
        team_id,     %% 队伍
        sex,         %% 性别
        realm,       %% 阵营
        career,      %% 职业
        guild_id,    %% 帮派id
        guild_name,  %% 帮派名
        arena_info,  %% 竞技场信息（[DungeonId, Group]）
        show_names=[],%% 在聊天窗口显示的称号列表
        official_names=0, %%聊天窗口显示的官职id
        lv,          %% 等级
        image = 0,   %% 角色头像
        sid = none,  %% 广播进程
        pid = none,  %% 聊天线进程
        accept_greeting_num = 0,%% 接受祝福次数
        appointment = 0,    %% 是否处于仙侣奇缘状态
        platform = "",
        server_id = 0,
        kfz_chat_flag = 0,
        kfz_chat_lasttime = 0,
        warrior_job = none
    }).

%% 逻辑信息
-record(logic_status, {
        id,
        socket,
        link_node
    }).

%% 聊天信息
-record(chat_status, {
        id,
        socket,
        fortune_refresh_time = 0,       %% 运势刷新时间
        fortune_help_time = 0           %% 运势求助刷新时间
    }).

%%玩家物品记录
-record(goods, {
        id=0,           %% 物品Id
        player_id=0,    %% 角色Id
        guild_id=0,     %% 帮派Id，只有存入帮派仓库才会有值
        goods_id=0,     %% 物品类型Id，对应ets_goods_type.goods_id
        type=0,         %% 物品类型
        subtype=0,      %% 物品子类型
        equip_type=0,   %% 装备类型：0为无，1为武器，2为防具，3为饰品
        price_type=0,   %% 价格类型：1 铜钱, 2 绑定元宝，3 元宝，4 绑定铜钱
        price=0,        %% 购买价格
        sell_price=0,   %% 出售价格
        bind=0,         %% 绑定状态，0为不可绑定，1为可绑定还未绑定，2为可绑定已绑定
        trade=0,        %% 交易状态，0为可交易，1为不可交易
        sell=0,         %% 出售状态，0为可出售，1为不可出售
        isdrop=0,        %% 丢弃状态，0为可丢弃，1为不可丢弃
        level=0,        %% 物品等级
        vitality = 0,   %% 体力
        spirit = 0,     %% 灵力
        hp = 0,         %% 血量
        mp = 0,         %% 内力
        forza=0,        %% 力量
        agile=0,        %% 敏捷
        wit=0,          %% 智力
        thew=0,         %% 体质
        att=0,          %% 攻击
        def=0,          %% 防御
        hit = 0,        %% 命中
        dodge = 0,      %% 躲避
        crit = 0,       %% 暴击
        ten = 0,        %% 坚韧
        fire = 0,       %% 火抗
        ice = 0,        %% 冰抗
        drug = 0,       %% 毒抗
        speed=0,        %% 速度
        attrition=0,    %% 耐久度上限，当前耐久度由lib_goods:get_goods_attrition(UseNum)算得
        use_num=0,      %% 可使用次数，由lib_goods:get_goods_use_num(Attrition)算得
        suit_id=0,      %% 套装ID，0为无
        skill_id=0,     %% 技能ID，0为无
        quality=0,      %% 品质等级
        quality_his=[], %% 历史升星记录
        quality_fail=0, %% 历史最高品质升级失败次数
        quality_factor=0.000, %% 品质加成系数
        star=0,         %% 品质星星数
        stren=0,        %% 强化等级
        stren_ratio=0,  %% 强化附加成功率
        stren_his=0,    %% 历史最高强化等级
        stren_fail=0,   %% 历史最高强化等级的失败次数
        stren_exratio=0,%% 至尊强化加成百分比值
        hole=0,         %% 镶孔数
        hole1_goods=0,  %% 孔1所镶物品ID
        hole2_goods=0,  %% 孔2所镶物品ID
        hole3_goods=0,  %% 孔3所镶物品ID
        hole_exgoods=0, %% 至尊强化属性宝石孔
        location=0,     %% 物品所在位置，1 装备一，2 装备二，3 装备三, 4 背包，5 仓库
        cell=0,         %% 物品所在格子位置
        num=0,          %% 物品数量
        color=0,        %% 物品颜色，0 白色，1 绿色，2 蓝色，3 紫色，4 橙色
        expire_time=0,  %% 有效期，0为无
        addition = [],  %% 附加属性列表
        wash = [],      %% 洗炼属性列表
        soul = [],      %% 灵魂属性列表
        prefix=0,       %% 前缀，0普通，1优秀，2精良，3完美，4传说，5神话，6洪荒
        note = <<>>,    %% 标识
        unique_time=0   %% 无双神兵有效期，0为无
        ,sign_goods=0               %% 签名物品类型ID
        ,sign_content = <<>>        %% 签名内容
        ,sign_expire=0              %% 签名过期时间
        ,darkgold_purity=0          %% 红玉装备纯度
        ,darkgold_bless=0           %% 红玉装备祝福
        ,darkgold_bless_num=0       %% 红玉装备祝福次数
        ,darkgold_hole1_goods=0     %% 红玉装备镶嵌孔1
        ,darkgold_hole2_goods=0     %% 红玉装备镶嵌孔2
        ,darkgold_hole3_goods=0     %% 红玉装备镶嵌孔3
        ,darkgold_purity_num=0      %% 红玉装备纯度提升次数
   }).

%%玩家物品属性表
-record(goods_attribute, {
        id,             %% 编号
        player_id,      %% 角色Id
        gid,            %% 物品Id
        attribute_type, %% 属性类型，1 附加，2 强化，3 品质，4 镶嵌
        attribute_id,   %% 属性类型Id
        value_type,     %% 属性值类型，0为数值，1为百分比
        hp,             %% 气血
        mp,             %% 内力
        att,            %% 攻击
        def,            %% 防御
        hit,            %% 命中
        dodge,          %% 躲避
        crit,           %% 暴击
        ten             %% 坚韧
    }).

%%装备套装属性表
-record(suit_attribute, {
        id,             %% 编号
        suit_id,        %% 套装ID
        suit_num,       %% 套装件数
        attribute_id,   %% 属性类型Id
        value_type,     %% 属性值类型，0为数值，1为百分比
        value           %% 属性值
    }).

%% 物品掉落规则表
-record(ets_base_goods_drop, {
        mon_id=0,               %% 怪物编号
        boss=0,                 %% 是否BOSS
        stable_goods=[],        %% 固定掉落物品包
        rand_num=[],            %% 随机掉落数
        rand_goods=[],          %% 随机掉落物品包列表
        task=0,                 %% 是否有任务物品，0为无，1为有
        task_goods=[],          %% 任务物品类型列表
        interval_num=0,         %% 间隔掉落的特定数
        interval_time=0,        %% 间隔掉落时间，0为无
        interval_goods=[],      %% 间隔掉落物品包
        special_num=0,          %% 特定杀怪数，0为无
        special_goods=[],       %% 特定杀怪数掉落物品包
        limit_start=0,          %% 限制开始时间，0为无
        limit_end=0,            %% 限制结束时间，0为无
        limit_goods=[],         %% 限制物品列表
        counter_goods=[]        %% 有计数器的物品
    }).

%% 掉落规则
-record(ets_drop_rule, {
        mon_id=0,               %% 怪物编号
        boss=0,                 %% 是否BOSS
        task=0,                 %% 是否有任务物品，0为无，1为有
        broad=0,                %% 掉落物品是否广播场景，0不广播，1广播
        drop_list=[],           %% 掉落列表
        drop_rule=[],           %% 掉落规则
        counter_goods=[]        %% 需计数的物品列表
    }).

%% 掉落规则
-record(ets_drop_goods, {
        id=0,                   %% 编号
        mon_id=0,               %% 怪物Id
        type=0,                 %% 类型，0 随机掉落，1 固定掉落，2 任务物品
        list_id=0,              %% 列表ID
        goods_id=0,             %% 物品ID
        ratio=0,                %% 机率
        num=0,                  %% 最大数量
        stren=0,                %% 最大强化数
        prefix=0,               %% 最大前缀数
        bind=0,                 %% 绑定状态
        notice=0,               %% 是否公告，1公告，0不公告
        factor=0,               %% 是否使用系数，0不使用，1使用
        hour_start=0,           %% 时间段限制，0为无
        hour_end=0,             %% 时间段限制，0为无
        time_start=0,           %% 日期限制，0为无
        time_end=0,             %% 日期限制，0为无
        replace_list = []       %% 职业替换列表，[昆仑,逍遥,唐门]
    }).

%% 物品掉落表
-record(ets_drop, {
        id=0,             %% 编号
        player_id=0,      %% 角色ID
        team_id=0,        %% 组队ID
        scene=0,          %% 场景ID
        drop_goods=[],    %% 掉落物品[[物品类型ID,物品类型,物品数量,物品品质]...]
        goods_id=0,       %% 掉落物品 - 物品类型ID
        gid=0,            %% 玩家掉落物品ID
        num=0,            %% 最大数量
        stren=0,          %% 最大强化数
        prefix=0,         %% 最大前缀数
        bind=0,           %% 绑定状态
        notice=0,         %% 是否公告，1公告，0不公告
        broad = 0,        %% 是否广播场景，0不广播，1广播
%        goods_item=[],    %% 掉落物品
        expire_time=0,    %% 过期时间
        mon_id = 0,       %% 怪物类型ID
        mon_name = <<>>   %% BOSS怪物名字
    }).

%% 怪物掉落计数表
-record(ets_drop_counter, {
        mon_id=0,         %% 怪物编号
        drop_num=0,       %% 掉落数
        drop_time=0,      %% 完成掉落数时间
        mon_num=0,        %% 杀怪数
        mon_time=0        %% 完成杀怪数时间
    }).

%% 怪物物品掉落系数
-record(ets_drop_factor, {
        id=0,                   %% 编号
        drop_factor=1.00,       %% 掉落系数粗调
        drop_factor_list=[],    %% 掉落系数细调
        time=0                  %% 更新时间
    }).

%% 怪物掉落物品计数表
-record(ets_mon_goods_counter, {
        goods_id=0,         %% 物品类型ID
        goods_num=0,        %% 物品限制数
        ratio_list=[],      %% 附加掉率列表
        drop_num=0,         %% 物品掉落数
        time=0              %% 时间
    }).

%% 物品兑换规则
-record(ets_goods_exchange, {
        id=0,             %% 编号
        npc=0,            %% NPC编号
        type=0,           %% 兑换类型，0 物品兑换，1 竞技积分兑换
        method=0,         %% 兑换方式，0为固定兑换，1为随机兑换
        raw_goods=[],     %% 原物品列表
        dst_goods=[],     %% 兑换物品列表
        bind=0,           %% 兑换物品的绑定状态
        max_overlap=0,    %% 兑换物品的最大叠加数
        honour=0,         %% 荣誉需求
        king_honour=0,    %% 帝王谷荣誉需求
        lim_lv=0,         %% 等级需求
        lim_week=0,       %% 每周限制兑换数
        limit_num=0,      %% 每天限制兑换数
        limit_id=0,       %% 每天限制兑换数的ID
        start_time=0,     %% 开始时间
        end_time=0,       %% 结束时间
        status=0          %% 状态，0为未生效，1为生效
    }).

%% 炼化配方
-record(ets_forge, {
        id=0,               %% 编号
        name = <<>>,        %% 配方名称
        type=0,             %% 分类：1基础宝石，2丹药
        raw_goods=[],       %% 材料物品列表
        goods_id=0,         %% 炼化物品
        bind=0,             %% 绑定状态
        ratio=0,            %% 成功率
        coin=0,             %% 费用
        llpt=0,             %% 历练声望
        note=0              %% 是否公告，1是
    }).

%% 配偶信息 PlayerStatus#player_status.spouse
-record(spouse_info, {
        id = 0,
        name = <<>>,
        line = 0
    }).

%% 夫妻技能信息 PlayerStatus#player_status.couple_skill_info
-record(couple_skill_info, {
        couple_fly_level = 1,
        couple_fly_time = 0,
        couple_revive_level = 1,
        couple_revive_time = 0
    }).

%% 夫妻家园信息 PlayerStatus#player_status.couple_home
-record(couple_home_info, {
        home_id = 0,
        home_level = 0
    }).

%%###########ETS##################
-record(logic_online, {
        id = 0,
        nickname = none,
        pid = 0,
        sid = 0,
        socket = null,
        career=0,
        realm = 0,          % 阵营 秦、楚、汗
        scene = 0,
        scene_res_id = 0,       % 资源id
        x = 0,
        y = 0,
        hp = 0,
        hp_lim = 0,
        mp = 0,
        mp_lim = 0,
        lv = 1,
        guild_id = 0,        % 帮派ID
        guild_name = [],     % 帮派名称
        pet_figure = 0,      % 宠物形象
        pet_name = [],       % 宠物名称
        pet_level = 0,       % 宠物等级
        pet_aptitude = 0,    % 宠物资质
        pet_growth = 0,      % 宠物成长
        pet_attack_mode = 0, % 宠物攻击模式
        speed = 0,	     % 移动速度
        mount_figure = 0,   % 坐骑形像
        mount_equip = [0,0],    % 坐骑装备, [装备一，装备二]
        equip_current = [0,0,0,0,0,0],      % 当前装备类型ID - [武器, 衣服, 无双神兵, 武器强化数, 衣服强化数, 武器强化至尊加成]
        fashion_weapon = [0,0],             % 穿戴的武器时装 - [武器时装类型ID，武器时装强化数]
        fashion_armor = [0,0],              % 穿戴的衣服时装 - [衣服时装类型ID，衣服时装强化数]
        fashion_accessory = [0,0],          % 穿戴的挂饰时装 - [挂饰时装类型ID，挂饰时装强化数]
        fashion_head = [0,0],               % 穿戴的头饰时装 - [头饰时装类型ID，头饰时装强化数]
        fashion_foot = [0,0],               % 穿戴的足饰时装 - [足饰时装类型ID，足饰时装强化数]
        fashion_chat = [0,0],               % 穿戴的聊天弹框时装 - [聊天弹框时装类型ID，聊天弹框时装强化数]
        hide_fashion_weapon = 0,            % 是否隐藏武器时装，1为隐藏
        hide_fashion_armor = 0,             % 是否隐藏衣服时装，1为隐藏
        hide_fashion_accessory = 0,         % 是否隐藏饰品时装，1为隐藏
        hide_fashion_head = 0,              % 是否隐藏头饰时装，1为隐藏
        hide_fashion_foot = 0,              % 是否隐藏足饰时装，1为隐藏
        hide_fashion_chat = 0,              % 是否隐藏聊天弹框时装，1为隐藏
        suit_id = 0,
        stren7_num = 0,     % 全身装备加七强化数
        weapon_light = 0,   % 武器光效 （编号1-6，如果为0表示没有换装过，则用equip_curent字段的第四个值显示武器光效）
        stren7_light = 0,   % 防具光效 （编号1-6，如果为0表示没有换装过，则用stren7_num字段显示防具光效）
        prefix_num = 0,     % 全身装备前缀数
        level_num = 0,      % 全身装备等级数
        super_stren_num = 0,     % 全身装备至尊数
        darkgold_num = 0,   % 全身红玉数
        leader = 0,          % 是否队长
        sex = 0,
        pk_status = 0,       % pk状态：和平模式 0，全体模式 1， 国家模式 2， 帮派模式 3，队伍模式 4， 善恶模式 5
        pk_value = 0,        % 玩家pk值(白名:[0,100]，黄名:(100-200]，红名:200+)
        vip_type = 0,        % Vip类型（0非vip、1黄金vip、2白金vip、3紫金vip）
        %% VIP特权
        vip_lv = 0,                 % VIP特权等级
        vip_expire_time = 0,        % VIP特权过期时间
        line = 0,
        biao_lv = 0,         % 镖车镖旗等级 0:没有镖车镖旗
        biao_color = 0,      % 镖车颜色
        sit_down = 0,                   % 打坐
        sit_role = 0,                   % 双修角色ID
        arena_group = 0,                % 竞技场分组
        arena_angry = 0,                % 竞技场怒气
        arena_rank = 0,                 % 竞技场名次
        guild_battle_stone_flag = 0,    % 帮战神石标记
        siege_battle_group = 0,         % 攻城战阵营
        siege_battle_attack_group_flag = 0, % 攻城战攻方阵营标记
        platform = "",                  % 平台标识
        server_id = 0,                  % 运营服ID
        achieved_name_list = [],        % 称号
        winetype = 0,                   % 醉酒状态
        image = 0,                      % 角色头像
        holding_wedding = 0,            % 角色是否正在举行婚礼
        spouse = #spouse_info{},        % 配偶
        wearing_ring = 0,               % 是否佩戴婚戒
        couple_intimacy_first = false,
        cruise_mode = 0,                % 巡游模式
        cruise_status = 0,              % 巡游状态
        figure = 0,                     % 玩家变身形象
        fly_mount = 0,                  % 飞行坐骑id
        kfz_league_team_id = "",        % 跨服联赛队伍ID
        kfz_league_team_name = "",      % 跨服联赛队伍名称
        kfz_3v3_angry = 0,              % 3v3战场怒气
        kfz_medal_attribute = [],       % 跨服勋章铸魂属性
        kfz_medal_skill = [],           % 跨服勋章铸魂技能
        fly_skill = 0                   % 是否学习轻功
        ,wing_id = 0                        % 翅膀ID
        ,wing_figure = 0                    % 翅膀形像
        ,hide_wing_figure = 0               % 是否隐藏翅膀形像，1为隐藏
        ,wing_status = 0                    % 翅膀状态，0卸下状态，1出战状态，2飞行状态
        ,wing_stren = 0                     % 翅膀强化数
        ,dress_degree = 0 % 着装度
        ,fashion_armor_figure = 0                      % 穿戴的衣服时装形象ID，为0时为不穿戴时装形象
        ,fashion_weapon_figure = 0                     % 穿戴的武器时装形象ID，为0时为不穿戴时装形象
        ,fashion_accessory_figure = 0                  % 穿戴的饰品时装形象ID，为0时为不穿戴时装形象
        ,invisible = 0
        ,warrior_id = 0             % 官阶
        ,warrior_wen = 0            % 官职 -- 文职
        ,warrior_wu = 0             % 官职 -- 武职
        ,warrior_war = 0            % 官职 -- 国战官职
        ,warrior_kfz_terr = 0       % 官职 -- 跨服领土战官职
        ,ring_num = 0               % 婚戒数量
        ,pet_fly_lv = 0                     % 宠物飞行器等级
        ,mount_footprint = 0 % 坐骑足迹
        ,fly_egg_figure = 0                 % 飞行坐骑形象
        ,fly_egg_perfect = 0                % 飞行坐骑完美度
    }).

-record(ets_online, {
        id = 0,
        nickname = none,
        pid = 0,
        sid = 0,
        career=0,
        realm = 0,          % 阵营 秦、楚、汗
        scene = 0,
        scene_res_id = 0,       % 这里是资源id
        x = 0,
        y = 0,
        hp = 0,
        hp_lim = 0,
        mp = 0,
        mp_lim = 0,
        att = 0,             % 攻击
        def = 0,             % 防御
        hit = 0,             % 命中率
        dodge = 0,           % 躲避
        crit = 0,            % 暴击
        ten = 0,             % 坚韧
        fire = 0,            % 火
        ice = 0,             % 冰
        drug = 0,            % 毒
        lv = 1,
        guild_id = 0,        % 帮派ID
        guild_name = [],     % 帮派名称
        guild_position = 0,  % 帮派职位
        guild_quit_lasttime = 0, % 帮派上次退出时间
        pet_figure = 0,      % 宠物形象
        pet_name = [],       % 宠物名称
        pet_level = 0,       % 宠物等级
        pet_aptitude = 0,    % 宠物资质
        pet_growth = 0,      % 宠物成长
        pet_attack_mode = 0, % 宠物攻击模式
        pid_dungeon = none,  % 副本进程
        speed = 0,           % 移动速度
        pid_team = 0,        % 组队进程
        mount_figure = 0,    % 坐骑形像
        mount_equip = [0,0], % 坐骑装备, [装备一，装备二]
        equip_current = [0,0,0,0,0,0],      % 当前装备类型ID - [武器, 衣服, 无双神兵, 武器强化数, 衣服强化数, 武器强化至尊加成]
        fashion_weapon = [0,0],             % 穿戴的武器时装 - [武器时装类型ID，武器时装强化数]
        fashion_armor = [0,0],              % 穿戴的衣服时装 - [衣服时装类型ID，衣服时装强化数]
        fashion_accessory = [0,0],          % 穿戴的饰品时装 - [饰品时装类型ID，饰品时装强化数]
        fashion_head = [0,0],               % 穿戴的头饰时装 - [头饰时装类型ID，头饰时装强化数]
        fashion_foot = [0,0],               % 穿戴的足饰时装 - [足饰时装类型ID，足饰时装强化数]
        fashion_chat = [0,0],               % 穿戴的聊天弹框时装 - [聊天弹框时装类型ID，聊天弹框时装强化数]
        hide_fashion_weapon = 0,            % 是否隐藏武器时装，1为隐藏
        hide_fashion_armor = 0,             % 是否隐藏衣服时装，1为隐藏
        hide_fashion_accessory = 0,         % 是否隐藏饰品时装，1为隐藏
        hide_fashion_head = 0,              % 是否隐藏头饰时装，1为隐藏
        hide_fashion_foot = 0,              % 是否隐藏足饰时装，1为隐藏
        hide_fashion_chat = 0,              % 是否隐藏聊天弹框时装，1为隐藏
        suit_id = 0,        % 是否套装
        stren7_num = 0,     % 全身装备加七强化数
        weapon_light = 0,   % 武器光效 （编号1-6，如果为0表示没有换装过，则用equip_curent字段的第四个值显示武器光效）
        stren7_light = 0,   % 防具光效 （编号1-6，如果为0表示没有换装过，则用stren7_num字段显示防具光效）
        prefix_num = 0,     % 全身装备前缀数
        level_num = 0,      % 全身装备等级数
        super_stren_num = 0,     % 全身装备至尊数
        darkgold_num = 0,   % 全身红玉数
        leader = 0,          % 是否队长
        sex = 0,
        battle_status = [],  % 战斗状态
        pk_status = 0,       % pk状态：和平模式 0，全体模式 1， 国家模式 2， 帮派模式 3，队伍模式 4， 善恶模式 5
        goods_pid = 0,       % 物品进程,
        pk_value = 0,        % 玩家pk值(白名:[0,100]，黄名:(100-200]，红名:200+)
        pay_gold = 0,        % 充值总元宝数
        vip_type = 0,        % Vip类型（0非vip、1黄金vip、2白金vip、3紫金vip）
        %% VIP特权
        vip_lv = 0,          % VIP特权等级
        vip_expire_time = 0,        % VIP特权过期时间
        biao_lv = 0,         % 镖车等级(0 没有镖车, 30,40,50,60... 镖车等级)
        yunbiao_pt = 0,      % 运镖保护时间
        biao_color = 0,      % 镖车颜色
        sit_down = 0,        % 打坐
        sit_role = 0,        % 双修角色ID
        water_end_time = 0,  % buff温泉到期时间
        reborn_time    = 0,  % 复活时间
        arena_match_time = 0,       % 竞技场比赛时间
        arena_group = 0,            % 竞技场分组
        arena_angry = 0,            % 竞技场怒气
        arena_rank = 0,             % 竞技场排名
        guild_battle_flag = 0,      % 帮战参赛标记
        guild_battle_stone_flag = 0,% 帮战神石标记
        siege_battle_group = 0,     % 攻城战阵营
        siege_battle_attack_group_flag = 0, % 攻城战攻方阵营标记
        platform = "",              % 平台标识
        server_id = 0,              % 运营服ID
        kfz_score = 0,              % 跨服战积分
        kfz_score_week = 0,         % 跨服战周积分
        kfz_3v3_apply_lasttime = 0, % 跨服3v3最后报名时间
        kfz_league_team_id = "",    % 跨服联赛队伍ID
        kfz_league_team_name = "",  % 跨服联赛队伍名称
        achieved_name_list = [],    % 称号
        winetype = 0,               % 醉酒状态
        honour = 0,                 % 远征岛荣誉

        td_floor = 0,               % 本周剩余次数
        td_pid = 0,                 % 塔防pid
        td_s_wave = 0,              % 单人塔防波数
        td_t_wave = 0,              % 多人塔防波数
        td_data = 0,                % 塔防波数有效日期
        image = 0,                  % 角色头像
        is_pra = 0,                 % 玩家是否在离线修炼中
        last_login_ip = [],         % 玩家登录IP
        holding_wedding = 0,        % 角色是否正在举行婚礼
        spouse = #spouse_info{},    % 配偶
        wearing_ring = 0,           % 是否佩戴婚戒
        couple_intimacy_first = false,
        cruise_mode = 0,            % 巡游模式
        cruise_status = 0,          % 巡游状态
        figure = 0,                 % 玩家变身形象
        fly_mount = 0,              % 飞行坐骑id
        combat_power = 0,           % 战斗力
        national_buff_info = [],    % 国家BUFF信息
        is_pay = false,             % 是否充值
        master_join_lasttime = 0,   % 最后拜师时间
        friend_num = 0,             % 登录时好友数

        kfz_3v3_angry = 0,          % 3v3跨服怒气

        kfz_medal_active = 0,       %3v3勋章激活
        kfz_medal_lv = 1,           %3v3勋章等级
        kfz_medal_exp = 0,          %3v3勋章经验
        kfz_medal_win = 0,          %3v3竞技场胜利次数
        kfz_medal_lose = 0,         %3v3竞技场失败次数

        kfz_medal_type = 0,         % 跨服战勋章类型(0屁民勋章、1武斗勋章、2武师勋章、3武圣勋章)
        kfz_medal_attribute = [],   % 跨服战勋章属性
        kfz_medal_skill = [],       % 跨服战勋章技能
        kfz_team_id = 0,            % 跨服战战队ID
        fly_skill = 0               % 是否学习轻功

        ,accname = ""               % 平台帐户

        ,wing_id = 0                        % 翅膀ID
        ,wing_figure = 0                    % 翅膀形像
        ,hide_wing_figure = 0               % 是否隐藏翅膀形像，1为隐藏
        ,wing_status = 0                    % 翅膀状态，0卸下状态，1出战状态，2飞行状态
        ,wing_stren = 0                     % 翅膀强化数

        ,dress_degree = 0                   % 着装度
        ,fashion_armor_figure = 0           % 穿戴的衣服时装形象ID，为0时为不穿戴时装形象
        ,fashion_weapon_figure = 0          % 穿戴的武器时装形象ID，为0时为不穿戴时装形象
        ,fashion_accessory_figure = 0       % 穿戴的饰品时装形象ID，为0时为不穿戴时装形象
        ,invisible = 0                      % 是否隐身
        ,warrior_id = 0             % 官阶
        ,warrior_wen = 0            % 官职 -- 文职
        ,warrior_wu = 0             % 官职 -- 武职
        ,warrior_war = 0            % 官职 -- 国战官职
        ,warrior_kfz_terr = 0       % 官职 -- 跨服领土战官职
        ,ring_num = 0               % 婚戒数量
        ,pet_fly_lv = 0                     % 宠物飞行器等级
        ,mount_footprint = 0 % 坐骑足迹
        ,fly_egg_figure = 0                 % 飞行坐骑形象
        ,fly_egg_perfect = 0                % 飞行坐骑完美度
    }).

%% 场景数据结构
-record(ets_scene,
    {
        id = 0,              %% 场景唯一ID
        sid = 0,             %% 资源id
        name = <<>>,         %% 场景名称
        type = 0,            %% 场景类型(0:安全场景, 1:野外场景, 2:副本场景)
        x = 0,               %% 默认开始点
        y = 0,               %% 默认开始点
        elem=[],             %% 场景元素
        requirement = [],    %% 进入需求
        mask = "",
        npc = [],
        mon = [],
        safe
    }
).

%% 场景怪物AI
-record(ets_ai,
    {
        id = {0,0,0},        %% 坐标
        scene = 0,           %% 场景
        data = []
    }
).

%% 场景怪物 - 任务用
-record(ets_scene_mon,
    {
        id = 0,              %% 怪物ID
        sid = 0,             %% 资源id
        name = <<>>,         %% 场景名称
        mname = <<>>,        %% 怪物名字
        kind = 0,            %% 怪物类型
        x = 0,
        y = 0
    }
).

-record(ets_mon, {
        id = 0,
        name = <<>>,
        kind = 0,        %%0 怪物，1采集物品，2旗子，3矿点
        boss = 0,        %%0普通怪，1野外BOSS，2宠物BOSS，3世界BOSS，4帮派BOSS，5副本BOSS，6爬塔BOSS，7塔防BOSS
        career = 0,
        auto,            %% 0主动，1自动生成
        scene,           %%所属场景唯一
        scene_id,        %%场景资源id
        mid,
        icon = 0,        %% 资源
        lv,
        hp = 0,
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
        fire = 0,        %% 火
        ice = 0,         %% 冰
        drug = 0,        %% 毒
        unyun = 0,       %% 抗晕
        unbeatback = 0,  %% 抗击退
        uncm = 0,        %% 抗沉默
        skill = [],      %% 技能
        skill_pro = [],  %% 被动技能
        talent_skill = [],       % 天赋技能，(用于分身)
        talent_kind = 0,         % 天赋系，(用于分身)
        kfz_league_team_id = "", % 跨服联赛队伍ID(用于分身)
        arena_group = 0,         % 竞技场分组(用于分身)
        pk_status = 0,           % pk状态：和平模式 0，全体模式 1， 国家模式 2 帮派模式 3，队伍模式 4， 善恶模式 5(用于分身)
        guild_id = 0,            % 帮派ID(用于分身)
        pid_team = 0,            % 组队进程(用于分身)
        pk_value = 0,            % 玩家pk值 
        att_area,        %% 攻击范围
        trace_area,      %% 追踪范围
        beat_back,       %% 击退
        x = 0,
        y = 0,
        d_x,             %% 默认出生X
        d_y,             %% 默认出生y
        aid = none,      %% 怪物活动进程
        %bid = none,     %% 战斗进程
        retime,          %% 重生时间
        type = 0,        %% 怪物战斗类型（0被动，1主动）
        exp=0,           %% 怪物经验
        llpt=0,          %% 怪物掉落历练声望
        coin=0,          %% 怪物掉落铜钱
        drop_goods,      %% 怪物可掉落物品[{Goodsid1, DropRate1}, {Goodsid2, DropRate2}, ...]
        battle_status = [],   %% 战斗状态
        att_type = 0,    %% 0近战，1远程
        link_node = none,
        line = 0,
        drop = 0,       %% 掉落方式
        drop_ratio = 1, %% 掉落系数
        owner_id_list    = [],  %% 怪物所属（用于掉落）
        attack_target    = 0,   %% 攻击目标（玄武副本怪物追踪）
        last_attack_role = 0,   %% 最后进行攻击的玩家
        out = 0,        %% 副本排除
        sgroup = 0,     %% 怪物阵营id: 帮派类型为帮派ID、争夺战为阵营ID
        realm = 0,      %% 镖车所属国家
        path = [],      %% 自动行走路径
        cruise = [],    %% 迎亲巡游属性
        tree_owner = 0, %% 拥有者（摇钱树）
        health = 0,     %% 健康状态（摇钱树）
        color = 0,      %% 颜色属性
        hp_judge = [],  %% 血量判断
        stage = [],     %% 爆发阶段
        trans = 0,      %% 变身
        combin_skill = [], %% 组合技能
        rand_att = 0,   %% 是否随机攻击
        event = [],     %% 怪物曾经发送过的特殊事件列表
        mon_own = 0,    %% 怪物所属玩家（奥运圣火活动特加）
        mon_own_name ="",%% 怪物所属玩家名称（奥运圣火活动特加）
        siege_battle_car_num = 0     %% 攻城车编号
    }).

-record(ets_npc, {
        id = 0,
        nid = 0,
        func = 0,         %% 功能
        icon = 0,         %% 资源
        name,
        scene,
        x,
        y,
        talk,
        realm = 0
    }).

%%物品类型记录
-record(ets_goods_type, {
        goods_id,           %% 物品类型Id
        goods_name,         %% 物品名称
        type,               %% 物品类型, 1 装备类， 2 增益类，3 任务类 4 坐骑类
        subtype,            %% 物品子类型，
                            %% 装备子类型：1 武器，2 衣服，3 头盗，4 手套，5 鞋子，6 项链，7 戒指
                            %% 增益子类型：1 药品，2 经验
                            %% 坐骑子类型：1 一人坐骑 2 二人坐骑 3 三人坐骑
        equip_type=0,       %% 装备类型：0为无，1为武器，2为防具，3为饰品
        price_type=1,       %% 价格类型：1 铜钱, 2 银两，3 金币，4 绑定的铜钱
        price=0,            %% 购买价格
        sell_price=0,       %% 出售价格
        bind=0,             %% 是否绑定，0为不可绑定，1为可绑定还未绑定，2为可绑定已绑定
        trade=0,            %% 是否交易，1为不可交易，0为可交易
        sell=0,             %% 是否出售，1为不可出售，0为可出售
        isdrop=0,           %% 是否丢弃，1为不可丢弃，0为可丢弃
        soul=0,             %% 是否有灵魂属性，1为有，0为无
        level=0,            %% 等级限制
        career=0,           %% 职业限制，0为不限
        sex=0,              %% 性别限制，0为不限，1为男，2为女
        job=0,              %% 职位限制，0为不限
        nobility=0,         %% 爵位限制
        xwpt_limit=0,       %% 修为声望限制
        forza_limit=0,      %% 力量需求，0为不限
        wit_limit=0,        %% 智力需求，0为不限
        agile_limit=0,      %% 敏捷需求，0为不限
        vitality = 0,       %% 体力
        spirit = 0,         %% 灵力
        hp = 0,             %% 基础属性 - 血量
        mp = 0,             %% 基础属性 - 内力
        forza=0,            %% 基础属性 - 力量
        wit=0,              %% 基础属性 - 灵力
        agile=0,            %% 基础属性 - 身法
        thew=0,             %% 基础属性 - 体质
        att=0,              %% 基础属性 - 攻击
        def=0,              %% 基础属性 - 防御
        hit = 0,            %% 基础属性 - 命中
        dodge = 0,          %% 基础属性 - 躲避
        crit = 0,           %% 基础属性 - 暴击
        ten = 0,            %% 基础属性 - 坚韧
        fire = 0,           %% 基础属性 - 火抗
        ice = 0,            %% 基础属性 - 冰抗
        drug = 0,           %% 基础属性 - 毒抗
        speed=0,            %% 基础属性 - 速度
        attrition=0,        %% 耐久度，0为永不磨损
        suit_id=0,          %% 套装ID，0为无
        skill_id=0,         %% 技能ID，0为无
        max_hole=0,         %% 最大镶孔数
        max_stren=0,        %% 最大强化等级
        max_quality=0,      %% 最大品质数
        max_overlap=0,      %% 可叠加数，0为不可叠加
        color,              %% 物品颜色，0 白色，1 绿色，2 蓝色，3 紫色，4 橙色
        expire_time=0,      %% 有效期，0为无
        addition=[],        %% 附加属性列表，[{属性类型ID, 属性值}, ...]
        reply_num=0,        %% 气血和内力的单次回复量
        scene_limit=[],     %% 场景限制
        search = 0          %% 是否可搜索，0不可搜，1可搜
    }).

%%装备类型附加属性表
-record(ets_goods_add_attribute, {
        id,             %% 编号
        goods_id,       %% 物品类型ID
        attribute_id,   %% 属性类型Id
        value_type,     %% 属性值类型
        min_value,      %% 最小值
        max_value       %% 最大值
    }).

%%装备效果表
-record(ets_goods_effect, {
        goods_id=0,         %% 物品类型ID
        %exp=0,              %% 经验
        %coin=0,             %% 铜钱
        %bcoin=0,            %% 绑定铜钱
        %llpt=0,             %% 历练声望
        %xwpt=0,             %% 修为声望
        %arena=0,            %% 竞技积分
        %battle_score=0,     %% 帮派战功
        %honour=0,           %% 荣誉
        %bag_num=0,          %% 格子数
        %time=0,             %% 时长
        %fashion=[],         %% 时装转换
        buf_type=0,         %% BUFF类型
        buf_attr=0,         %% BUFF属性ID
        buf_val=0,          %% BUFF属性值
        buf_time=0,         %% BUFF时长
        buf_scene = [],     %% BUFF场景限制
        effect_type=0,      %% 效果类型
        effect_value=0      %% 效果数值
    }).

%%商店表
-record(ets_shop, {
        id,             %% 编号
        shop_type=0,    %% 商店类型，1为商城，2为武器店，3为防具店，5为杂货店
        shop_subtype=0, %% 商店子类型，如商城的子类：1 新品上市，9 限时热卖
        goods_id=0,     %% 物品类型ID
        goods_num=0,    %% 物品出售数量，只对限时热卖有效
        new_price=0     %% 新价
    }).

%%限购商店表
-record(ets_shop_limit, {
        id,                 %% 编号
        type=0,             %% 商店类型
        goods_id=0,         %% 物品类型ID
        goods_num=0,        %% 物品数量
        price_type=0,       %% 价格类型，1元宝
        price_org=0,        %% 原价
        price_new=0,        %% 新价
        price_list=[],      %% 价格列表
        is_refresh=0,       %% 零点是否刷新
        limit_id=0,         %% 限购ID
        limit_num=0,        %% 限购数量
        open_start=0,       %% 开服限制开始（天数）
        open_end=0,         %% 开服限制结束（天数）
        activity_start=0,   %% 活动开始时间
        activity_end=0      %% 活动结束时间
    }).

%%礼包配置表
-record(ets_gift, {
        id = 0,                 %% 礼包ID
        name = <<>>,            %% 礼包名称
        goods_id = 0,           %% 物品类型ID
        give_way = null,        %% 发放方式
        get_way = null,         %% 领取方式
        give_obj = [],          %% 发放对象
        get_delay = 0,          %% 领取延迟
        gift_rand = 0,          %% 礼包随机物品个数，0为固定，1为随机，2为列表随机
        gifts = [],             %% 礼包内容
        bind = 0,               %% 绑定状态
        is_notice = 0,          %% 是否公告
        start_time = 0,         %% 开始时间
        end_time = 0,           %% 结束时间
        status = 0              %% 状态
    }).

%%活动礼包配置表
-record(ets_gift2, {
        id = 0,                 %% 活动礼包ID
        name = <<>>,            %% 活动礼包名称
        type = 0,               %% 组合类型
        url = <<>>,             %% 活动跳转URL
        bind=0,                 %% 绑定状态
        lv=0,                   %% 等级需求
        coin=0,                 %% 铜钱
        bcoin=0,                %% 绑定铜钱
        gold=0,                 %% 元宝
        silver=0,               %% 绑定元宝
        goods_list = [],        %% 礼包内容
        desc = <<>>,            %% 礼包描述
        is_show = 1,            %% 是否显示礼包内容
        time_start=0,           %% 活动开始时间
        time_end=0,             %% 活动结束时间
        status=0                %% 状态
        ,md5_type=0             %% 加密类型
    }).

%%宝箱配置
-record(ets_box, {
        id=0,                   %% 宝箱ID
        name = <<>>,            %% 宝箱名称
        price=0,                %% 开1次宝箱价格
        price2=0,               %% 开10次宝箱价格
        price3=0,               %% 开50次宝箱价格
        base_goods=[],          %% 保底物品
        guard_num=0,            %% 保护几率
        ratio=0,                %% 机率范围
        high_box=0,             %% 高级物品全服限制
        high_player=0,          %% 高级物品个人限制
        goods_list=[]           %% 物品列表
    }).

%%宝箱物品配置
-record(ets_box_goods, {
        id=0,                   %% 编号
        box_id=0,               %% 宝箱ID
        goods_id=0,             %% 物品ID
        type=0,                 %% 类型，0 普通物品，1 高级物品
        bind=0,                 %% 物品绑定状态，0 不绑定，2 已绑定
        notice=0,               %% 通告类型，0 普通，1 全服
        ratio=0,                %% 机率范围
        ratio_start=0,          %% 机率开始值
        ratio_end=0,            %% 机率结束值
        lim_box=0,              %% 全服限制，至少N小时才能掉一个
        lim_player=0,           %% 个人限制，至少N秒才能掉一个
        lim_num=0,              %% 次数限制，至少N次才能掉一个
        lim_career=0            %% 职业限制
    }).
%
%%% 宝箱包裹
%-record(ets_box_bag, {
%        pid = 0,                %% 角色ID
%        goods_list = []         %% 物品列表，格式：[ { 物品ID, 物品数量, 绑定 }, ... ]
%    }).

%% 开宝箱全局计数
-record(ets_box_counter, {
        box_id=0,               %% 宝箱ID
        count=0,                %% 开箱计数
        limit_goods=[],         %% 限制物品，格式：[ { 物品ID, 限制时间 }, ... ]
        count_new=0,            %% 新远征宝箱计数
        limit_goods_new=[]      %% 新远征限制物品
    }).

%% 开宝箱单玩家计数
-record(ets_box_player_counter, {
        pid=0,                  %% 玩家ID
        box_id=0,               %% 宝箱ID
        count=0,                %% 开箱计数
        guard=0,                %% 是否已使用保护几率，0 未保护过，1 已保护过
        limit_goods=[],         %% 限制物品，格式：[ { 物品ID, 限制时间, 限制次数 }, ... ]
        count_new=0,
        guard_new=0,
        limit_goods_new=[]
    }).


%% 任务数据
-record(task,
    {
        id
        ,role_id
        ,name = <<"">>
        ,desc = <<"">>			%% 描述

        %% 部分限制条件
        ,class = 0              %% 任务分类，0普通任务，1运镖任务，2帮会任务
        ,type = 0				%% 类型
        ,kind = 0				%% 种类
        ,level = 1				%% 需要等级
        ,repeat = 0				%% 可否重复
        ,realm = 0              %% 阵营
        ,career = 0				%% 职业限制
        ,prev = 0				%% 上一个必须完成的任务id
        ,proxy = 0              %% 是否可以委托
        ,transfer = 0           %% 是否传送
        ,start_item = []		%% 开始获得物品{ItemId, Number}
        ,end_item = []			%% 结束回收物品

        ,start_npc = 0			%% 开始npcid
        ,end_npc = 0			%% 结束npcid
        ,start_talk = 0		    %% 开始对话
        ,end_talk = 0			%% 结束对话
        ,unfinished_talk = 0 	%% 未完成对话

        ,condition = []			%% 条件内容	[{task, 任务id}, {item, 物品id, 物品数量}]
        ,content = []			%% 任务内容 [[State, 1, kill, NpcId, Num, NowNum], [State, 0, talk, NpcId, TalkId], [State, 0, item, ItemId, Num, NowNum]]
        ,state = 0      		%% 完成任务需要的状态值 state = length(content)

        %% 任务奖励
        ,exp = 0				%% 经验
        ,coin = 0				%% 金钱
        ,binding_coin = 0       %% 绑定金
        ,spt = 0                %% 灵力
        ,llpt = 0           % 历练声望
        ,xwpt = 0           % 修为声望
        ,fbpt = 0           % 副本声望
        ,bppt = 0           % 帮派声望
        ,gjpt = 0           % 国家声望
        ,attainment	= 0			%% 修为
        ,contrib = 0			%% 贡献
        ,guild_exp = 0			%% 帮会经验

        ,award_select_item_num = 0%% 可选物品的个数
        ,award_item = []		%% 奖励物品
        ,award_select_item = [] %% 奖励可选物品
        ,award_gift = []		%% 礼包奖励

        ,start_cost = 0         %% 开始时是消耗铜币
        ,end_cost = 0 			%% 结束时消耗游戏币
        ,next = 0				%% 结束触发任务id
        ,next_cue = 0           %% 是否弹出结束npc的对话框
        ,proxy_time = 0
        ,proxy_gold = 0
        ,fin_gold = 0
        ,cumulate = 0           %% 是否经验累积
    }
).

%% 角色任务记录
-record(role_task,
    {
        id,
        role_id=0,
        task_id=0,
        type = 0,
        kind = 0,
        trigger_time=0,
        state=0,
        end_state=0,
        mark=[]        %%任务记录器格式[State=int(),Finish=bool(), Type=atom((), ...]
    }
).

%% 角色任务历史记录
-record(role_task_log,
    {
        role_id=0,
        task_id=0,
        type = 0,
        trigger_time=0,
        finish_time=0
    }
).

%% 任务条件数据
-record(task_condition,
    {
        id
        ,type = 0
        ,kind = 0
        ,level = 1				%% 需要等级
        ,repeat = 0				%% 可否重复
        ,realm = 0              %% 阵营
        ,career = 0				%% 职业限制
        ,prev = 0				%% 上一个必须完成的任务id
        ,condition = []         %% 扩充条件	TODO 具体描述日后再加
    }
).

%% 自动完成的任务记录
-record(role_task_auto,
    {
        id              = {0, 0}
        ,role_id        = 0
        ,task_id        = 0
        ,type           = 0
        ,name           = <<>>
        ,number         = 0     %% 委托次数
        ,gold           = 0     %% 需要元宝
        ,trigger_time   = 0     %% 触发时间
        ,finish_time    = 0     %% 完成时间
        ,exp            = 0	    %% 经验
        ,llpt           = 0     %% 历练声望
        ,xwpt           = 0     %% 修为声望
        ,fin_gold       = 0     %% 立即完成元宝
    }
).

%%关系列表
-record(ets_rela,
    {
        id = 0,             %% 记录id
        idA = 0,            %% 角色A的id
        idB = 0,            %% 角色B的id
        rela = 0,           %% 与B的关系(0:没关系1:好友2:黑名单3:仇人)
        intimacy = 0,       %% 亲密度
        group = 1,          %% B所在分组
        closely = 0,        %% 是否为密友
        location = 0,       %% 是否显示位置
        killed_by_enemy = 0,%% 被仇人杀死多少次
        location_time = 0,  %% 显示位置到期时间
        gift = 0            %% A曾经赠送给B的祝福礼包(1,2,3)
    }
).

%% 好友分组列表
-record(ets_rela_group,
    {
        id = 0,         %% 玩家id
        group = []      %% 好友分组
    }).

%%好友资料
-record(ets_rela_info,
    {
        id = 0,                  %%角色id
        nickname = [],           %%角色名字
        sex = 0,                 %%角色性别
        lv = 0,                  %%角色等级
        career = 0,              %%角色职业
        online_flag = 0,         %%在线标志
        vip = 0,                 %%vip类型
        realm = 0,               %%国家
        image = 0,               %%角色头像
        last_login_time = 0      %%最近登录时间
    }).

%%角色的初始好友分组
-record(ets_rela_set,
    {
        id = 0,	            %%角色A的id
        name1 = <<"分组1">>,    %%三个分组的名字 {name1, name2, name3}
        name2 = <<"分组2">>,
        name3 = <<"分组3">>
    }
).

%% 邮件
-record(mail, {
        id,                 %% 邮件Id
        type,               %% 邮件类型
        state,              %% 邮件状态
        locked,             %% 锁定状态(1锁定/2未锁)
        timestamp,          %% 邮件时间戳
        sid,                %% 发件人Id
        sname = <<>>,       %% 发件人名字 (binary())，第一次读信时加载
        slv = 0,            %% 发件人等级，第一次读信时加载
        uid,                %% 收件人Id
        title,              %% 标题 (binary())
        content = <<>>,     %% 信件内容 (binary())，第一次读信时加载
        links = [],         %% 网址信息( [{LinkId, LinkName, GameLink}, ...] )，第一次读信时生成
        goods_id,           %% 物品标识（无物品为0）
        id_type,            %% 标识的类型（0物品Id/1类型Id）
        bind = 0,           %% 是否绑定（当id_type为1时有效）
        stren = 0,          %% 强化等级（当id_type为1时有效）
        prefix = 0,         %% 前缀（当id_type为1时有效）
        goods_type_id,      %% 物品类型ID（无物品为0，客户端显示图标需要）
        goods_num,          %% 物品数量
        bcoin,              %% 绑定铜币
        coin,               %% 铜币
        silver,             %% 绑定元宝
        gold,               %% 元宝
        platform = <<>>,
        server_id = 0,
        pay_activated = 0   %% 领取触发充值
    }).

%%技能
-record(ets_skill, {
        id=0,
        skill_id = 0,     % 技能id
        role_id = 0,      % 用户id
        name = <<>>,
        desc = <<>>,
        career = 0,       % 职业
        type = 0,         % 主，被，辅
        obj = 0,          % 释放目标
        mod = 0,          % 单体还是全体
        area = 0,         % 攻击范围，格子数
        cd = 0,           % CD时间
        lastime = 0,      % 持续时间
        attime = 0,       % 攻击次数，如攻击2次
        attarea = 0,      % 攻击距离
        limit = [],       % 限制使用的技能有
        hate = 0,         % 仇恨值
        pro = 0,          % 附加成功概率
        pet_skill_flag=0, % 宠物技能标识
        energy = 0,       % 增加能量值
        data = []
    }).

%%天赋
-record(ets_talent, {
        pid = 0,          % 所属技能id
        skill_id = 0,     % 技能id
        name = <<>>,
        kind = 0,         % 派系
        career = 0,       % 职业
        type = 0,         % 主，被，辅
        lastime = 0,      % 持续时间
        data = []
    }).

%% 天赋相关参数
-record(talent_state, {
        talent = 0,                 % 天赋等级
        talent_last = 0,            % 剩余天赋值
        talent_kind_1 = 0,          % 天赋值派系1
        talent_kind_2 = 0,          % 天赋值派系2
        tal_exp = 0,                % 天赋经验
        tal_exp_limit = 0,          % 天赋经验上限
        kind = 0                    % 选择的天赋系
    }).

%%##########OTHER#################
%%记录用户一些常用信息
-record(player_status, {
        id = 0,             % 用户ID
        %accid = 0,         % 平台ID
        accname = [],       % 平台账号
        nickname = none,    % 玩家名
        c_rename = 0,       % 是否可以改名，0不可以，1可以
        reg_time = 0,
        last_login_time = 0,% 最后登陆时间
        sex = 0,            % 性别 1男 2女
        career = 0,         % 职业 1，2，3（分别是昆仑（战士），逍遥（法师），唐门（刺客））
        realm = 0,          % 阵营 秦、楚、汉
        realm_ctime = 0,    % 上次转换阵营时间
        llpt = 0,           % 历练声望
        xwpt = 0,           % 修为声望
        fbpt = 0,           % 副本声望
        bppt = 0,           % 帮派声望
        gjpt = 0,           % 国家声望
        mlpt = 0,           % 魅力声望
        wage = 0,           % 工资
        spirit = 0,         % 灵力
        jobs = 1,           % 职位
        nobility = 0,       % 爵位
        pid = none,         % process id
        gold = 0,           % 元宝
        silver = 0,         % 绑定元宝
        coin = 0,           % 铜钱
        bcoin = 0,          % 绑定铜钱
        point = 0,          % 返利的积分
        rebate = 0,         % 还未返利的元宝数
        pay_gold = 0,       % 充值总元宝数
        pay_lv = 0,         % 充值RMB等级
        scene = 0,          % 场景ID,
        scene_res_id = 0,   % 场景资源id
        x = 0,
        y = 0,
        lv = 1,             % 等级
        hp = 0,
        mp = 0,
        hp_lim = 0,
        mp_lim = 0,
        forza = 0,          % 力量
        agile = 0,          % 身法
        wit = 0,            % 灵力
        thew = 0,           % 体质
        att = 0,            % 攻击
        def = 0,            % 防御
        hit = 0,            % 命中率
        dodge = 0,          % 躲避
        crit = 0,           % 暴击
        ten = 0,            % 坚韧
        fire = 0,           % 火
        ice = 0,            % 冰
        drug = 0,           % 毒
        combat_power = 0,   % 战斗力
        last_combat_power=0,        % 排行使用
        base_attribute = [],        % 基础一级属性[力量, 身法, 灵力, 体质]
        skill_attribute = [],       % [att, crit, hp, mp]
        active_skill_attribute = [],      % [{hpR, time}]
        meridian_attribute = [],    % 经脉加成属性
        equip_attribute=[],         % 装备加成属性列表[Hp, Mp, Att, Def, Hit, Dodge, Crit, Ten]
        chengjiu_attribute=[],      % 成就加成属性列表
        mount_attribute = [],       % 坐骑加成属性列表
        warrior_id = 0,             % 官阶
        warrior_wen = 0,            % 官职 -- 文职
        warrior_wu = 0,             % 官职 -- 武职
        warrior_war = 0,            % 官职 -- 国战官职
        warrior_kfz_terr = 0,       % 官职 -- 跨服领土战官职
        warrior_attribute = [],     % 官职加成属性列表
        couple_ring_attribute = [], % 婚戒加成属性列表
        pet_id = 0,         % 宠物ID
        pet_figure = 0,     % 宠物形象
        pet_name = [],      % 宠物名称
        pet_level = 0,      % 宠物级别
        pet_aptitude = 0,   % 宠物资质
        pet_attribute=[],   % 宠物的加成[HpLim, MpLim, Att, Def, Hit, Dodge, Crit, Ten]
        pet_skill_attribute=[],     % 宠物技能的加成[HpLimPercent, MpLimPercent, AttPercent, DefPercent, HitPercent, DodgePercent, CritPercent, TenPercent]
        pet_manual_attribute=[],    % 宠物图鉴的加成[HpLim, MpLim, Att, Def, Hit, Dodge, Crit, Ten, Forza, Agile, Wit, Thew, Fire, Ice, Drug]
        pet_potential_attribute=[], % 宠物潜能的加成[HpLim, MpLim, Att, Def, Hit, Dodge, Crit, Ten, Forza, Agile, Wit, Thew, Fire, Ice, Drug]
        pet_growth = 0,             % 宠物成长
        pet_equip_active_skills=[], % 宠物已装备主动技能
        pet_strength = 0,   % 宠物体力值
        pet_attack_mode = 0,% 宠物攻击模式
        pet_active_skill_flag = 0,  % 宠物主动技能标识
        buff_attribute=[],  % BUFF属性列表 [HpRatio, MpRatio, AttRatio, DefRatio, HitRatio, DodgeRatio, CritRatio, TenRatio]
        att_area = 0,       % 攻击距离
        att_speed = 0,      % 攻击速度
        pri_att_speed = 0,  % 原来的攻击速度，用于变攻速技能
        base_speed = 0,     % 基础移动速度
        speed = 0,          % 移动速度
        cell_num = 0,       % 背包格子数
        storage_num = 0,    % 仓库格子数
        mount = 0,          % 坐骑ID
        mount_figure = 0,   % 坐骑形像
        mount_equip = [0,0],    % 坐骑装备, [装备一，装备二]
        mount_speed = 0,    % 坐骑速度
        mount_spirit = [0,0], % 坐骑灵力消耗，[格子数，消耗灵力值]
        mount_spirit_cur = 0, % 坐骑累积走的格子数
        mount_lim = 0,      % 坐骑栏数量
        equip_attrit = 0,   % 装备磨损数，下线则清零
        equip_current = [0,0,0,0,0,0],      % 当前装备类型ID - [武器, 衣服, 无双神兵, 武器强化数, 衣服强化数, 武器强化至尊加成]
        fashion_weapon = [0,0],             % 穿戴的武器时装 - [武器时装类型ID，武器时装强化数]
        fashion_armor = [0,0],              % 穿戴的衣服时装 - [衣服时装类型ID，衣服时装强化数]
        fashion_accessory = [0,0],          % 穿戴的饰品时装 - [饰品时装类型ID，饰品时装强化数]
        fashion_head = [0,0],               % 穿戴的头饰时装 - [头饰时装类型ID，头饰时装强化数]
        fashion_foot = [0,0],               % 穿戴的足饰时装 - [足饰时装类型ID，足饰时装强化数]
        fashion_chat = [0,0],               % 穿戴的聊天弹框时装 - [聊天弹框时装类型ID，聊天弹框时装强化数]
        hide_fashion_weapon = 0,            % 是否隐藏武器时装，1为隐藏
        hide_fashion_armor = 0,             % 是否隐藏衣服时装，1为隐藏
        hide_fashion_accessory = 0,         % 是否隐藏饰品时装，1为隐藏
        hide_fashion_head = 0,              % 是否隐藏头饰时装，1为隐藏
        hide_fashion_foot = 0,              % 是否隐藏足饰时装，1为隐藏
        hide_fashion_chat = 0,              % 是否隐藏聊天弹框时装，1为隐藏
        suit_id = 0,                    % 当前装备全套的套装ID
        stren7_num = 0,     % 全身装备加七强化数
        weapon_light = 0,   % 武器光效 （编号1-6，如果为0表示没有换装过，则用equip_curent字段的第四个值显示武器光效）
        stren7_light = 0,   % 防具光效 （编号1-6，如果为0表示没有换装过，则用stren7_num字段显示防具光效）
        prefix_num = 0,     % 全身装备前缀数
        level_num = 0,      % 全身装备等级数
        inlay_min_lv = 0,   % 全身装备镶嵌石头最小等级
        super_stren_num = 0,     % 全身装备至尊数
        darkgold_num = 0,   % 全身红玉数
        retime = 3000,
        goods_pid,          % 物品模块进程Pid
        online_gift=0,      % 当前在线礼包
        online_gift_time=0, % 当前在线礼包生成时间
        exp = 0,
        exp_lim = 0,
        socket = none,
        sid = none,
        %bid = none,          % 战斗进程
        guild_id = 0,        % 帮派ID
        guild_name = [],     % 帮派名称
        guild_position = 0,  % 帮派职位
        guild_quit_lasttime = 0, % 帮派上次退出时间
        guild_quit_num = 0,  % 帮派退出次数
        guild_lv = 0,        % 帮派等级
        guild_dungeon_time_midnight = 0, % 副本设置当天的零点
        last_guild_dungeon_midnight = 0, % 上次打帮派副本的零点
        guild_dungeon_npc_list = [],     % 帮派副本领取npc列表
        master_quit_lasttime= 0, % 师门上次退出时间
        pid_dungeon = none,  % 副本进程
        pid_team = 0,        % 组队进程
        leader = 0,          % 是否队长，1为队长
        quickbar = [],       % 快捷栏
        skill = [],          % 技能
        skill_state = [],    % 技能cd时间
        talent_skill = [],   % 天赋技能
        talent_state = #talent_state{},   % 天赋相关
        fly_skill = 0,       % 天赋轻功是否学习
        online_flag = 0,     % 在线标记
        battle_status = [],  % 战斗状态
        heartbeat_time = 0,  %心跳时间
        talk_lim = 0,        % 禁言，0为正常，1为禁言
        pk_status = 0,              % pk状态：和平模式 0，全体模式 1， 国家模式 2， 帮派模式 3，队伍模式 4， 善恶模式 5
        pk_status_change_time = 0,  % 上次PK状态切换时间
        world_chat_time = 0,        %上次世界聊天时间戳
        other_chat_time = 0,        %上次其他频道聊天时间戳
        fcm_online_time = 0,        % 防沉迷累计在线时间
        fcm_offline_time = 0,       % 防沉迷累计离线时间
        fcm_last_notify_time = 0,   % 防沉迷通知的上次发送时间
        fcm_state = 0,              % 防沉迷状态：0健康（正常），1疲劳（收益减半），2不健康（收益为0）
        sell_status = 0,            % 点对点交易状态，1 交易中、3 锁定、4 确认、5 完成
        sell_id = 0,                % 交易者ID
        sell_list = [],             % 玩家出售列表 - [{物品ID，价格类型，单价}，...],
        pk_value = 0,               % 玩家pk值(白名:[0,100]，黄名:(100-200]，红名:200+)
        pk_value_reflesh_time = 0,
        hp_reply = [],              % 气血回复：[物品类型，单次回复量, 剩余量, 上次回复时间]
        mp_reply = [],              % 内力回复：[物品类型，单次回复量, 剩余量, 上次回复时间]
        hp_bag = 0,                 % 气血包
        mp_bag = 0,                 % 内力包
        hp_bag_time = 0,            % 气血包上次回复时间
        mp_bag_time = 0,            % 内力包上次回复时间
        hp_bag_reply = [],          % 新气血包 [物品类型ID，单次回复量, 剩余量, 上次回复时间]
        mp_bag_reply = [],          % 新内力包 [物品类型ID，单次回复量, 剩余量, 上次回复时间]
        auto_fight_give_data = 0,   % 上次赠送免费挂机日期
        auto_fight_start_time = 0,  % 挂机开启时间
        auto_fight_free_time = 0,   % 剩余免费时间(S)
        auto_fight_vip_time = 0,    % 剩余元宝时间(S)
        %% 打坐
        sit_down = 0,               % 打坐，值大于0表示在打坐中，1打坐，2双修
        sit_hp_time = 0,            % 打坐回血时间
        sit_exp_time = 0,           % 打坐加经验时间
        sit_llpt_time = 0,          % 打坐加历练时间
        sit_intimacy_time = 0,      % 打坐加亲密度时间
        sit_role = 0,               % 双修角色ID
        sit_invite_role = 0,        % 双修邀请角色ID
        mining   = 0,               % 采集状态，值为0表示未采集
        %% 好友祝福
        %is_accept_greeting = 0,     % 是否接受过祝福
        %accept_greeting_num = 0,    % 接受祝福次数
        gm = 0,                     % GM账号
        link_node = none,           % 其他连接节点
        link_port = none,           % 其他连接节点端口
        open_login = 0,             % 是否开启登陆器任务0是关闭，1是开启
        calc_node = [],             % 分布计算节点
        box_bag = null,             % 宝箱包裹
        box_new_info = null,        % 新宝箱包裹
        %%VIP
        vip_type = 0,               % VIP类型：0非vip、1黄金vip、2白金vip、3砖石vip
        vip_end_time = 0,           % VIP到期时间
        %% VIP特权
        vip_lv = 0,                 % VIP特权等级
        vip_expire_time = 0,        % VIP特权过期时间
        %% 运镖
        yunbiao = 0,                %% 0:没运镖, 1:正常运镖
        biao_lv = 0,                %% 镖车等级
        biao_buff = [0, 0, 0, 0, 0],%% 运镖buff [血量，血上限，防御，抗性，速度]
        yunbiao_pt = 0,             %% 运镖保护状态
        biao_color = 0,             %% 镖车颜色
        biao_lucky = 0,             %% 镖车幸运值
        waigua = 0,                 %% 外挂标识
        water_end_time = 0,         %% buff温泉到期时间
        arena_match_time = 0,            % 竞技场报名时间
        arena_grade = 0,                 % 竞技场级别
        arena_zone  = 0,                 % 竞技场战区
        arena_group = 0,                 % 竞技场分组
        arena_angry = 0,                 % 竞技场怒气值
        arena_win_num = 0,               % 竞技场获胜次数
        arena_win_list = [],             % 竞技场获胜列表
        arena_lose_num = 0,              % 竞技场失败次数
        arena_score = 0,                 % 竞技场积分
        arena_score_total = 0,           % 竞技场总积分
        arena_win_total  = 0,            % 竞技场总获胜次数
        arena_score_week  = 0,           % 竞技场周积分
        arena_win_week  = 0,             % 竞技场周获胜次数
        arena_task_award = [],           % 竞技场任务奖励
        arena_rank = 0,                  % 竞技场排名
        sixiang_match_time = 0,          % 远征四项报名时间
        sixiang_zone = 0,                % 远征四项战区
        guild_battle_flag = 0,           % 帮战标记
        guild_battle_time = 0,           % 帮战时间
        guild_battle_zone = 0,           % 帮战战区
        guild_battle_base_scene_id = 0,  % 帮战基地场景ID
        guild_battle_scene_id = 0,       % 帮战战场场景ID
        guild_battle_stone_flag = 0,     % 帮战神石标记
        guild_battle_score = 0,          % 帮战积分
        guild_battle_win_list = [],      % 帮战获胜列表
        guild_battle_angry = 0,          % 帮战怒气值
        guild_battle_task_award = [],    % 帮战任务奖励
        siege_battle_time = 0,           % 攻城战时间
        siege_battle_group = 0,          % 攻城战阵营
        siege_battle_attack_group_flag = 0,% 攻城战攻方阵营标记
        seige_battle_base_scene_id = 0,  % 攻城战基地场景ID
        siege_battle_win_num = 0,        % 攻城战杀敌次数
        siege_battle_expadd_lasttime = 0,% 攻城战经验增加最后时间
        siege_battle_revive_point = 0,   % 攻城战复活点
        siege_battle_buff = 0,           % 攻城战BUFF
        siege_battle_car_flag = 0,       % 攻城车标识
        siege_battle_car_buff = [0,0,0,0,0,0,0,0,0,0,0,0], % 攻城车buff [气血上限，气血, 内力, 攻击, 防御, 命中, 闪避, 暴击, 坚韧, 火抗, 冰抗, 毒抗] 
        siege_battle_primary_speed = [0, 0, 0, 0, 0], % 原来的速度 [speed, att_speed, att_area, hp, mp]
        siege_battle_car_num = 0,        % 攻城车编号
        siege_battle_door_cd = 0,        % 传送门cd
        platform = "",                   % 平台标识
        server_id = 0,                   % 运营服ID
        real_platform = "",              % 真实平台标识
        real_server_id = 0,              % 真实运营服ID
        kfz_score = 0,                   % 跨服3v3兑换积分
        kfz_score_week = 0,              % 跨服3v3周积分
        kfz_score_lasttime = 0,          % 跨服3v3积分获取最后时间
        kfz_3v3_apply_lasttime = 0,      % 跨服3v3最后报名时间
        kfz_3v3_award = [],              % 跨服3v3累积奖励[总经验,日经验,总历练声望,日经验, 日积分]
        kfz_3v3_active = 0,              % 跨服3v3中的活跃状态 0:挂机状态 1:默认活跃 2:战斗状态 3:采集祭坛状态
        kfz_honour= 0,                   % 跨服争霸赛荣誉
        kfz_arena_match_subtype = 0,     % 跨服争霸赛比赛子类型
        kfz_flag = 0,                    % 跨服战标记
        kfz_chat_flag = 0,               % 跨服聊天标记
        kfz_league_match_type = 0,       % 跨服联赛类型
        kfz_league_team_id = "",         % 跨服联赛队伍ID
        kfz_league_team_name = "",       % 跨服联赛队伍名称
        kfz_alliance_match_type = 0,     % 跨服排位天梯赛类型
        kfz_alliance_match_round = 0,    % 跨服排位天梯赛轮次
        kfz_alliance_match_zone = 0,     % 跨服排位天梯赛分区
        kfz_alliance_update_attr_time = 0, % 跨服排位天梯赛战力更新时间
        kfz_star_flag = 0,               % 跨服天梯明星赛标识位（0默认 1处于跨服天梯明星赛）
        master_score = 0,                % 师道值
        master_join_lasttime = 0,        % 最后拜师时间
        pet_capacity = 0,                % 宠物栏个数
        pet_capture_num = 0,             % 宠物捕获次数
        pet_capture_lasttime = 0,        % 宠物捕获最后时间
        pet_rename_num = 0,              % 宠物改名次数
        pet_rename_lasttime = 0,         % 宠物改名最后时间
        pet_treasurebox_free_num = 0,    % 宠物宝箱免费开启次数
        pet_treasurebox_free_lasttime=0, % 宠物宝箱免费开启最后时间
        pet_treasurebox_unpaid=0,        % 宠物宝箱未付消费
        pet_potential_fragment_num = 0,  % 宠物潜能碎片数
        pet_hit_egg_lasttime = 0,        % 宠物砸蛋最后时间
        pet_skill_treasurebox_lasttime = 0,% 宠物技能宝箱开启最后时间
        genius = 0,                      % 文采值
        honour = 0,                      % 荣誉
        king_honour = 0,                 % 帝王谷荣誉
        mood = 0,                        % 心情
        achieved_name_list = [],         % 称号
        spouse = #spouse_info{},         % 配偶
        wearing_ring = 0,                % 佩戴婚戒等级
        couple_intimacy_first = false,   % 是否亲密排行第一
        holding_wedding = 0,             % 角色是否正在举行婚礼
        cruise_mode = 0,                 % 巡游(0未/1普通/2抢亲)
        cruise_status = 0,               % 是否在巡游中
        couple_skill_info = #couple_skill_info{},% 夫妻技能信息
        couple_home = #couple_home_info{},% 夫妻家园信息
        national_info = [],              % 国家实力信息
        national_buff_info = [],         % 国家BUFF信息
        team_friend_buff = [],           % 好友组队BUFF
        winetype = 0,                    % 喝酒类型
        fix_chengjiu = 0,                % 修复成就，1为已修复，0为未修复
        guildboss_addexp_time = 0,       %上次在帮派狩猎场加经验时间
        guildboss_zhongjiang =0,         %喂养帮派神狗是否中奖
        check_password = 0,              % 是否需要验证二级密码（0不需要，1需要）
        wage_time = 0,                   %% 领取时间
        td_pid = 0,                      %%塔防进程id
        last_feedboss_time = 0,          %%上次喂狗时间
        td_s_wave = 0,                   % 单人塔防波数
        td_t_wave = 0,                   % 多人塔防波数
        td_data = 0,                     % 塔防波数有效日期
        image = 0,                       % 角色头像
        is_pra = 0,                      % 玩家是否在离线修炼中
        last_login_ip = [],              % 玩家登录IP
        figure = 0,                      % 玩家变身形象
        figure_goods_id = 0,             % 变身卡id
        figure_begin_time = 0,           % 变身卡开启时间
        figure_left_time = 0,            % 玩家变身剩余时间
        figure_buff = [0,0,0,0,0,0,0,0,0,0], % 变身buff
        last_water = 0,                  % 上次戏水时间
        guild_skill_list = [],           % 帮派技能列表
        guild_member_skill_point = 0,   % 帮派个人技能潜能点
        guild_member_skill_effect = {0,0,0,0}, %% 帮派个人技能buff{攻击，气血，暴击，防御}
        material = 0,                   % 帮派资材
        contact_list_award_time = 0,    % 通讯录奖励领取时间
        greeting_acc_exp = 0,           % 好友祝福累积经验
        greeting_acc_llpt = 0,          % 好友祝福累积历练声望
        fly_mount = 0,                  % 飞行坐骑id
        fly_mount_speed = 0,            % 飞行坐骑速度
        doubt_sign = 1,                 % 登录是否要弹验证码
        doubt = 0,                      % 小号疑似度
        studio_doubt = 0,               % 工作室号
        friend_num = 0,                 % 登录时好友数
        is_pay = false,                 % 是否有充值，true为有充值
        kill_list = [],                 % 杀害列表[{role_id, time}]
        challenge_start_time = 0,       % 单挑发起时间
        challenge_id = 0,               % 单人挑战标记ID
        challenge_wins = 0,             % 单人挑战胜利场数
        water_time = 0,                 %泡澡时间
        water_date = 0,                 %泡澡日期
        kfz_3v3_angry = 0,              %跨服战3v3怒气
        kfz_medal_active = 0,       %3v3勋章激活
        kfz_medal_lv = 1,           %3v3勋章等级
        kfz_medal_exp = 0,          %3v3勋章经验
        kfz_medal_win = 0,          %3v3竞技场胜利次数
        kfz_medal_lose = 0,         %3v3竞技场失败次数
        water_leiji = 0,            %温泉累积
        kfz_medal_type = 0,         % 跨服战勋章类型(0屁民勋章、1武斗勋章、2武师勋章、3武圣勋章)
        kfz_medal_attribute = [],   % 跨服战勋章属性
        kfz_medal_type_temp = 0,    % 跨服战勋章类型缓存
        kfz_medal_attribute_temp = [],  %跨服战勋章属性缓存
        kfz_medal_skill = [],       % 跨服战勋章技能
        ext_skill_flag = 0,         % 扩展技能标志
        vip_bag_flag = [],          % vip回馈礼包标志位
        crime_sign = 0,             % 犯罪标志
        kfz_team_id = 0,            % 跨服战战队ID
        week_gjpt = 0,              % 周国家声望
        week_gjpt_lasttime = 0,     % 上次获取周国家声望时间
        td_buff = [],               % 古墓扩展buff
	    practice_start_time = 0,    %打坐修炼开始时间
        practice_unixdate   = 0,    %打坐修炼凌晨时间戳
        practice_left_time  = 0,    %打坐修炼剩余时间
        practice_fast_time  = 0,    %打坐加速时间
        practice_state      = 0,    %打坐修炼状态 1修炼中 0不在修炼
        buy_lottry_num = 0,         %本周购买彩票数量
        buy_lottry_time = 0,        %上次购买彩票时间
        battle_help = [],           %助攻角色列表,
        quiz_last_time = 0          %上次答题时间
        ,stone_enchant_lv = 0               % 宝石附魂等级
        ,stone_enchant_exp = 0,             % 宝石附魂经验
        kfz_boss_score = 0,                 % 跨服BOSS积分
        kfz_boss_score_week = 0,            % 跨服BOSS周积分
        kfz_boss_score_apply_lasttime = 0,  % 跨服BOSS最后更新时间
        reborn_time = 0                     % 复活时间
        ,interior = 0                       % 内部标识
        ,wing_id = 0                        % 翅膀ID
        ,wing_figure = 0                    % 翅膀形像
        ,hide_wing_figure = 0               % 是否隐藏翅膀形像，1为隐藏
        ,wing_speed = 0                     % 翅膀速度
        ,wing_attribute = [0,0,0,0,0,0,0,0,0,0,0]   % 翅膀属性加成
        ,wing_status = 0                    % 翅膀状态，0卸下状态，1出战状态，2飞行状态
        ,wing_stren = 0                     % 翅膀强化数
        ,wing_fly_time = 0                  % 飞行能量消耗时间
        ,wing_huanhua = 0                   % 翅膀幻化值
        ,fly_hurt_state = 0                 % 飞行伤害标识 0 未伤害 1 正在伤害中
        ,fight_status = 0                   % 攻击状态（上次发起/受攻击时间戳）
        ,fxgz_lv = 0                        % 福星高照等级
        ,fxgz_exp = 0                       % 福星高照当前经验
        ,fxgz_score = 0                     % 福星高照积分
        ,start_collect_altar_time = 0       % 采集祭坛开始时间
        ,old_player_flag = 0                % 老玩家类型    #0非老玩家 1在线老玩家 2充值老玩家
        ,old_player_bag_time = 0            % 领取礼包时间
        ,old_player_active_list = []        % 活跃度七天记录
        ,old_player_logday_list = []        % 连续登陆记录
        ,old_player_notice_flag = 0         % 回归宣言
        ,old_player_online_bag_times = 0    % 领取在线奖励次数
        ,carnival_time = 0                  % 嘉年华时间
        ,dress_degree = 0                               % 着装度
        ,dress_degree_attr = [0,0,0,0,0,0,0,0,0,0,0,0]  % 着装度属性加成
        ,fashion_suit_attr = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] % 时装套装加成,与门客的对应
        ,fashion_armor_figure = 0                       % 穿戴的衣服时装形象ID，为0时为不穿戴时装形象
        ,fashion_weapon_figure = 0                      % 穿戴的武器时装形象ID，为0时为不穿戴时装形象
        ,fashion_accessory_figure = 0                   % 穿戴的饰品时装形象ID，为0时为不穿戴时装形象
        ,fashion_head_figure = 0                        % 穿戴的头部时装形象ID，为0时为不穿戴时装形象
        ,fashion_foot_figure = 0                        % 穿戴的足饰时装形象ID，为0时为不穿戴时装形象
        ,fashion_chat_figure = 0                        % 穿戴的聊天弹框时装形象ID，为0时为不穿戴时装形象
        ,love_degree = 0                    %% 爱心度
        ,memorial_devote = []               %% 纪念碑贡献
        ,guild_dial_refresh_time = 0        % 帮派转盘刷新时间,只用于每天的自动刷新
        ,guild_dial_buy_time = 0            % 帮派转盘购买时间
        ,guild_dial_goods_list = []         % 转盘物品编号列表
        ,first_sign_time = 0                % 首次签到时间
        ,interval = []                      % 签到时间天数间隔
        ,hanger_attr = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] % 门客加成
        ,baike_attr = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] % 百科加成
        ,personal_dial_card_list = []       % 个人摇奖点数列表

        ,invisible = 0                      % 是否隐身
        ,reclaim_val = 0                    % 物品回收积分
        ,wuju_match_time = 0                % 武举报名当天零点时间
        ,ring_num = 0               % 婚戒数量
        ,group_buing_join_list = []         % 玩家已参团物品列表
        ,pet_equip_exp   = 0                % 宠物装备觉醒经验
        ,pet_equip_score = 0                % 宠物装备积分
        ,pet_equip_achieve = 0              % 宠物装备成就
        ,pet_equip_zhufu = 0                % 宠物装备祝福值
        ,pet_equip_consume = 0
        ,pet_equip_achieve_val = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
        ,pet_fly_lv = 0                     % 宠物飞行器等级
        ,realm_notice_flag = 0              % 阵营公告标志位
        ,futai_id = 0                       % 帮派副本浮台id
        ,sea_dungeon_score = 0           % 深海副本boss完成数量
        ,last_die_time = 0                  % 上次死亡时间
        ,mount_footprint = 0                % 坐骑足迹
        ,kfz_war_battle_res = 0             % 跨服国战战魂（战斗资源）
        ,kfz_terr_id = 0                    % 跨服领土战，玩家当前进入的领土id
        ,kfz_terr_score = 0                 % 跨服领土战积分

        ,fly_egg_figure = 0                 % 飞行坐骑形象
        ,fly_egg_goods_count = 0            % 飞行坐骑消耗道具累计
        ,fly_egg_perfect = 0  
   }).

%% 物品状态表
-record(goods_status, {
        player_id = 0,              % 用户ID
        null_cells = [],            % 背包空格子位置
        equip_current = [0,0,0,0,0,0],    % 当前装备类型ID - [武器, 衣服, 无双神兵, 武器强化数, 衣服强化数, 武器强化至尊加成]
        equip_suit = [],            % 当前身上套装列表 - [{套装ID，套装数量}，...]
        suit_id = 0,                % 当前装备全套的套装ID
        stren7_num = 0,     % 全身装备加七强化数
        prefix_num = 0,     % 全身装备前缀数
        level_num = 0,      % 全身装备等级数
        super_stren_num = 0,     % 全身装备至尊数
        darkgold_num = 0,   % 全身红玉数
        hp_cd = 0,                  % 使用气血药的冷却时间
        mp_cd = 0,                  % 使用内力药的冷却时间
        sell_status = 0,            % 点对点交易状态，1 交易中
        self_sell = [],             % 自身挂售在交易市场的记录ID
%        shop_one_gold=0,            % 商城1元宝限购，1 已卖过，0 未卖过
%        shop_zero_gold=0,           % 商城0元宝限购，1 已卖过，0 未卖过
        gift_list = [],             % 已领取礼包列表
        merge_time = 0              % 合并时间
        ,box_open_time = 0          % 开宝箱时间
        ,wing_train_time = 0        % 翅膀培养时间
        ,wing_skill_map_time = 0    % 翅膀摘星时间
    }).

%% 帮派
-record(ets_guild, {
        id = 0,                        % 记录ID
        name = <<>>,                   % 帮派名称
        name_upper = <<>>,             % 帮派名称（大写）
        tenet = <<>>,                  % 帮派宣言
        announce = <<>>,               % 帮派公告
        initiator_id = 0,              % 创始人ID
        initiator_name = <<>>,         % 创始人名称
        chief_id = 0,                  % 现任帮主ID
        chief_name = <<>>,             % 现任帮主昵称
        deputy_chief1_id = 0,          % 副帮主1ID
        deputy_chief1_name = <<>>,     % 副帮主1昵称
        deputy_chief2_id = 0,          % 副帮主2ID
        deputy_chief2_name = <<>>,     % 副帮主2昵称
        deputy_chief_num = 0,          % 副帮主数
        member_num = 0,                % 当前成员数
        member_capacity = 0,           % 成员上限
        realm = 0,                     % 阵营
        level = 0,                     % 级别
        reputation = 0,                % 声望
        funds = 0,                     % 帮派资金
        contribution = 0,              % 建设值
        contribution_daily = 0,        % 每日收取的建设值
        contribution_threshold = 0,    % 建设值上限
        contribution_get_nexttime = 0, % 下次收取建设值时间
        combat_num = 0,                % 帮战次数
        combat_victory_num = 0,        % 帮战胜利次数
        qq1 = <<>>,                    % QQ群1
        qq2 = <<>>,                    % QQ群2
        create_time = 0,               % 记录创建时间
        create_type = 0,               % 创建类型
        disband_flag = 0,              % 解散申请标记
        disband_confirm_time = 0,      % 解散申请的确认开始时间
        disband_deadline_time = 0,     % 掉级后的自动解散时间
        depot_level = 0,               % 仓库等级
        hall_level = 0,                % 大厅等级
        house_level = 0,               % 厢房等级
        mall_level = 0,                % 商城等级
        mall_contri = 0,               % 商城累积的建设度
        rename_flag = 0,               % 改名标记
        battle_time = 0,               % 帮战时间
        battle_lasttime = 0,           % 帮战最后参赛时间
        siege_time = 0,                % 攻城战时间
        siege_lasttime = 0,            % 攻城战最后参赛时间
        siege_group = 0,               % 攻城战阵营
        siege_lastgroup = 0,           % 攻城战最后参赛阵营
        siege_attack_group_flag = 0,   % 攻城战攻方阵营标记
        siege_margin_flag = 0,         % 攻城战保证金缴纳标记
        siege_end_flag = 0,            % 攻城战结束标记
        siege_result = 0,              % 攻城战结果
        siege_occupier_image = [],     % 攻城战城主形象
        siege_occupier_cw_lasttime = 0,% 攻城战城主上次登录传闻时间
        siege_tax_time = 0,            % 攻城战收税时间
        siege_occupy_num = 0,          % 攻城战连续占领次数
        merge_guild_id = 0,            % 邀请合并的帮派ID
        merge_guild_direction = 0,     % 邀请合并的方向 0保留本帮 1解散本帮
        guild_boss_type = 60101,       % 帮派boss类型
        guild_boss_ice = 0,            % 帮派boss魔魄值
        guild_boss_fire = 0,           % 帮派boss神魄值
        guild_boss_exp = 0,            % 帮派boss灵力值
        guild_boss_exp_call = 0,       % 帮派boss被召唤时刻灵力值
        guild_boss_call = 0,           % 上次召唤帮派boss时间
        guild_boss_call_type = 0,      % 上次被召唤的boss类型
        guild_boss_lingyang_time = 0,  % 上次领养时间
        guild_kuang_scene = 0,         % 帮派矿场Id
        guild_boss_change_time = 0,    % 上次帮派魂魄变化时间
        guild_boss_line = 0,           % 帮派boss所在线路
        guild_boss_story = [],         % 帮派boss召唤记录
        biao = 0,                      % 帮派镖车(0为没有运镖, 1为运镖)
        biao_begin_time = 0,           % 帮派镖车开启时间
        biao_mon_id = 0,               % 帮派镖车唯一id
        biao_line = 0,                 % 帮派镖车所在线路
        gather_member_lasttime = 0,    % 帮主召唤最后时间
        activity_red_packet_num = 0,   % 活动期间帮派红包总数量
        kfz_boss_contribution = [],    % 跨服BOSS贡献
        kfz_boss_score = 0,            % 跨服BOSS积分
        kfz_boss_conti_win_num = 0,    % 跨服BOSS连胜次数
        kfz_boss_conti_lose_num = 0,   % 跨服BOSS连败次数
        kfz_boss_lasttime = 0          % 跨服BOSS最后参赛时间
    }).

%% 帮派（线路独占信息）
-record(ets_guild_exclusive, {
        guild_id = 0,                  % 帮派ID
        scene = 0                      % 帮派场景
    }).

%% 帮派成员
-record(ets_guild_member, {
        id = 0,                       % 记录ID
        name = <<>>,                  % 角色昵称
        guild_id = 0,                 % 帮派ID
        guild_name = <<>>,            % 帮派名称
        donate_total = 0,             % 历史总贡献
        donate_total_card = 0,        % 建设令的历史总贡献
        donate_total_coin = 0,        % 铜钱的历史总贡献
        donate_lasttime = 0,          % 最后贡献时间
        donate_total_lastday = 0,     % 日贡献
        donate_total_lastweek = 0,    % 周贡献
        paid_get_lasttime = 0,        % 日福利最后获取时间
        create_time = 0,              % 记录创建时间
        title = <<>>,                 % 帮派称号
        remark = <<>>,                % 个人备注
        sex   = 0,                    % 性别
        honor = 0,                    % 荣誉
        jobs  = 0,                    % 职位
        level = 0,                    % 等级
        position = 0,                 % 帮派职位
        online_flag = 0,              % 是否在线
        last_login_time = 0,          % 最后登录时间
        career = 0,                   % 职业
        depot_store_lasttime = 0,     % 帮派仓库最后存入物品时间
        depot_store_num = 0,          % 帮派仓库存入数量
        version = 0,                  % 乐观锁
        donate  = 0,                  % 贡献
        hall_enter_lasttime = 0,      % 帮派大厅最后进入时间
        paid_add = 0,                 % 日福利增加
        siege_tax_add = 0,            % 城战税收增加
        siege_tax_time = 0,           % 攻城战收税时间
        image = 0,                    % 头像
        vip = 0,                      % VIP类型
        material = 0,                 % 帮派资材
        qq = [],                      % QQ号
        phone = [],                   % 手机号
        privacy = 0,                  % 通讯录可见性
        activity_online_time = 0,     % 活动期间成员在线累计时间
        get_red_packet_num = 0,       % 获取到的红包数量
        guild_boss_qinmi = 0,         % 帮派神兽亲密度
        used_material = 0,            % 富裕值(用掉的帮派财富)
        kfz_boss_set_apply = 0,       % 跨服BOSS报名权限
        kfz_boss_match_time = 0       % 跨服BOSS比赛时间
    }).

%% 帮派申请
-record(ets_guild_apply, {
        id = 0,                       % 记录ID
        guild_id = 0,                 % 帮派ID
        player_id = 0,                % 角色ID
        player_name = <<>>,           % 角色昵称
        player_sex   = 0,             % 性别
        player_jobs  = 0,             % 职位
        player_level = 0,             % 等级
        create_time = 0,              % 申请时间
        player_career = 0,            % 职业
        online_flag = 0,              % 线路
        player_vip_type = 0           % VIP类型
    }).

%% 帮派邀请
-record(ets_guild_invite, {
        id = 0,                       % 记录ID
        guild_id = 0,                 % 帮派ID
        player_id = 0,                % 角色ID
        create_time = 0               % 邀请时间
    }).

%% 帮战获胜列表
-record(guild_battle_win, {
    player_id = 0,
    win_time = 0
    }).

%% 帮战场景
-record(ets_guild_battle_scene, {
    id = 0,
    scene_id = 0,
    zone = 0
}).

%% 帮派战状态信息
-record(ets_guild_battle_state_info, {
    battle_time  = 0,                    % 战场时间
    battle_state = 0,                    % 战场状态
    state_time   = 0,                    % 状态时间
    state_interval = 0                   % 状态间隔
}).

%% 帮战结果
-record(ets_guild_battle_result, {
    id = 0,
    name = <<>>,
    level = 0,
    battle_time = 0,
    battle_lasttime = 0,
    battle_zone = 0,
    battle_base_scene_id = 0,
    battle_scene_id = 0,
    battle_enter_num = 0,
    battle_score_total = 0,
    battle_win_num_total = 0,
    battle_lose_num_total = 0,
    battle_stone_num_total = 0,
    battle_monster_num_total = 0,
    battle_score = 0,
    battle_win_num = 0,
    battle_lose_num = 0,
    battle_stone_num = 0,
    battle_monster_num = 0,
    battle_stone_type = 1,
    battle_monster_num_info = [0, 0, 0, 0, 0],
    battle_stone_num_info = [0, 0, 0, 0, 0],
    battle_rank = 0
 }).

%% 帮战成员结果
-record(ets_guild_battle_member_result, {
    id = 0,
    name = <<>>,
    guild_id = 0,
    guild_name = 0,
    battle_time = 0,
    battle_zone = 0,
    battle_score_total = 0,
    battle_win_num_total = 0,
    battle_lose_num_total = 0,
    battle_stone_num_total = 0,
    battle_monster_num_total = 0,
    battle_score = 0,
    battle_win_num = 0,
    battle_lose_num = 0,
    battle_stone_num = 0,
    battle_monster_num = 0,
    battle_general_mon_num = 0, %% 天魔小兵数
    battle_guild_rank = 0,
    battle_rank = 0,
    battle_angry = 0,
    battle_award_flag = false
    }).

%% 帮战成员结果
-record(ets_guild_battle_member_info, {
        id = 0,
        name = <<>>,
        guild_id = 0,
        guild_name = 0,
        battle_time = 0,
        battle_zone = 0,
        battle_score_total = 0,
        battle_win_num_total = 0,
        battle_lose_num_total = 0,
        battle_stone_num_total = 0,
        battle_monster_num_total = 0,
        battle_score = 0,
        battle_win_num = 0,
        battle_lose_num = 0,
        battle_stone_num = 0,
        battle_monster_num = 0,
        level = 0,
        career = 0,
        sex = 0
    }).

%% 帮派奖励物品配置表
-record(ets_base_guild_battle_award, {
        guild_rank  = 0,                % 帮派名次
        award_goods = []                % 奖励物品
    }).

%% 帮派奖励物品表
-record(ets_guild_award, {
        guild_id = 0                    % 帮派ID
        ,goods_list = []                % 奖励物品列表, [{物品类型ID，物品数量，贡献}，...]
    }).

%% 帮派奖励分配表
-record(ets_guild_award_alloc, {
        id = 0                          % 编号
        ,player_id = 0                  % 角色ID
        ,guild_id = 0                   % 帮派ID
        ,goods_id = 0                   % 物品类型ID
        ,goods_num = 0                  % 物品数量
        ,score = 0                      % 消耗贡献
        ,time = 0                       % 分配时间
    }).

%% 帮派成员奖励物品表
-record(ets_guild_member_award, {
        pid = 0                         % 角色ID
        ,guild_id = 0                   % 帮派ID
        ,goods_list = []                % 奖励物品列表
    }).

%% 攻城战信息
-record(ets_siege_battle, {
    siege_time = 0,                      % 攻城战时间
    flag_monster_info = 0,               % 护院大旗击破结果
    flag_monster_hurt_info_list = [],    % 护院大旗信息
    broken_array_monster_info_list = [], % 破阵石信息
    door_monster_info = 0,               % 城门击破结果
    door_monster_hurt_info_list = [],    % 城门信息
    occupy_type = 0,                     % 攻城战占领类型 0无城主 1攻方获胜 2守方获胜
    occupy_group = 0,                    % 占领阵营
    occupy_guild_id = 0,                 % 占领帮派ID
    occupy_guild_name = <<>>,            % 占领帮派名字
    last_occupy_guild_id = 0,            % 上次占领帮派ID
    occupy_num = 0                       % 连续占领次数
}).

%% 攻城战状态信息
-record(ets_siege_battle_state_info, {
    battle_time  = 0,                    % 战场时间
    battle_state = 0,                    % 战场状态
    state_time   = 0,                    % 状态时间
    state_interval = 0                   % 状态间隔
}).

%% 攻城战奖励物品配置表
-record(ets_base_siege_battle_award, {
        siege_result = 0,                % 攻城战结果
        award_goods  = []                % 奖励物品
}).

%% 攻城战场景
-record(ets_siege_battle_scene, {
    id = 0,
    scene_id = 0,
    group = 0,
    apply_guild_num = 0
}).

%% 攻城战结果
-record(ets_siege_battle_result, {
    id = 0,                             % 帮派ID
    name = <<>>,                        % 帮派名称
    chief_id = 0,                       % 帮主ID
    chief_name = <<>>,                  % 帮主名称
    level = 0,                          % 等级
    realm = 0,                          % 阵营
    siege_time = 0,                     % 攻城战时间
    siege_lasttime = 0,                 % 攻城战最后参赛时间
    siege_group = 0,                    % 攻城战阵营
    siege_lastgroup = 0,                % 攻城战最后参赛阵营
    siege_attack_group_flag = 0,        % 攻城战攻方阵营标记
    siege_margin_flag = 0,              % 攻城战保证金缴纳标记
    siege_base_scene_id = 0,            % 攻城战基地场景ID
    siege_forces_use = 0,               % 攻城战已用兵力值
    siege_stone_hurt = 0,               % 攻城战神石伤害
    siege_stone_last_hurt = 0,          % 攻城战神石上次伤害
    siege_occupy_value = 0,             % 攻城战占领值
    siege_occupy_value_rank = 0,        % 攻城战占领值排行
    siege_stone_hurt_time = 0,          % 攻城战神石伤害时间
    siege_end_flag = 0,                 % 攻城战结束标记
    siege_result = 0,                   % 攻城战结果
    siege_rank = 0,                     % 攻城战排名
    siege_occupier_image = [],          % 攻城战城主形象
    siege_occupy_value_add_lasttime = 0,% 攻城战占领值最后增加时间
    siege_occupy_value_add = 0,         % 攻城战占领值最后增加值
    siege_occupy_value_deduct_lasttime = 0,% 攻城战占领值最后减少时间
    siege_occupy_value_deduct = 0,      % 攻城战占领值最后减少值
    siege_occupy_num = 0                % 攻城战连续占领次数
 }).

%% 攻城战成员结果
-record(ets_siege_battle_member_result, {
    id = 0,                             % 成员ID
    name = <<>>,                        % 成员名称
    level = 0,                          % 成员等级
    guild_id = 0,                       % 帮派ID
    guild_name = <<>>,                  % 帮派名称
    siege_time = 0,                     % 攻城战时间
    siege_group = 0,                    % 攻城战阵营
    siege_attack_group_flag = 0,        % 攻城战攻方阵营标记
    siege_win_num = 0,                  % 攻城战杀敌次数
    siege_score = 0,                    % 攻城战战功
    siege_revive_point = 0,             % 攻城战复活点
    siege_occupy_value_lasttime = 0,    % 攻城战占领值上次时间
    siege_revive_other_num = 0,         % 攻城战复活他人次数
    siege_award_exp = 0,                % 攻城战奖励经验
    siege_award_score = 0,              % 攻城战奖励战功
    siege_award_goods_list = [],        % 攻城战奖励物品
    siege_kill_flag = 0                 % 攻城战击破战旗数
 }).

%% 宠物物品配置
-record(ets_base_goods_pet, {
        id = 0,                         % 物品类型ID
        name = <<>>,                    % 物品名称
        aptitude_threshold = 0,         % 资质上限
        effect = 0,                     % 物品效用
        effect2 = 0,                    % 物品效用2
        probability = 0,                % 商城出现的概率
        sell = 0,                       % 是否商城出售
        type = 0,                       % 类型
        subtype = 0,                    % 子类型
        color = 0,                      % 品阶
        price = 0,                      % 价格
        level = 0,                      % 级别
        expire_time = 0,                % 失效时间
        is_s = 0                        % 是否S级
    }).

%% 宠物商城
-record(ets_pet_shop, {
        player_id = 0,                  % 角色ID
        last_update_time = 0,           % 最后更新时间
        goods_preferential_list = <<>>, % 优惠商品列表
        goods_buy_list = <<>>           % 已买商品列表
    }).

%% 宠物技能
-record(ets_pet_skill, {
        id = 0,                         % 记录ID
        pet_id = 0,                     % 宠物ID
        type_id = 0,                    % 物品类型ID
        type = 0,                       % 物品类型
        subtype = 0,                    % 物品子类型
        level = 0,                      % 级别
        effect = 0,                     % 效用
        effect2 = 0,                    % 效用2
        player_id = 0,                  % 角色ID
        sub_skills = [],                % 子技能列表 [{子技能1物品类型id，级别}, {子技能2物品类型id，级别}]
        active_flag = 0,                % 主动技能标识
        use_flag = 0                    % 主动技能使用标识
    }).

%% 宠物
-record(ets_pet, {
        id = 0,                          % 宠物ID
        name = <<>>,                     % 宠物名称
        player_id = 0,                   % 角色昵称
        type_id = 0,                     % 宠物类型ID
        figure = 0,                      % 变身前的宠物形象
        origin_figure = 0,               % 原来的宠物形象
        level = 0,                       % 等级
        aptitude = 0,                    % 资质
        aptitude_threshold = 0,          % 资质上限
        quality = 0,                     % 品阶
        forza = 0,                       % 力量
        wit = 0,                         % 灵力
        agile = 0,                       % 身法
        thew = 0,                        % 体质
        growth = 0,                      % 成长
        unalloc_attr = 0,                % 未分配的属性点
        strength = 0,                    % 快乐值
        strength_threshold = 0,          % 快乐值上限
        fight_flag = 0,                  % 放出标志位 0放出 1收回
        fight_starttime = 0,             % 放出时间
        upgrade_exp = 0,                 % 升级经验
        figure_change_flag = 0,          % 变身标记位
        figure_change_lefttime = 0,      % 变身剩余时间或结束时间
        create_time = 0,                 % 创建时间
        pet_attr = [],                   % 宠物加成属性
        pet_skill_attr = [],             % 宠物技能加成属性
        skills = <<>>,                   % 技能
        name_upper = <<>>,               % 转换成大写后的宠物名称
        strength_nexttime = 0,           % 下次同步快乐值时间
        savvy_lv = 0,                    % 悟性等级
        savvy_exp = 0,                   % 悟性经验
        aptitude_lucky_lv = 0,           % 资质幸运等级
        aptitude_lucky_exp = 0,          % 资质幸运经验
        growth_lucky_lv = 0,             % 成长幸运等级
        growth_lucky_exp = 0,            % 成长幸运经验
        potentials = <<>>,               % 宠物潜能
        pet_potential_attr = [],         % 宠物潜能加成属性
        combat_power = 0,                % 战斗力
        spirit = 0,                      % 灵力
        spirit_threshold = 0,            % 灵力上限
        equip_active_skills = [],        % 已装备主动技能[{技能类型ID,等级}|...]
        active_skills = <<>>,            % 主动技能
        shortcut_skills = [],            % 快捷主动技能[{技能id,位置}]
        attack_mode = 0,                 % 攻击模式
        fly_lv = 0                       % 飞行器等级
    }).

%% 宠物图鉴配置
-record(ets_base_pet_manual, {
        id = 0,                          % 形像ID
        name = <<>>,                     % 宠物名称
        intimacy_initial = 0,            % 亲密度初始值
        intimacy_activate = 0,           % 亲密度激活值
        intimacy_threshold = 0,          % 亲密度上限值
        intimacy_figure_change = 0,      % 亲密度变身消耗值
        figure_change_total_time = 0,    % 变身时效
        figure_change_cd_time = 0,       % 变身冷却时间
        buff = 0,                        % 属性加成
        buff_binary = <<>>,              % 解析后的属性加成
        intimacy_goods_type_id = 0,      % 亲密度提升道具ID
        attribute = []                   % 属性加成
    }).

%% 宠物图鉴
-record(ets_pet_manual, {
        id = 0,                          % 记录ID
        player_id = 0,                   % 角色ID
        figure = 0,                      % 形像ID
        name = <<>>,                     % 宠物名称
        intimacy = 0,                    % 亲密度
        figure_change_starttime = 0,     % 变身开始时间
        figure_change_cd_endtime = 0,    % 变身冷却结束时间
        intimacy_initial = 0,            % 亲密度初始值
        intimacy_activate = 0,           % 亲密度激活值
        intimacy_threshold = 0,          % 亲密度上限值
        intimacy_figure_change = 0,      % 亲密度变身消耗值
        figure_change_total_time = 0,    % 变身时效
        figure_change_cd_time = 0,       % 变身冷却时间
        buff = 0,                        % 属性加成
        buff_binary = <<>>,              % 解析后的属性加成
        intimacy_goods_type_id = 0,      % 亲密度提升道具ID
        activate_flag = 0                % 激活标识
    }).

%% 宠物潜能配置
-record(ets_base_pet_potential, {
        id = 0,                   % 潜能类型ID
        buff_type = 0,            % 属性加成类型
        buff_value = 0,           % 属性加成值
        lv = 0,                   % 默认等级
        quality = 0,              % 默认品质
        exp_upgrade = 0,          % 升级经验
        exp_mix = 0,              % 融合经验
        name = <<>>,              % 潜能名称
        double_flag = 0,          % 宠物的双潜能标识
        value_list = []           % 加成列表，对双属性有用
    }).

%% 宠物潜能
-record(ets_pet_potential, {
        id = 0,                   % 记录ID
        player_id = 0,            % 角色ID
        pet_id = 0,               % 宠物ID
        location = 0,             % 位置
        potential_type_id = 0,    % 潜能类型ID
        lv = 0,                   % 等级
        exp = 0,                  % 经验
        create_time = 0,          % 创建时间
        buff_type = 0,            % 加成类型
        buff_value = 0,           % 加成值
        quality = 0,              % 品质
        exp_upgrade = 0,          % 升级经验
        exp_mix = 0,              % 融合经验
        name = <<>>,              % 潜能名称
        double_flag = 0,          % 宠物的双潜能标识
        value_list = [],          % 加成列表，对双属性有用
        active_flag = 0           % 子技能激活标识 0 未激活,1 激活左技能, 2 激活右技能, 3 激活全技能
    }).

%% 宠物宝箱
-record(ets_pet_treasurebox, {
        id = 0,     	          % 记录ID
        player_id = 0,	          % 角色ID
        lv = 0,                   % 宝箱等级
        activate_flag = 0,	  % 激活标识
        yb_activate_num = 0,	  % 元宝激活次数
        activate_lasttime = 0	  % 最后激活时间
    }).

%% 宠物技能宝箱
-record(ets_pet_skill_treasurebox, {
        player_id = 0,	          % 角色ID
        coin_lucky_value = 0,     % 铜币幸运值
        yb_lucky_value = 0,       % 元宝幸运值
        practice_value = 0,       % 勤修值
        skill_list1 = [],	      % 技能列表
        skill_list2 = [],	      % 技能列表
        state = 0,                % 状态
        pay = 0,                  % 支付状态
        create_time = 0	          % 创建时间
    }).

%% 师傅信息
-record(ets_master, {
        id = 0,                   % 角色ID
        name = <<>>,              % 名称
        sex  = 0,                 % 性别
        career = 0,               % 职业
        realm = 0,                % 阵营
        vip_type = 0,             % VIP类型
        level = 0,                % 等级
        score = 0,                % 师道值
        apprentice_num = 0,       % 徒弟数量
        apprentice_finish_num = 0,% 出师徒弟数量
        status = 0,               % 是否上榜
        register_time = 0,        % 上榜时间
        create_time = 0,          % 创建时间
        report_num = 0,           % 汇报次数
        line = 0,                 % 线路
        last_login_time = 0,      % 最后登录时间
        image = 0                 % 头像
    }).

%% 师徒关系
-record(ets_master_apprentice, {
        id = 0,                      % 角色ID
        name = <<>>,                 % 名称
        sex = 0,                     % 性别
        career = 0,                  % 职业
        level = 0,                   % 等级
        status = 0,                  % 状态
        master_id = 0,               % 师傅角色ID
        report_num = 0,              % 汇报次数
        last_report_level = 0,       % 上次汇报等级
        last_report_time = 0,        % 上次汇报时间
        accumulate_report_level = 0, % 累积汇报等级
        apply_time = 0,              % 申请时间
        invite_time = 0,             % 邀请时间
        join_time = 0,               % 加入时间
        line = 0,                    % 线路
        last_login_time = 0,         % 最后登录时间
        relationship = 0,            % 称呼
        image = 0                    % 头像
    }).

%% 竞技场获胜列表
-record(arena_win, {
    player_id = 0,
    win_time = 0
    }).

%% 竞技场
-record(ets_arena, {
    match_time = 0,
    player_num = 0,
    zone_num   = [0, 0, 0],
    max_player_level = [0, 0, 0]
    }).

%% 竞技场状态信息
-record(ets_arena_state_info, {
    battle_time  = 0,                    % 战场时间
    battle_state = 0,                    % 战场状态
    state_time   = 0,                    % 状态时间
    state_interval = 0                   % 状态间隔
}).

%% 竞技场结果
-record(ets_arena_result, {
    id = 0,                    % 记录ID
    grade = 0,                 % 赛区级别
    zone = 0,                  % 赛区
    player_id = 0,             % 角色ID
    player_name = 0,           % 角色姓名
    sex = 0,                   % 性别
    realm = 0,                 % 国家
    career = 0,                % 职业
    level = 0,                 % 等级
    group = 0,                 % 阵营
    win_num = 0,               % 杀人数
    win_list = [],             % 杀人列表
    lose_num = 0,              % 被杀次数
    enter_flag = 0,            % 进入标记位
    score = 0,                 % 积分
    angry = 0,                 % 怒气值
    match_time = 0,            % 比赛时间
    create_time = 0,           % 创建时间
    scene_id = 0,              % 场景ID
    score_total = 0,           % 总积分
    win_total = 0,             % 总杀人数
    score_week = 0,            % 周积分
    win_week = 0,              % 周杀人数
    last_match_time = 0,       % 最后参数时间
    group_rank = 0,            % 阵营排名
    award_score = 0,           % 奖励积分
    week_calendar_score = 0,   % 周日历奖励积分
    conti_win_num = 0,         % 连斩数
    last_win_time = 0,         % 最后杀人时间
    conti_win_list = [],       % 连斩杀人列表
    conti_win_maxnum = 0,      % 最大连斩数
    award_get_time = 0         % 奖励领取时间
}).

%% 竞技场场景
-record(ets_arena_scene, {
    id = 0,
    scene_id = 0,
    grade = 0,
    zone = 0,
    score  = [0, 0, 0],
    result = [0, 0, 0]
}).

%% 宝石战场状态信息
-record(ets_jewel_battle_state_info, {
    battle_time  = 0,                    % 战场时间
    battle_state = 0,                    % 战场状态
    state_time   = 0,                    % 状态时间
    state_interval = 0                   % 状态间隔
}).

%% 宝石战场
-record(ets_jewel_battle_result, {
    player_id = 0,             % 角色ID
    player_name = 0,           % 角色姓名
    sex = 0,                   % 性别
    realm = 0,                 % 国家
    career = 0,                % 职业
    level = 0,                 % 等级

    scene_id = 0,              % 场景ID
    task_seq = 1,              % 第几波任务
    jewel = 0,                 % 当前宝石碎片数量

    jewel_win = 0,             % 杀死其他玩家，所获宝石碎片数量
    jewel_lose = 0,            % 被其他玩家杀死，所损失宝石碎片数量

    iron = 0,                  % 当前采集的铁矿的数量，100表示已经领取了奖励
    copper = 0,                % 当前采集的铜矿的数量，100表示已经领取了奖励
    gold = 0,                  % 当前采集的金矿的数量，100表示已经领取了奖励
    crystal = 0,               % 当前采集的晶矿的数量，100表示已经领取了奖励
    boss = 0                   % 当前斩杀的boss的数量，100表示已经领取了奖励
}).

%% 宝石战场场景
-record(ets_jewel_battle_scene, {
    id = 0,  %% 场景唯一ID
    num = 0  %% 场景当前人数
}).

%% 宝石试炼状态信息
-record(ets_grab_stone_state_info, {
    battle_time  = 0,                    % 战场时间
    battle_state = 0,                    % 战场状态
    state_time   = 0,                    % 状态时间
    state_interval = 0                   % 状态间隔
}).

%% 宝石试炼结果
-record(ets_grab_stone_result, {
    id = {0, 0},                 % 记录ID
    player_id = 0,               % 角色ID
    win_list = [],               % 杀人列表
    stone_list = [],             % 宝石列表
    score_list = [0, 0, 0, 0, 0],% 积分
    score_total = 0,             % 总积分
    collect_info = [],           % 采集信息
    match_time = 0               % 比赛时间
}).

%% 远征四项
-record(ets_sixiang, {
    match_time     = 0,
    start_time     = 0,
    match_interval = 0
    }).

%% 远征四项结果
-record(ets_sixiang_result, {
    id = 0,                    % 记录ID
    zone = 0,                  % 赛区
    scene_id = 0,              % 场景ID
    player_id = 0,             % 角色ID
    player_name = 0,           % 角色姓名
    lv = 0,                    % 等级
    enter_flag = 0,            % 进入标志
    rank = 0,                  % 名次
    lap_num = 0,               % 圈数
    last_time_point = 0,       % 最后记时点
    use_time = 0,              % 用时
    match_time = 0,            % 比赛时间
    create_time = 0,           % 创建时间
    get_exp_time = 0           % 经验领取时间
}).

%% 远征四项状态信息
-record(ets_sixiang_state_info, {
    battle_time  = 0,                    % 战场时间
    battle_state = 0,                    % 战场状态
    state_time   = 0,                    % 状态时间
    state_interval = 0                   % 状态间隔
}).

%% 远征四项场景
-record(ets_sixiang_scene, {
    id = 0,
    scene_id = 0,
    zone = 0
}).

%% 节点信息
-record(ets_cluster, {
    id           = 1,
    platform     = "",
    node_type    = 0,
    center_addr  = "",
    center_port  = 0,
    gateway_port = 0,
    chat_port    = 0,
    unite_port   = 0,
    registry_key = "",
    ticket       = "",
    open_flag    = 0,
    socket       = undefined
}).

%% 跨服擂台赛战场
-record(ets_kfz_arena_battle, {
    battle_time  = 0,   % 战场时间
    battle_state = 0,   % 战场状态
    state_time   = 0,   % 状态时间
    state_interval = 0, % 状态间隔
    apply_player_num = 0% 报名人数
}).

%% 跨服擂台赛比赛
-record(ets_kfz_arena, {
    match_time  = 0,                     % 比赛时间
    match_type  = 0,                     % 比赛类型
    match_state = 0,                     % 比赛状态
    state_time  = 0,                     % 状态时间
    state_interval = 0,                  % 状态间隔

    zs_boss_monster_id = [],             % 战神BOSS
    ws_boss_monster_id = [],             % 武圣BOSS
    wsxz_monster_id = [],                % 武圣勋章
    zs_boss_monster_kill_flag = 0,       % 战神BOSS击杀标记
    ws_boss_monster_kill_flag = 0,       % 武圣BOSS击杀标记
    wsxz_monster_kill_flag = 0,          % 武圣勋章采集标记
    zsxz_owner_list = [],                % 战神勋章获得者
    wsxz_owner_list = [],                % 武圣勋章获得者
    jjxz_owner_list = [],                % 竞技勋章获得者

    dibang_zw_boss_monster_id = [],      % 地榜真武BOSS
    dibang_ws_boss_monster_id = [],      % 地榜武圣BOSS
    dibang_wsxz_monster_id    = [],      % 地榜武圣勋章
    dibang_zw_boss_monster_kill_flag=0,  % 地榜真武BOSS击杀标志
    dibang_ws_boss_monster_kill_flag = 0,% 地榜武圣BOSS击杀标志
    dibang_wsxz_monster_kill_flag=0      % 地榜武圣勋章采集标志
}).

%% 跨服擂台赛场景
-record(ets_kfz_arena_scene, {
    id = 0,                    % 场景ID
    scene_type_id = 0,         % 场景类型ID
    match_type = 0,            % 比赛类型
    zone = 0                   % 比赛分区
}).

%% 跨服擂台赛结果
-record(ets_kfz_arena_result, {
    id = 0,                    % 记录ID
    platform = "",             % 平台标识
    server_id = 0,             % 运营服ID
    player_id = 0,             % 玩家ID
    new_player_id = 0,         % 新玩家ID
    player_name = "",          % 玩家名称
    new_accid = 0,             % 新平台帐号ID
    new_accname = "",          % 新平台帐号
    data_sync_flag = 0,        % 数据同步标记
    line = 0,                  % 分配线路
    port = 0,                  % 线路端口
    match_time  = 0,           % 比赛时间
    match1_eliminate_flag = 0, % 初赛淘汰标识
    match_subtype = 0,         % 比赛子类型
    eliminate_rank = 0,        % 淘汰名次
    match1_enter_flag = 0,     % 初赛进入标识
    match1_score = 0,          % 初赛分数
    match2_score = 0,          % 决赛分数
    match1_rank = 0,           % 初赛排名
    match2_rank = 0,           % 决赛排名
    match1_zone = 0,           % 初赛分区
    match1_group = 0,          % 初赛分组
    match1_scene_id = 0,       % 初赛场景ID
    combat_power = 0,          % 战斗力
    exp = 0,                   % 经验
    llpt = 0,                  % 历练声望
    sex = 0,                   % 性别
    realm = 0,                 % 国家
    career = 0,                % 职业
    level = 0,                 % 等级
    zsxz_num = 0,              % 战神勋章个数
    wsxz_num = 0,              % 武圣勋章个数
    jjxz_num = 0,              % 竞技勋章个数
    wssb_num = 0,              % 无双神兵个数
    zsxz_bag_num = 0,          % 武圣勋章礼盒
    wsxz_bag_num = 0,          % 武神勋章礼盒
    canyu_bag_num = 0,         % 参与礼盒
    match1_kill_num = 0,       % 初赛杀敌数
    match2_kill_num = 0,       % 决赛杀敌数
    match1_lucky_flag = 0,     % 初赛幸运标记
    exp_add_lasttime = 0,      % 经验最后获得时间
    score_add_lasttime = 0,    % 擂台积分增加最后时间
    wssb_collect_starttime = 0,% 无双神兵开始采集时间
    wssb_get_lasttime = 0,     % 无双神兵最后获得时间
    zsxz_add_lasttime = 0,     % 战神勋章最后获得时间
    dibang_wsxz_collect_starttime = 0,% 地榜武圣勋章开始采集时间
    dibang_wsxz_get_lasttime = 0,     % 地榜武圣勋章最后获得时间
    dibang_zwxz_add_lasttime = 0,     % 地榜真武勋章最后获得时间
    match1_group_rank = 0,     % 初赛分组排名
    conti_win_num = 0,         % 连斩数
    last_win_time = 0,         % 最后杀人时间
    last_lose_time = 0,        % 上次被杀时间
    win_list = [],             % 杀人列表
    state = 0,                 % 状态
    state_time = 0,            % 状态时间
    create_time = 0            % 创建时间
}).

%% 跨服联赛状态
-record(ets_kfz_league_state, {
    id           = 1,          % 记录ID
    match_type   = 0,          % 比赛类型
    next_match_type = 0,       % 下场比赛类型
    match_state  = 0,          % 比赛状态
    state_time   = 0,          % 状态时间
    haixuan_round = 0          % 海选轮数
}).

%% 跨服联赛队伍
-record(ets_kfz_league_team, {
    team_id      = "",        % 队伍ID
    team_name    = "",        % 队伍名称
    team_member  = [],        % 小组赛队伍列表
    platform     = "",        % 平台
    server_id    = 0,         % 服务器
    update_time  = 0          % 更新时间
}).

%% 跨服联赛场景
-record(ets_kfz_league_scene, {
    id = 0,                    % 场景ID
    scene_type_id = 0,         % 场景类型ID
    match_type = 0,            % 比赛类型
    match_zone = 0,            % 比赛分区
    match_state = 0,           % 比赛状态
    state_time = 0,            % 状态时间
    pid = 0                    % 战场进程ID
}).

%% 跨服联赛观赛
-record(ets_kfz_league_watch, {
    id          = 0,           % 玩家ID
    match_type  = 0,           % 比赛类型
    team_id     = 0            % 队伍ID
}).

%% 跨服联赛结果
-record(ets_kfz_league_result, {
    id = 0,                    % 记录ID
    platform = "",             % 平台标识
    server_id = 0,             % 运营服ID
    player_id = 0,             % 玩家ID
    new_player_id = 0,         % 新玩家ID
    player_name = "",          % 玩家名称
    new_accid = 0,             % 新平台帐号ID
    new_accname = "",          % 新平台帐号
    data_sync_flag = 0,        % 数据同步标记
    state = 0,                 % 战场状态
    state_time = 0,            % 战场状态时间
    leader_flag = 0,           % 队长标识
    team_id = 0,               % 队伍ID
    team_name = "",            % 队伍名字
    team_combat_power = 0,     % 队伍战斗力
    member_list = [],          % 成员列表
    match_type = 0,            % 比赛类型
    next_match_type = 0,       % 下场比赛类型
    line = 0,                  % 分配线路
    port = 0,                  % 线路端口
    match_zone = 0,            % 比赛分区
    match_scene_id = 0,        % 战场场景ID
    match_score = 0,           % 比赛分数
    match_rank = 0,            % 比赛名次
    match_group = 0,           % 比赛阵营
    match2_zone = 0,           % 小组赛分区
    match3_number = 0,         % 平台决赛编号
    match4_number = 0,         % 全国决赛编号
    match_eliminate_flag = 0,  % 比赛淘汰标识
    conti_win_num = 0,         % 连斩数
    last_win_time = 0,         % 最后杀人时间
    win_list = [],             % 杀人列表
    collect_treasurebox_info = [], % 宝箱采集信息
    create_time = 0            % 创建时间
}).

%% 跨服天梯排位赛状态
-record(ets_kfz_alliance_state, {
    id           = 1,          % 记录ID
    match_type   = 0,          % 比赛类型
    match_round  = 0,          % 比赛轮数
    next_match_type = 0,       % 下场比赛类型
    next_match_round = 0,      % 下场比赛轮数
    match_state  = 0,          % 比赛状态
    state_time   = 0,          % 状态时间
    champion_list = []         % 冠军列表
}).

%% 跨服天梯排位赛场景
-record(ets_kfz_alliance_scene, {
    id = 0,                    % 场景ID
    scene_type_id = 0,         % 场景类型ID
    match_type = 0,            % 比赛类型
    match_round = 0,           % 比赛轮次
    match_zone = 0,            % 比赛分区
    match_state = 0,           % 比赛状态
    state_time = 0,            % 状态时间
    pid = 0                    % 战场进程ID
}).

%% 跨服天梯排位赛结果
-record(ets_kfz_alliance_result, {
    id = 0,                    % 唯一标识
    platform = "",             % 平台标识
    server_id = 0,             % 运营服ID
    player_id = 0,             % 玩家ID
    player_name = "",          % 玩家名称
    accname = "",              % 平台账号
    new_player_id = 0,         % 新玩家ID
    new_accid = 0,             % 新平台帐号ID
    new_accname = "",          % 新平台帐号
    data_sync_flag = 0,        % 数据同步标记
    data1_sync_flag = 0,       % 全国数据同步标记
    match_eliminate_flag = 0,  % 比赛淘汰标识
    state = 0,                 % 战场状态
    state_time = 0,            % 战场状态时间
    match_type = 0,            % 当前比赛类型
    match_round = 0,           % 当前比赛轮次
    next_match_type = 0,       % 下场比赛类型
    next_match_round = 0,      % 当前比赛轮次    
    line = 0,                  % 分配线路
    port = 0,                  % 线路端口
    match_scene_id = 0,        % 战场场景ID
    level = 0,                 % 等级
    career = 0,                % 职业
    realm = 0,                 % 国家
    combat_power = 0,          % 战斗力
    revive_num = 0,            % 复活次数
    match_score = 0,           % 积分
    match1_score = 0,          % 资格赛积分
    match2_score = 0,          % 排行赛积分
    match3_score = 0,          % 全服排行赛积分
    match1_rank = 0,           % 资格赛排行
    match2_rank = 0,           % 排行赛排行
    match3_rank = 0,           % 全服排行赛排行
    match1_win = 0,            % 资格赛胜场
    match2_win = 0,            % 排行赛胜场
    match3_win = 0,            % 全服排行赛胜场
    match_competitor_time = 0, % 申请匹配时间
    match_conti_win  = 0,      % 连胜场数
    match_conti_lose = 0,      % 连败场数
    match_zone = 0,            % 战区
    match_group = 0,           % 分组
    report_offline_time = 0,   % 举报恶意下线时间
    create_time = 0            % 创建时间
}).

%% 跨服天梯排位赛排名
-record(ets_kfz_alliance_rank, {
    id          = 0,           % 唯一标识{match_type, career, 类型}
    result_list = []           % 结果列表
  }).

%% 跨服天梯排位赛日志
-record(ets_kfz_alliance_log, {
    id = 0,                    % 唯一标识
    match_type = 0,            % 比赛类型
    career = 0,                % 职业
    platform1 = "",            % 平台标识1
    server_id1 = 0,            % 运营服ID1
    player_id1 = 0,            % 玩家ID1
    player_name1 = "",         % 玩家名称1
    new_accname1 = "",         % 新平台帐号1
    match_score1 = 0,          % 积分1
    realm1 = 0,                % 国家1
    match_round1  = 0,         % 轮次1
    win_flag = 0,              % 胜负标识
    platform2 = "",            % 平台标识
    server_id2 = 0,            % 运营服ID2
    player_id2 = 0,            % 玩家ID2
    player_name2 = "",         % 玩家名称2
    new_accname2 = "",         % 新平台帐号2
    match_score2 = 0,          % 积分2
    realm2 = 0,                % 国家2
    match_round2  = 0,         % 轮次2
    create_time = 0            % 创建时间
}).

%% 跨服BOSS状态信息
-record(ets_kfz_boss_state_info, {
    battle_time  = 0,          % 战场时间
    battle_state = 0,          % 战场状态
    state_time   = 0,          % 状态时间
    state_interval = 0         % 状态间隔
}).

-record(ets_kfz_3v3_result, {
    id = 0,                    % 记录ID
    combat_power_total = 0,    % 队伍战斗力
    platform = "",             % 平台标识
    server_id = 0,             % 运营服ID
    player_id = 0,             % 玩家ID
    new_player_id = 0,         % 新玩家ID
    player_name = "",          % 玩家名称
    new_accid = 0,             % 新平台帐号ID
    new_accname = "",          % 新平台帐号
    data_sync_flag = 0,        % 数据同步标记
    line = 0,                  % 分配线路
    port = 0,                  % 分配端口
    scene_id = 0,              % 战场场景
    combat_power = 0,          % 战斗力
    kfz_score = 0,             % 跨服战积分
    score = 0,                 % 积分
    exp = 0,                   % 经验
    llpt = 0,                  % 历练声望
    result = 0,                % 战场结果
    state = 0,                 % 战场状态
    state_time = 0,            % 状态时间
    award_gettime = 0,         % 奖励领取时间
    leader_flag = 0,           % 队长标志
    team_id = "",              % 队伍标识
    create_time = 0,           % 创建时间
    kill_num = 0,              % 杀人数
    bekill_num = 0,            % 被杀数
    level = 0,                 % 等级
    combat_power_list = [],    % 战斗力列表
    level_list = [],           % 等级列表
    win_lose_list = [],        % 胜利失败场数列表
    team_type = 0,             % 队伍类型
    kfz_team_win = 0,          % 战队胜利场数
    kfz_team_lose = 0,         % 战队失败场数

    kfz_medal_lv_average = 0,  % 3v3竞技勋章平均等级
    medal_3v3_exp = 0,         % 3v3竞技勋章经验

    kfz_team_id = 0,           % 跨服战逻辑服战队ID

    kfz_jt  = 0,               % 3v3跨服占领祭坛次数
    kfz_zg  = 0,               % 3v3跨服助攻次数
    kfz_zj = 0,                % 3v3跨服战战绩
    kfz_mvp =0,                % 3v3跨服战MVP

    daily_reported_count = 0,  % 当日累计被举报次数
    debuff_time = 0,           % 得到debuff的时间
    reported_count = 0,        % 当场累计被举报次数
    allocated_score = 0,       % 举报分配的积分，负数表示扣除，正数表示奖励

    daily_win_times=0          % 当日胜利场数
}).

%跨服战3v3场景表
-record(ets_kfz_3v3_scene, {
                            battle_scene_id = 0,  % 战场副本场景ID
                            battle_state = 0,     % 战场状态(0空闲 1准备 2开始 3结束)
                            battle_pid = 0,       % 战场状态机进程ID
                            battle_start_time = 0,% 战场开始时间

                            red_team = [],        %红队玩家
                            red_team_id = "",     %红队玩家队伍ID
                            red_score = 0,        %红队跨服积分
                            red_zhanling = 0,     %红队占领值
                            red_metal_lv = 0,     %红队勋章等级

                            blue_team = [],       %蓝队玩家
                            blue_team_id = [],    %蓝队玩家队伍ID
                            blue_score = 0,       %蓝队跨服积分
                            blue_zhanling = 0,    %蓝队占领值
                            blue_metal_lv = 0,    %蓝队勋章等级

                            battle_time = 0       %战斗时间
                            }).

%% 跨服BOSS帮派结果
-record(ets_kfz_boss_guild_result, {
    id = 0,                    % 记录ID
    platform = "",             % 平台标识
    server_id = 0,             % 运营服ID
    guild_id = 0,              % 帮派ID
    guild_name = "",           % 帮派名称
    match_time  = 0,           % 比赛时间
    state = 0,                 % 状态
    state_time = 0,            % 状态时间
    line = 0,                  % 分配线路
    port = 0,                  % 线路端口
    scene_id = 0,              % 战场场景ID
    combat_power = 0,          % 战斗力
    contribution = 0,          % 贡献
    kfz_boss_contribution = [],% 贡献记录
    kfz_boss_conti_win_num = 0,% 连胜次数
    kfz_boss_conti_lose_num= 0,% 连败次数
    create_time = 0            % 创建时间
}).

%% 跨服BOSS玩家结果
-record(ets_kfz_boss_result, {
    id = 0,                    % 记录ID
    platform = "",             % 平台标识
    server_id = 0,             % 运营服ID
    player_id = 0,             % 玩家ID
    new_player_id = 0,         % 新玩家ID
    player_name = "",          % 玩家名称
    guild_id = 0,              % 帮派ID
    guild_name = "",           % 帮派名称
    match_time  = 0,           % 比赛时间
    new_accid = 0,             % 新平台帐号ID
    new_accname = "",          % 新平台帐号
    data_sync_flag = 0,        % 数据同步标记
    combat_power = 0,          % 战斗力
    create_time = 0,           % 创建时间
    %New DEF
    kfz_member_role_contribution = 0,      %玩家个人贡献值

    kfz_member_battle_line = 0,            %战场线路
    kfz_member_battle_port = 0,            %战场端口
    kfz_member_battle_scene = 0,           %战场场景ID
    kfz_member_battle_start_time = 0,      %战场开始时间
    kfz_member_battle_result = 0,          %战场结果
    kfz_member_battle_sg = 0,              %战场阵营

    kfz_member_role_reward_exp = 0,        %玩家奖励-经验奖励
    kfz_member_role_reward_llpt = 0,       %玩家奖励-声望奖励
    kfz_member_role_reward_bcoin = 0,      %玩家奖励-铜钱(绑)奖励
    kfz_member_role_reward_boss_score = 0, %玩家奖励-BOSS积分奖励
    kfz_member_role_reward_goods = [],     %玩家奖励-物品奖励奖励

    kfz_member_role_kill_list = []         %跨服战玩家PK列表[{roleid, time}……]
}).

%% 跨服玩家
-record(ets_kfz_player, {
    id          = 0,         % 角色ID
    platform    = "",        % 平台标识
    server_id   = 0,         % 服务器ID
    player_name = "",        % 角色名称
    new_player_id = 0,       % 新角色ID
    new_accid   = 0,         % 新平台帐号ID
    new_accname = "",        % 新平台帐号名称
    combat_power= 0,         % 战斗力
    base_info   = "",        % 角色信息
    online      = 0,         % 在线标志
    update_time = 0          % 更新时间
}).

%% 跨服好友
-record(ets_kfz_friend, {
    player_id1  = 0,         % 玩家ID1
    player_id2  = [],        % 玩家ID2
    update_time = 0          % 更新时间
}).

%% 跨服仇人
-record(ets_kfz_enemy, {
    player_id1  = 0,         % 玩家ID1
    player_id2  = [],        % 玩家ID2
    update_time = 0          % 更新时间
}).

%% 队伍暂离成员列表
-record(ets_tmb_offline, {
        id = 0,             %% 角色id
        team_pid = none,    %% 组队进程pid
        offtime = 0,        %% 离线时间
        dungeon_scene = 0,  %% 离开时副本的场景id
        dungeon_pid = none, %% 副本进程
        td_pid = 0,         %% 塔防管理进程
        dungeon_begin_sid = 0  %% 副本刚开始id
    }).

%%队伍资料
-record(team,
    {
        leaderid = 0,           %% 队长id
        leaderpid= none,        %% 队长pid
        teamname = [],          %% 队名
        member = [],            %% 队员列表
        dungeon_pid=none,       %% 副本进程id
        free_location = [],     %% 空闲位置
        distribution_type = 0,  %% 拾取模式(0:自由拾取 1:随机拾取 2:轮流拾取)
        turn = 0,               %% 轮流标记,初始为自由拾取
        dungeon_scene = 0,      %% 副本场景id
        %drop_choosing_l = [],   %% 掉落包正在捡取列表(保留10个)
        %drop_choose_success_l = [],  %% 掉落包捡取成功列表(保留10个)
        join_type = 2,                %% 1:不自动，2:自动
        create_type = 0,              %% 0:普通创建；2:副本创建
        create_sid = 0,               %% 副本创建时的副本地图id
        goto_dungeon = [],            %% 传送到副本区标志
        is_allow_mem_invite = 0,      %% 是否允许队员邀请组队
        arbitrate = [0, 0, 0, 0, [], 0]  %% 队伍仲裁记录, [记录号，类型，赞成次数，反对次数， 已投票队员id]
    }
).

%%队员数据
-record(mb,
    {
        id = 0,         %% 队员id
        pid = none,     %% 队员pid
        nickname = [],  %% 队员名字
        location = 0,   %% 队员所处位置
        lv = 0,         %% 队员等级
        career = 0,     %% 队员职业
        sht_exp = {0, 1},   %% {关系人id, 额外经验}
        team_friend_buff = []
    }
).

%% 队伍缓存
-record(ets_team, {
        team_pid = 0,
        mb_num = 0,
        join_type = 0
    }).

%% 队伍招募宣言
-record(ets_team_enlist, {
        id = 0,
        name = <<"">>,
        career = 0,
        lv = 0,
        online_flag = 0,
        type = 0,
        sub_type = 0,
        low_lv = 0,
        high_lv = 0,
        lim_career = 0,
        sex = 0,
        leader = 0,
        msg = <<"">>
    }).

%% 副本数据
-record(dungeon,
    {
        id = 1,              %% 副本id
        name = <<"">>,       %% 副本名称
        def = 0,             %% 进入副本的默认场景
        npc = 0,             %% npcid
        time = 0,            %% 限制时间
        count = 0,           %% 次数
        out = {0, 0, 0},     %% 传出副本时场景和坐标{场景id, x, y}
        condition = [],      %% 副本进入条件
        scene = [],          %% 整个副本所有的场景 {场景id, 是否激活}  只有激活的场景才能进入
        requirement = []     %% 场景的激活条件    [影响场景, 是否完成, kill, npcId, 需要数量, 现在数量]
    }
).

%% 经脉属性数据
-record(ets_meridian_attribute, {
        id = 0,
        mer_type_id              = 0, % 经脉id
        mer_level                = 0, % 经脉等级

        mer_crit        = 0,          % 经脉属性
        mer_ten         = 0,
        mer_hit         = 0,
        mer_shun        = 0,
        mer_att         = 0,
        mer_def         = 0,
        mer_hp_mp       = 0,
        mer_kang        = 0,

        mer_uplevel_goods_type   = 0,  % 升级物品类型
        mer_uplevel_goods_num    = 0,  % 升级物品数量
        meridian_needlevel       = 0,  % 经脉开启需求等级
        meridian_needhonor       = 0,  % 经脉开启需求历练
        meridian_uplevel_rewards = 0,  % 升级奖励
        meridian_uplevel_time    = 0,  % 升级需求时间
        meridian_uplevel_money   = 0,  % 升级需求铜币

        conditions = []
    }).

%% 系统公告表
-record(ets_notice, {
        id = 0,             %% 编号
        type=0,             %% 公告类型，0为系统公告，1为系统信息，2为系统公告和系统信息
        color,              %% 公告颜色
        content,            %% 公告内容
        url,                %% 内容链接
        num,                %% 总发送次数，0为不限
        span=0,             %% 时间间隔
        start_time=0,       %% 开始时间
        end_time=0,         %% 结束时间
        status=0            %% 状态，0为未完成，1为已完成
    }).

%% 每天记录
-record(ets_daily, {
        id            = {0, 0},  %% {角色id, 类型}
        count        = 0,          %% 数量
        refresh_time = 0          %% 最后修改时间
    }
).

%% 计数器
-record(ets_counter, {
        type = null,        %% 编号
        value = 0           %% 公告内容
    }).

%% 挂售列表
-record(ets_sell, {
        id = 0,                     %% 编号
        class1 = 0,                 %% 挂售分类 - 大类
        class2 = 0,                 %% 挂售分类 - 小类
        gid = 0,                    %% 物品ID
        pid = 0,                    %% 角色ID
        nickname = <<>>,
        accname = <<>>,
        goods_id = 0,               %% 物品类型ID
        goods_name = <<>>,          %% 物品名称
        num = 0,                    %% 数量
        type = 0,                   %% 物品类型
        subtype = 0,                %% 物品子类型
        lv = 0,                     %% 物品等级
        lv_num = 0,                 %% 等级分段
        color = 0,                  %% 颜色
        career = 0,                 %% 职业
        price_type = 0,             %% 价格类型
        price = 0,                  %% 价格
        time = 0,                   %% 时长，单位：小时
        end_time = 0,               %% 结束时间
        is_expire = 0,              %% 是否过期，1是
        expire_time = 0             %% 过期结束时间
    }).

%% 求购列表
-record(ets_wtb, {
        id = 0,                     %% 编号
        class1 = 0,                 %% 求购分类 - 大类
        class2 = 0,                 %% 求购分类 - 小类
        pid = 0,                    %% 角色ID
        goods_id = 0,               %% 物品类型ID
        goods_name = <<>>,          %% 物品名称
        num = 0,                    %% 数量
        type = 0,                   %% 物品类型
        subtype = 0,                %% 物品子类型
        lv = 0,                     %% 物品等级
        lv_num = 0,                 %% 等级分段
        color = 0,                  %% 颜色
        career = 0,                 %% 职业
        prefix = 0,                 %% 前缀
        stren = 0,                  %% 强化
        price_type = 0,             %% 价格类型
        price = 0,                  %% 价格
        time = 0,                   %% 时长，单位：小时
        end_time = 0                %% 结束时间
    }).

%% BUFF状态表
-record(ets_buff, {
        id=0,                       %% 编号
        pid=0,                      %% 角色ID
        type=0,                     %% BUFF类型，1 经验卡，2 BUFF符, 3烧酒
        goods_id=0,                 %% 物品类型ID
        attribute_id=0,             %% 属性类型ID
        value=0,                    %% 属性值 decimal(10,3)
        end_time=0,                 %% 结束时间戳
        scene = []                  %% 场景限制
    }).

%% 玩家经脉
-record(ets_player_meridian,
        {
            id=0,                      % 玩家id
            mer_ren=0,                 % 任脉等级
            mer_du=0,                  % 督脉等级
            mer_chong=0,               % 冲脉等级
            mer_dai=0,                 % 带脉等级
            mer_yinwei=0,              % 阴维等级
            mer_yangwei=0,             % 阳维等级
            mer_yinqiao=0,             % 阴翘等级
            mer_yangqiao=0,            % 阳翘等级

            meridian_uplevel_typeId=0, % 升级中的经脉类型
            meridian_uplevel_time=0,   % 升级结束时间戳

            mer_ren_g=0,               % 任脉根骨等级
            mer_du_g=0,
            mer_chong_g=0,
            mer_dai_g=0,
            mer_yinwei_g=0,
            mer_yangwei_g=0,
            mer_yinqiao_g=0,
            mer_yangqiao_g=0,

            wisdom_ren=0,              % 任脉慧根等级
            wisdom_du=0,
            wisdom_chong=0,
            wisdom_dai=0,
            wisdom_yinwei=0,
            wisdom_yangwei=0,
            wisdom_yinqiao=0,
            wisdom_yangqiao=0,
            wisdom_last_up_time=0,     % 慧根上次升级时间

            mer_ren_tupo=0,            % 任脉经脉突破等级
            mer_du_tupo=0,
            mer_chong_tupo=0,
            mer_dai_tupo=0,
            mer_yinwei_tupo=0,
            mer_yangwei_tupo=0,
            mer_yinqiao_tupo=0,
            mer_yangqiao_tupo=0
        }).

%% 聊天信息（小喇叭）
-record(ets_horn_msg, {
        msg_id = 0,         %% 以msg_id=0的记录的player_id字段保存下一Id
        player_id = 0,
        player_name = "",
        realm = 0,
        career = 0,
        sex = 0,
        gm = 0,
        vip = 0,
        image = 0,          %% 角色头像
        line = 0,           %% 线路，发送消息时再查询
        color = 0,          %% 文字颜色
        msg = "",           %% 消息内容
        msg_type = 0,
        show_names = [],
        platform = <<>>,
        server_id = 0
    }).

%%交易市场状态
-record(sell_status, {
        totals = 0,         % 总记录数
        caches = []         % 无条件查询的前12条记录
    }).

%%怪物召唤
-record(mon_call, {
        goods_id = 0,
        boss_id = 0,
        call_scene = 0,
        call_x_rand = 0,
        call_y_rand = 0,
        born_x_y = 0,
        livingtime=0
        }).
	
%% IP黑名单
-record(ets_ip_limit, {
        ip = [],
        time = 0
        }).

%%buff温泉
-record(ets_buff_watar, {
        id = 0,             %%ID
        type = 0,             %%温泉类型
        bufftype = 0,         %%buff类型
        buffnum = 0,          %%buff量/次
        buff_rand = 0,        %%buff范围
        opentime = [],        %%开启时间[{StartTime1,EndTime1},{StartTime2,EndTime2}]
        needbuff = 0,         %%需求buff类型
        born_scene = 0,       %%诞生场景
        born_x = 0,           %%诞生坐标-x
        born_y = 0,           %%诞生坐标-y
        born_npc = 0,         %%诞生NPC

        borntime = 0,         %%温泉诞生时间
        last_atime = 0,       %%温泉上次活动时间
        notice = 0,           %%是否公告
        notice_content = [],  %%公告内容
        pid = 0,              %%进程ID
        flower_time = 0
        }).

%% 采矿
-record(collector, {
        id,                     %% 角色Id
        mon_id = 0,             %% 矿点Id，0表示未采矿
        start_time = 0,         %% 开始采矿时间
        last_time = 0,          %% 上一次掉落时间
        fail_times = [],        %% 各矿物失败次数([{GoodsTypeId, FailTimes}, ...])
        bag_num = 35,           %% 采矿背包格子数，可扩展
        bag = [],               %% 采矿背包（[{GoodsTypeId, Num}, ...]）
        prof = 10               %% 采矿熟练度
    }).

%% 矿点
-record(lode, {
        id,                     %% 矿点Id
        mid,                    %% 怪物类型Id
        roles = [],             %% 玩家Id列表, [{PlayerId1, Pid1}, {PlayerId2, Pid2}, ...]
        created_mons = []       %% 由这个矿点中生成的怪物列表[{MonTypeId, MonIdList}, ...]
    }).

%% 先前线路记录
-record(before_line, {
        accname = 0,
        line = 0,
        scene = 0,
        x = 0,
        y = 0
    }).

%% 验证码图片数据
-record(captcha_image, {
        id = 0,
        code = <<>>,        %% 验证码
        image_data = <<>>,  %% 图片数据
        ctime = 0           %% 图片生成时间
    }).

%% 验证码
-record(captcha, {
        id = <<>>,          %% Id = list_to_binary(lists:concat([PlayerId, Type]))，Type = string()
        player_id = 0,      %% 玩家id
        type = 0,           %% 验证码类型
        timeout = 0,        %% 本验证码超时的时间戳
        code = <<>>,        %% 验证码
        errorcount = 0,     %% 记录输错次数
        is_pass = false     %% 是否通过验证
    }).

%% 二级密码
-record(secondary_password, {
        id = 0,             %% 角色Id
        is_pass = false,    %% 是否通过验证
        error_times = 0,    %% 输错次数
        question1 = 0,      %% 问题1 Id
        question2 = 0,      %% 问题2 Id
        answer1 = <<>>,     %% 答案1 BinString
        answer2 = <<>>,     %% 答案2 BinString
        password = <<>>     %% 二级密码 BinString
    }).

%% 锁妖塔各层霸主表
-record(ets_tower_master, {
        sid = 0,             %% 场景id
        players = [],        %% 霸主
        passtime = 0,        %% 完成时间
        reward = [],         %% 已经领取奖励的霸主
        tower_name = []      %% 塔名
    }).

%% 经验累积返还
-record(ets_task_cumulate, {
        id          = {0, 0},       %% {角色id, 任务id}
        role_id     = 0,            %% 任务类型
        task_id     = 0,            %% 任务类型
        task_name   = <<>>,         %% 任务类型
        type        = 0,            %% 任务类型
        exp         = 0,            %% 任务经验
        bcoin       = 0,            %% 绑定铜钱
        ratio       = 0,            %% 累积加成百分比
        gold        = 0,            %% 元宝数
        goods_id    = 0,            %% 物品ID
        goods_num   = 0,            %% 物品数量
        time        = 0,            %% 最后更新时间，零点时间戳
        day         = 0,            %% 可领取累积天数
        status      = 0             %% 领取状态，1已领取
    }).

%% 锁妖塔配置
-record(tower, {
        sid = 0,            %% 该层资源id
        time = 0,           %% 该层限制时间
        exp = 0,            %% 每层的经验
        level = 0,          %% 层数
        llpt = 0,           %% 每层的历练声望
        silver = 0,         %% 每层的绑定元宝
        items = [],         %% 每层的奖励物品
        total_exp = 0,      %% 累计到该层的经验
        total_llpt = 0,     %% 累计到该层的历练声望
        total_silver = 0,   %% 累计到该层的绑定元宝
        total_items = [],   %% 累计到该层的奖励物品
        master_exp = 0,     %% 该层霸主每天能领取的经验
        master_llpt = 0,    %% 该层霸主每天能领取的历练声望
        be_master = 0,      %% 是否设置霸主
        honour = 0,         %% 荣誉
        total_honour = 0,   %% 累计荣誉
        king_honour = 0,    %% 帝王谷荣誉
        total_king_honour = 0  %% 累计帝王谷荣誉
    }).

%% 锁妖塔怪物配置
-record(tower_mon,{
        id = 0,             %% 怪物id
        time = 0            %% 增加时间
    }).

%% 篝火木材配置
-record(ets_fire_wood, {
    team_pid = 0,        %% 作用的队伍进程id
    npc_id= 0,           %% 篝火NPCid
    role_id =0,          %% 添加角色ID
    buff_value = 0,      %% 作用效果
    end_time = 0         %% 到期时间
    }).

%离线修炼托管
-record(practice_outline, {
    id = 0,
    role_id = 0,
    practice_time = 0,
    time = 0
    }).

%烧酒离线托管
-record(wine_outline, {
    id = 0,
    last_wine_time = 0,
    exp_days = 0
    }).

%% 时间控制
-record(ets_time_control, {
    id = 0,
    type = 0,
    ratio = 0,
    num = 0,
    list = [],
    hour_start = 0,
    hour_end = 0,
    time_start = 0,
    time_end = 0
    }).

%离线修炼
-record(practice, {
    role_id         = 0     %% 角色id
    ,time           = 0     %% 修炼总时长
    ,quick_time     = 0     %% 加速时间【加速之后时间先减去每次加速时间】
    ,begin_time     = 0     %% 开始修炼时间
    ,end_time       = 0     %% 预计结束时间
    ,status         = 0     %% 状态，0=已完成，1=修炼中
    ,is_gold        = 0     %% 是否金币修炼
    ,level          = 0     %% 开始修炼时的角色的等级
    ,pause_time     = 0     %% 暂停修炼时间
}).

%% 仙侣奇缘
-record(ets_appointment, {
        id = 0,                 %% 玩家id
        now_partner_id = 0,     %% 现在的伴侣
        candle = [0, 0],        %% 蜡烛的位置
        begin_time = 0,         %% 仙侣奇缘开始时间
        last_exp_time = 0,      %% 仙侣奇缘上次加经验的时间
        state = 0,              %% 仙侣奇缘状态
        candle_id = 0,          %% 小蜡烛唯一npcid
        now_question_id = 0,    %% 现在题目id
        answer = [],            %% 答题答案
        score = 0,              %% 默契度
        continute = 0,          %% 连续答对题数
        special_pass_count = 0, %% 随机题目没出现题数
        special_correct = 0,    %% 随机题目答对次数
        sure_times = 0          %% 开启答题选择次数(0号题目)
    }).

%% 仙侣奇缘配置
-record(ets_appointment_config, {
        id = 0,                 %% 玩家id
        last_partner_id = 0,    %% 上次的伴侣
        now_partner_id = 0,     %% 现在的伴侣
        refresh_time = 0,       %% 上次刷新时间
        rand_ids = [],          %% 系统分配的伴侣
        state = 0,              %% 约会状态(1：邀请方,2：被邀请方)
        recommend_partner = [], %% 红颜知己([id， 名字， 选择次数])
        mark = [],               %% 7个与之约会的伴侣次数统计
        add_recommend_partner = 0 %% 增加红颜知己次数
    }).

%% 仙侣奇缘题目
-record(ets_appointment_subject, {
        id = 0,
        comment = <<>>,
        %comment2 = <<>>,
        option1 = <<>>,
        option2 = <<>>,
        option3 = <<>>,
        option4 = <<>>
        %option5 = <<>>,
        %option6 = <<>>,
        %option7 = <<>>,
        %option8 = <<>>,
        %rela = []
    }).

%% 仙侣奇缘题目
-record(ets_appointment_special_subject, {
        id = 0,
        answer = 1,
        comment = <<>>,
        option1 = <<>>,
        option2 = <<>>,
        option3 = <<>>,
        option4 = <<>>
    }).

-record(ets_falling_stone, {
        id = 0,
        start_time = 0,     %% 活动开始时间
        end_time = 0        %% 结束时间
    }).

%% 副本组队招募
-record(ets_dungeon_enlist, {
        id = 0,
        sid = 0,
        nickname = []
    }).

%% 副本组队招募(8.29)
-record(ets_dungeon_enlist2, {
        id = 0,
        sid = 0,
        line = 0,
        nickname = [],
        is_need_fire = 1,
        is_need_ice = 2,
        is_need_drug = 3,
        lv = 0,
        att = 0,
        def = 0,
        combatpower = 0,
        mb_num = 1
    }).

%% 角色称号
-record(role_achieved_name, {
        id = 0,             %% 角色Id * 1000000 + 称号Id
        role_id = 0,        %% 角色Id
        name_id = 0,        %% 称号Id
        type1 = 0,          %% 类型1
        type2 = 0,          %% 类型2
        is_display = 0,     %% 是否显示(0不显示/1显示)
        get_time = 0        %% 获得时间
    }).

-record(achieved_name, {
        name_id = 0,        %% 称号Id
        name = <<>>,        %% 称号名
        type1 = 0,          %% 称号类型1
        type2 = 0,          %% 称号类型2
        sex_limit = 0,      %% 性别限制（0不限）
        display = 0,        %% 显示类型（0不可显示/1可显示）
        max_show_time = 0,  %% 最大显示时间
        chat_show = 0,      %% 是否在聊天窗口显示称号（0不显示/1显示）
        notice = 0,         %% 通知方式（0不通知/1提示/2传闻）
        swf_show = 0,       %% 是否Flash效果（0否/1是）
        describe = <<>>,    %% 达成条件描述
        required = 0,       %% 获得称号的需求
        hp = 0,             %% 气血
        mp = 0,             %% 内力
        att = 0,            %% 攻击
        def = 0,            %% 防御
        hit = 0,            %% 命中
        dodge = 0,          %% 闪避
        crit = 0,           %% 暴击
        ten = 0,            %% 坚韧
        res = 0             %% 全抗
    }).

%% "我要进入副本"列表
-record(ets_want_enter_dungeon, {
        id = 0,         %% 玩家id
        sid = 0,        %% 副本id
        nickname = [],  %% 玩家名字
        sex = 0,        %% 性别
        career = 0,     %% 职业
        lv = 0          %% 等级
    }).

%% 成就配置
-record(base_chengjiu, {
        id = 0,             %% 成就ID
        type = 0,           %% 类型
        type_id = 0,        %% 类型ID，如物品ID，任务ID...
        type_list = 0,      %% 类型ID列表，如物品ID列表，任务ID列表...
        lim_num = 0,        %% 限制数量
        is_count = 0,       %% 是否要统计
        count_span = 0,     %% 统计间隔
        ratio = 0,          %% 机率
        name_id = 0,        %% 称号
        cjpt = 0            %% 成就声望
    }).

%% 成就奖励配置
-record(base_chengjiu_award, {
        id = 0,             %% 奖励ID
        lim_list = [],      %% 成就限制
        lim_chengjiu = 0,   %% 成就声望限制
        attr_id = 0,        %% 奖励属性ID
        attr_value = 0,     %% 奖励属性值
        goods_id = 0,       %% 奖励物品类型ID
        goods_num = 0,      %% 奖励物品数量
        bind = 0            %% 绑定状态
    }).

%% 成就列表
-record(dict_chengjiu, {
        %id = {0,0},         %% {角色ID,成就ID}
        id = 0,             %% 成就ID
        type = 0,           %% 成就类型
        count = 0,          %% 统计数量
        time = 0            %% 完成时间
    }).

%% 成就奖励列表
-record(ets_chengjiu_award, {
        role_id = 0,        %% 角色ID
        cjpt = 0,           %% 成就声望
        award_list = []     %% 已领取奖励列表，[{奖励ID,完成时间},...]
%        id = {0,0},         %% {角色ID,奖励ID}
%        time = 0            %% 完成时间
    }).

%% 角色成就声望列表
%-record(player_chengjiu, {
%        id = 0,             %% 角色ID
%        cjpt = 0            %% 成就声望
%    }).

%% 每日成就
-record(ets_target_day, {
        id = 0,         %% 玩家id
        finish_num = 0, %% 完成的事件
        event = [],     %% 事件内容[{事件标识, '物品/怪物/任务..id, 累计次数, 完成条件}]
        reward = [],    %% 领取奖励事件数
        time = 0,       %% 领取时间
        today_events=[] %% 今天随机到的每日任务列表
    }).

%% 周目标每日目标
-record(ets_target_week_task, {
        id = 0,                 %% 玩家id
        event_id = 0,           %% 当前事件
        fin_num = 0,            %% 完成数量
        fin_times = 0,          %% 今天完成目标次数
        fin_event_ids = [],     %% 今天已经完成的目标
        refresh_time = 0,       %% 刷新时间
        reward = 0              %% 是否已领取今天奖励
    }).

%% 周目标
-record(ets_target_week, {
        id = 0,         %% 玩家id
        score = 0,      %% 积分
        gold_freeback = 0,      %% 本周充值元宝
        reward_freeback = [],   %% 本周领取了的奖励
        time = 0,       %% 更新时间
        reward = [],    %% 领取的礼包id
        lim_num = 0,    %% 每周限制兑换次数
        lim_kfz_terr = 0 %% 领土战积分当前周已兑换次数
    }).

%% 连续登录 及 间隔登录（用于连续登录礼包及回归礼包）
-record(ets_login_counter, {
        id = 0,                         %% 玩家Id
        last_login_time = 0,            %% 最后登录时间
        last_logout_time = 0,           %% 上次退出时间
        continuous_days = 0,            %% 连续登录天数
        no_login_days = 0,              %% 未登录天数
        last_correct_time = 0,          %% 上次修正连续登录天数的对应午夜时间
        continuous_gift = [],           %% 连续登录礼包列表
        no_login_gift = [],             %% 回归登录礼包列表
        reset_time = 0,                 %% 重置连续登录天数的午夜时间
        is_charged = 0,                 %% 是否充值过
        version = 0                     %% 数据版本
    }).

%% 题库
-record(ets_quiz, {
        id = 0,                         %% 玩家Id
        content= "",
        correct=1,
        option1="",
        option2="",
        option3="",
        option4=""
    }).

%% 题库
-record(ets_quiz_s, {
        id = 0,                         %% 玩家Id
        content= "",
        correct=1,
        option1="",
        option2="",
        option3="",
        option4=""
    }).

%% 塔防积分
-record(ets_td_score, {
        scene=0,                 %场景ID
        monmgrpid=0,             %塔防服务进程ID
        tdmgrpid=0,              %塔防管理进程ID
        totalscore=0,            %本场总积分
        leftscore=0,             %本场剩余积分
        buff = [],               %技能buff
        last_add_time = 0,       %上次增加积分时间
        add_score_times = 0,     %公主祝福次数
        gz_add_hp_time = 0       %上次公主加血时间
        }).

%% 运势配置
-record(base_fortune, {
        id = 0,                     %% 运势编号
        name = <<>>,                %% 运势名称
        star = 0,                   %% 星星数量
        color = 0,                  %% 运势颜色
        ratio = 0,                  %% 运势刷新机率
        refresh_num = 0,            %% 刷新颜色次数
        brefresh_num = 0,           %% 被刷新颜色次数
        refresh_span = 0,           %% 刷新颜色时长，单位：秒
        refresh_ratio = []          %% 刷新颜色机率，列表[{颜色ID,颜色机率}...]
    }).

%% 运势任务颜色配置
-record(base_fortune_color, {
        color = 0,                  %% 任务颜色
        refresh_span = 0,           %% 刷新时长，单位：秒
        refresh_gold = 0,           %% 刷新任务元宝
        finish_gold = 0,            %% 完成任务元宝
        exp = 0,                    %% 奖励经验
        coin = 0,                   %% 奖励铜钱
        llpt = 0,                   %% 奖励历练声望
        goods_id = 0,               %% 奖励物品ID
        goods_num = 0,              %% 奖励物品数量
        gift_goods = 0,             %% 随机礼包物品ID
        gift_ratio = 0              %% 随机礼包机率
    }).

%% 运势任务配置
-record(base_fortune_task, {
        id = 0,                     %% 运势任务编号
        name = <<>>,                %% 任务名称
        lim_list = [],              %% 限制列表
        lim_num = 0,                %% 限制数量
        is_count = 0                %% 是否统计
    }).

%% 玩家运势信息
-record(ets_fortune, {
        role_id = 0,                %% 角色ID
        fortune_id = 0,             %% 运势ID
        color = 0,                  %% 任务颜色
        refresh_num = 0,            %% 可刷新颜色次数
        brefresh_num = 0,           %% 被刷新颜色次数
        task_id = 0,                %% 任务ID
        count = 0,                  %% 任务统计数
        refresh_task = 0,           %% 刷新的任务
        refresh_time = 0,           %% 下次刷新时间
        cdate = 0,                  %% 当天日期
        status = 0                  %% 任务完成状态，0未接取，1已接取，2已完成，3已交任务
    }).

%% 玩家运势任务颜色刷新日志
-record(ets_fortune_log, {
        id = 0,                     %% 编号
        role_id = 0,                %% 角色ID
        refresh_role = 0,           %% 刷新角色ID
        refresh_fortune = 0,        %% 刷新者的运势ID
        task_id = 0,                %% 任务ID
        color = 0,                  %% 任务颜色
        rdate = 0                   %% 刷新日期
    }).

%%塔防排行榜-单人
-record(ets_td_rank1, {id = 0, rolelist = [], wave = 0, use_time = 0, do_time = 0}).
%%塔防排行榜-多人
-record(ets_td_rank2, {id = 0, rolelist = [], wave = 0, use_time = 0, do_time = 0}).

%% 活动配置
-record(base_activity, {
        id = 0,                     %% 活动编号
        name = <<>>,                %% 活动名称
        unit_num = 0,               %% 单位数量
        lim_day_num = 0,            %% 每天上限，0为不限
        lim_total_num = 0,          %% 总上限，0为不限
        goods_list = [],            %% 奖励物品列表，[{物品ID,物品数量}...]
        bind = 0,                   %% 奖励物品绑定状态
        send_type = 0,              %% 奖励发放方式，0实时发放，1活动结束后发放
        clean_type = 0,             %% 数据清除类型，0无，1每天清
        stime = 0,                  %% 活动开始时间
        etime = 0,                  %% 活动结束时间
        rtime = 0,                  %% 领取结束时间
        goods_time = 0              %% 物品过期时间
    }).

%% 玩家活动信息
-record(ets_activity, {
        id = {0,0},                 %% {角色Id,活动Id}
        role_id = 0,                %% 角色Id
        activity_id = 0,            %% 活动Id
        count_now = 0,              %% 当前统计数
        count_total = 0,            %% 总统计数
        count_time = 0,             %% 最后统计时间
        send_day = 0,               %% 当天发送奖励数量
        send_total = 0,             %% 总的发送奖励数量
        recv_time = 0,              %% 最后领取时间
        recv_end_time = 0,          %% 领取结束时间
        goods_list = []             %% 物品列表，[{物品ID,物品数量}...]
    }).

%% 坐骑配置
-record(base_mount, {
        id = 0,                     %% 坐骑类型ID
        name = <<>>,                %% 坐骑名称
        grade = 0,                  %% 阶级
        speed = 0,                  %% 速度
        forza = 0,                  %% 力量成长
        wit = 0,                    %% 灵力成长
        agile = 0,                  %% 身法成长
        thew = 0,                   %% 体质成长
        solid = 0,                  %% 坚固成长
        forza_ext = 0,              %% 额外力量
        wit_ext = 0,                %% 额外灵力
        agile_ext = 0,              %% 额外身法
        thew_ext = 0,               %% 额外体质
        solid_ext = 0,              %% 额外坚固
        stren0_figure = 0,          %% 坐骑缺省形像
        stren7_figure = 0,          %% 坐骑强化7形像
        stren10_figure = 0          %% 坐骑强化10形像
    }).

%% 坐骑进阶配置
-record(base_mount_upgrade, {
        mount_id = 0,               %% 坐骑类型ID
        new_mount = 0,              %% 进阶坐骑类型ID
        stuff_id = 0,               %% 材料物品类型ID
        stuff_num = 0,              %% 材料物品数量
        coin = 0,                   %% 消耗铜钱
        lv_lim = 0,                 %% 最小等级限制
        luck = 0,                   %% 失败获得幸运值
        luck_lim = 0,               %% 幸运值上限
        luck_ratio = []             %% 幸运值概率列表
    }).

%% 坐骑技能书配置
-record(base_mount_skill, {
        goods_id = 0,               %% 技能书物品ID
        attribute_id = 0,           %% 属性类型ID
        attribute_ratio = 0,        %% 属性百分比值
        attribute_value = 0,        %% 属性数值
        lv = 0,                     %% 技能等级
        ratio = 0                   %% 机率，学习技能的成功机率
    }).

%% 坐骑还童配置
-record(base_mount_return, {
        goods_id = 0,               %% 物品类型ID
        quality_list = [],          %% 品质随机列表 [{品质,机率},...]
        grow_min_ratio = 0,         %% 单项成长最小取值比例，百分制%
        grow_max_ratio = 0,         %% 单项成长最大取值比例，百分制%
        forza_min_ratio = 0,        %% 力量成长最小取值比例，百分制%
        forza_max_ratio = 0,        %% 力量成长最大取值比例，百分制%
        grade_min_lim = 0,          %% 最小阶级限制
        grade_max_lim = 0,          %% 最大阶级限制
        coin = 0                    %% 消耗铜钱
    }).

%% 坐骑变身配置
-record(base_mount_figure, {
        goods_id = 0,               %% 物品类型ID
        stren0_figure = 0,          %% 变身缺省形像
        stren7_figure = 0,          %% 变身强化7形像
        stren10_figure = 0,         %% 变身强化10形像
        time = 0,                   %% 变身时长
        ratio = 0                   %% 变身效果百分比
    }).

%% 坐骑装备配置
-record(base_mount_equip, {
        lv = 0,                     %% 等级
        exp = 0,                    %% 升级经验
        att = 0,                    %% 攻击
        hp = 0,                     %% 气血
        resistance = 0,             %% 全抗
        hit = 0,                    %% 命中
        dodge = 0,                  %% 躲避
        crit = 0,                   %% 暴击
        ten = 0                     %% 坚韧
    }).

%% 坐骑ETS
-record(ets_mount, {
        id = 0,                     %% 坐骑ID
        name = <<>>,                %% 坐骑名称
        role_id = 0,                %% 角色ID
        type_id = 0,                %% 坐骑类型ID
        figure = 0,                 %% 形像
        lv = 0,                     %% 等级
        exp = 0,                    %% 经验
        grade = 0,                  %% 阶级
        grade_luck = 0,             %% 升阶幸运值
        luck_time = 0,              %% 幸运值刷新时间
        intimacy = 0,               %% 亲密度
        intimacy_time = 0,          %% 亲密度刷新时间
        familiar = 0,               %% 灵犀值
        familiar_total = 0,         %% 总的灵犀值
        quality = 0,                %% 品质
        stren = 0,                  %% 强化
        stren_ratio=0,              %% 强化附加成功率
        stren_his=0,                %% 历史最高强化等级
        stren_fail=0,               %% 历史最高强化等级的失败次数
        speed = 0,                  %% 速度
        forza = 0,                  %% 力量成长
        wit = 0,                    %% 灵力成长
        agile = 0,                  %% 身法成长
        thew = 0,                   %% 体质成长
        solid = 0,                  %% 坚固成长
        attribute = [0,0,0,0,0,0,0,0,0,0,0],  %% 属性值，[Hp, Mp, Att, Def, Hit, Dodge, Crit, Ten, Fire, Ice, Drug]
        combat_power = 0,           %% 战斗力
        nature = 0,                 %% 性格，0平庸，1热血，2认真，3鲁莽，4冷静，5胆小，6强壮
        theurgy = 0,                %% 天赋神通（特殊技能）
        status = 0,                 %% 状态，0休息，1出战，2骑乘
        ctime = 0,                  %% 创建时间
        rtime = 0,                  %% 更新时间
        equip1 = 0,                 %% 装备一，马鞍，物品ID
        equip2 = 0,                 %% 装备二，缰绳，物品ID
        figure_goods = 0,           %% 变身卡物品ID
        figure_change = 0,          %% 变身形像
        figure_time = 0,            %% 变身截止时间
        figure_show = 0,            %% 是否显示变身形像
        figure_ratio = 0,           %% 变身效果百分比值
        batch_return = []           %% 批量还童，[{Quality, Forza, Wit, Agile, Thew, Solid}, ...]
    }).

%% 坐骑技能ETS
-record(ets_mount_skill, {
        id = 0,                     %% 编号
        role_id = 0,                %% 角色ID
        mount_id = 0,               %% 坐骑ID
        attribute_id = 0,           %% 技能属性类型ID
        skill1 = 0,                 %% 技能1
        skill2 = 0,                 %% 技能2
        skill3 = 0,                 %% 技能3
        skill = 0,                  %% 最终技能
        lv = 0,                     %% 最终技能等级
        attribute_ratio = 0,        %% 属性百分比值
        attribute_value = 0,        %% 属性数值
        luck = 0                    %% 幸运值
        ,is_sub = 0                 %% 是否附属技能，0否，1是
        ,dskill = 0                 %% 双属性技能
        ,dskill_lv = 0              %% 双属性技能等级
    }).

%% 坐骑技能ETS
-record(ets_mount_szbf, {
        gid = 0,                    %% 物品ID
        goods_id = 0,               %% 物品类型ID
        role_id = 0,                %% 角色ID
        luck = 0,                   %% 幸运值
        skill_list = []             %% 技能书列表，[技能书ID,...]
    }).

%% 坐骑装备ETS
-record(ets_mount_equip, {
        id = {0,0},                 %% {坐骑ID,装备类型}
        mount_id = 0,               %% 坐骑ID
        type = 0,                   %% 装备类型，1马鞍，2缰绳
        role_id = 0,                %% 角色ID
        lv = 0,                     %% 等级
        exp = 0,                    %% 经验
        att = 0,                    %% 攻击
        hp = 0,                     %% 气血
        resistance = 0,             %% 全抗
        hit = 0,                    %% 命中
        dodge = 0,                  %% 躲避
        crit = 0,                   %% 暴击
        ten = 0                     %% 坚韧
    }).

%% 坐骑变身记录（公共线）
-record(ets_mount_change, {
    role_id,    %% 
    day         %% 0点的时间戳
    }).

%% 夫妻
-record(couple, {
        id = 0,             %% 本服第N对夫妻
        husband = 0,        %% 丈夫角色Id
        wife = 0,           %% 妻子角色Id
        husband_name = <<>>,%% 丈夫角色名
        wife_name = <<>>,   %% 妻子角色名
        marry_time = 0,     %% 结婚登记时间
        couple_level = 0,   %% 夫妻等级
        cruise_time = 0,    %% 迎亲巡游时间
        cruise_times = 0,   %% 迎亲巡游次数
        cruise_finish = 0,  %% 巡游是否完成
        cruise_line = 0,    %% 巡游线路
        free_wedding = 0,   %% 是否已举办免费型婚宴
        wedding_type = 0,   %% 婚宴类型
        wedding_time = 0,   %% 婚宴时间
        wedding_held = 0,   %% 是否已举行过
        a_card_num = 20,    %% 男方喜帖数
        b_card_num = 20,    %% 女方喜帖数
        guests_a = [],      %% 男方宾客
        guests_b = [],      %% 女方宾客
        divorce_due_time=0, %% 离婚申请到期时间
        man_apply = 0,      %% 男方是否申请离婚
        woman_apply = 0,    %% 女方是否申请离婚
        sweet = 0,          %% 甜蜜度
        couple_intimacy = 0 %% 夫妻亲密
    }).

%% 婚姻记事
-record(marriage_event, {
        id = 0,             %% 事件Id
        couple_id = 0,      %% 夫妻Id
        event_time = 0,     %% 事件时间
        event = <<>>        %% 事件内容
    }).

%% 求婚信息（男方求婚加入，女方回应清除，任一方下线清除）
-record(propose, {
        man = 0,            %% 男方Id
        woman = 0,          %% 女方Id
        propose_type = 0    %% 铜币求婚0/道具求婚1
    }).

%% 记录当前婚宴信息
-record(current_wedding, {
        id = 0,             %% 以Id为1来记录当前婚宴
        couple_id = 0,      %% 夫妻Id
        marry_time = 0,     %% 登记时间
        husband = 0,        %% 丈夫角色Id
        wife = 0,           %% 妻子角色Id
        husband_name = <<>>,%% 丈夫角色名
        wife_name = <<>>,   %% 妻子角色名
        man_career = 0,     %% 男方职业
        woman_career = 0,   %% 女方职业
        man_realm = 0,      %% 男方阵营
        woman_realm = 0,    %% 女方阵营
        man_image = 0,      %% 男方头像编号
        woman_image = 0,    %% 女方头像编号
        wedding_type = 0,   %% 婚宴类型
        wedding_time = 0,   %% 婚宴开启时间
        wedding_line = 0,   %% 婚宴线路
        guests_a = [],      %% 男方宾客
        guests_b = [],      %% 女方宾客
        make_address = 0    %% 是否已经致词（0未/1已致词）
    }).

%% 记录婚宴时间安排
-record(wedding, {
        id = 0,             %% CoupleId, #couple.id
        wedding_held = 0,   %% 是否已经举行过
        wedding_time = 0,   %% 婚宴时间
        wedding_line = 0    %% 婚宴线路
    }).

%% 婚宴送礼
-record(wedding_gift, {
        id = 0,             %% 序号
        role_id = 0,        %% 赠送者Id，以Id=0的记录的role_id列记录当前序号
        role_name = <<>>,   %% 赠送者名字
        role_sex = 0,       %% 赠送者性别
        role_career = 0,    %% 赠送者职业
        role_realm = 0,     %% 赠送者阵营
        giving_time = 0,    %% 赠送时间
        coin = 0,           %% 赠送铜币数
        flower = 0,         %% 送花数量
        benediction = <<>>  %% 祝福语
    }).

%% 巡游管理
-record(current_cruise, {
        id = 0,             %% 以Id为1来记录当前巡游队伍
        couple_id = 0,      %% 夫妻Id
        husband = 0,        %% 丈夫角色Id
        wife = 0,           %% 妻子角色Id
        husband_name = <<>>,%% 丈夫角色名
        wife_name = <<>>,   %% 妻子角色名
        man_career = 0,     %% 男方职业
        woman_career = 0,   %% 女方职业
        man_realm = 0,      %% 男方阵营
        woman_realm = 0,    %% 女方阵营
        man_image = 0,      %% 男方头像编号
        woman_image = 0,    %% 女方头像编号
        wedding_type = 0,   %% 婚宴类型
        marry_time = 0,     %% 登记时间
        wedding_time = 0,   %% 婚宴时间
        cruise_time = 0,    %% 巡游时间
        cruise_times = 0,   %% 巡游次数
        cruise_mode = 0,    %% 巡游模式
        cruise_finish = 0,  %% 是否已完成
        cruise_line = 0     %% 巡游线路
    }).

%% 夫妻家园
-record(couple_home, {
        id = 0,             %% 家园Id
        home_name = <<>>,   %% 家园名
        husband = 0,        %% 丈夫Id
        husband_name = <<>>,%% 丈夫名
        wife = 0,           %% 妻子Id
        wife_name = <<>>,   %% 妻子名
        home_level = 1,     %% 家园等级
        buy_time = 0,       %% 购买时间
        frozen_time = 0,    %% 冻结时间
        visitor_num = 20,   %% 限制访客数
        tree_exists = 0,    %% 是否存在树
        tree_type = 0,      %% 情侣树类型
        tree_level = 0,     %% 情侣树等级
        tree_exp = 0,       %% 情侣树经验
        tree_health = 0,    %% 情侣树健康度
        tree_last_grown = 0,%% 情侣树生长时间
        tree_pest = 0,      %% 情侣树虫子数
        manuring_time = 0,  %% 施肥时间
        manuring_times = 0, %% 今天施肥次数
        tree_mon_id = 0,    %% 情侣树怪物Id
        scene_id = 0        %% 场景唯一Id
    }).

%% 家园种植物
-record(home_object, {
        id = 0,
        home_id = 0,
        pos = 0,
        type_id = 0,
        health = 80,
        pest = 0,
        exp = 0,
        max_exp = 0,
        buy_time = 0,
        last_grown = 0,
        mon_id = 0
    }).

%% 夫妻家园访问者名单
-record(home_visitor, {
        home_id = 0,
        role_id = 0
    }).

%% 爱情保险
-record(couple_insure, {
        role_id = 0,    %% 角色Id
        id = 0,         %% 保单Id
        husband = 0,
        wife = 0,
        applicant = 0,  %% 投保人
        cost = 0,
        insure_type = 0,
        insure_time = 0,
        frozen_time = 0,
        man_check_in_time = 0,
        man_check_in_times = 0,
        woman_check_in_time = 0,
        woman_check_in_times = 0,
        man_insurance_bag = [],
        woman_insurance_bag = []
    }).

%% 许愿墙
-record(wishing_paper, {
        id = 0,
        role_id = 0,
        role_name = <<>>,
        content = <<>>,
        time = 0,
        wishing_to = 0,
        wishing_to_name = <<>>
    }).

%% 钟爱一生夫妻信息
-record(ets_couple_love_life, {
        id = 0,             %% 本服第N对夫妻
        husband = 0,        %% 丈夫角色Id
        wife = 0,           %% 妻子角色Id
        husband_name = <<>>,%% 丈夫角色名
        wife_name = <<>>,   %% 妻子角色名
        husband_lv = 0,     %% 丈夫等级
        wife_lv = 0,        %% 妻子等级
        husband_career = 0, %% 丈夫职业
        wife_career = 0,    %% 妻子职业
        marry_time = {},    %% 结婚登记时间{年,月,日,星期几}
        sweet = 0,          %% 甜蜜度
        sign_times = 0,     %% 夫妻共同签到的次数
        husband_sign = 0,   %% 丈夫签到
        wife_sign = 0       %% 妻子签到
    }).

%% 夫妻每月签到
-record(ets_couple_sign, {
        id = 0,             %% 签到序号
        couple_id = 0,      %% 夫妻ID
        year = 0,           %% 签到年
        month = 0,          %% 签到月
        gift = 0,           %% 签到礼包
        sign_list = []      %% 这个月的签到情况{丈夫签到,妻子签到,签到日}
    }).

-record(ets_couple_ring, {
        role_id = 0,        %% 角色ID
        ring = []           %% 钻戒礼盒{戒指位置, 钻戒ID, 戒指类型}
    }).

-record(ets_couple_ring_limit, {
        role_id = 0,        %% 角色ID
        open_times = 0,     %% 打开婚戒礼盒次数
        buy_times = 0,      %% 购买戒指次数
        week = 0            %% 打开和购买的结婚周数
    }).

%% 70级副本2层路径
-record(ets_70d_path, {
        id = 1,
        specail_path = [],
        time = 0
    }).

%% 玩家形象
-record(figure, {
        goods_id = 0,       %% 物品id
        figure = 0,         %% 形象id
        time = 0,           %% 形象持续时间
        hp_lim = 0,         %% 血上限
        att = 0,            %% 攻击
        def = 0,            %% 防御
        speed = 0,          %% 移动速度
        hit = 0,            %% 命中
        dodge = 0,          %% 躲避
        crit = 0,           %% 暴击
        ten = 0,            %% 坚韧
        fire = 0,           %% 火抗
        ice = 0,            %% 冰抗
        drug = 0            %% 毒抗
    }).

%% 玩家变身表
-record(ets_figure, {
        id = 0,     %% 玩家id
        pid = 0,    %% 玩家进程id
        begin_time = 0, %% 变身开启时间
        left_time = 0   %% 变身剩余时间
    }).

%彩票池
-record(ets_lottry_tick,{
      role_id = 0,   %玩家id
      role_name = 0, %玩家名字
      realm = 0,     %国家
      buytime = 0,   %购买日期
      lottry_num = 0,%奖券号码
      lottry_goods_id = 0,%彩票卷物品id
      is_online = 1,      %是否在线
      active_value = 0    %活跃度
}).

%中奖池
-record(ets_lottry_lucker,{
      role_id = 0,   %玩家id
      role_name = 0, %玩家名字
      realm = 0,     %国家
      buytime = 0,   %购买日期
      lottry_num = 0,%奖券号码
      lottry_goods_id = 0,%彩票卷物品id
      luck_lv = 0,   %中奖等级
      is_get = 0     %是否已经领奖
}).

%号码池
-record(ets_tick_pond, {
      id = 1,
      num_pond = []
}).

%累积池
-record(ets_lottry_leiji, {
    id = 0,   %ID
    gold = 0, %元宝
    coin = 0, %铜币
    exp = 0,  %经验
    is_active = 1 %是否抽奖完成：0否， 1是
}).

% 活跃度副本，彩票所用
-record(ets_lottry_target_day, {
        id = 0,       %% 玩家id
        activity = 0  %% 活跃度
    }).

%夺旗战数据管理ets
-record(ets_flag_battle, {
    id = 1,          %数据ID
    def_nation = 0,  %防御方国家
    flag_state = [], %旗子状态：[{线路， 旗帜数量},……]
    kill_manager = [] %杀死旗子记录[{国家，数量}]
}).

%战旗buff
-record(ets_flag_battle_buff, {
   realm = 1,        %国家id
   buff = 0          %buff倍率
}).

% 帮派技能
-record(ets_guild_skill, {
        id = {0, 0},        % key{帮派id，技能id}
        guild_id = 0,       % 帮派id
        skill_id = 0,       % 技能id
        lv = 0              % 技能等级
    }).

% 帮派成员技能
-record(ets_guild_member_skill, {
        id = {0, 0}, % key{玩家技能， 技能id}
        player_id = 0,  % 玩家id
        skill_id = 0,   % 技能id
        lv = 0,         % 技能等级
        active_time = 0,% 激活时间
        active = 0      % 是否激活（1为激活，0为没有激活）
    }).

%夺旗战怪物管理
-record(ets_flag_battle_mon, {
    id = 0,
    line = 0,
    x = 0,
    y = 0,
    kill_time = 0,
    kill_guild_id = 0,
    kill_guild_name = "",
    kill_guild_realm = 0
}).

%夺旗战胜方管理
-record(ets_flag_battle_win_guild, {
    guild_id = 0,
    guild_name = "",
    win_time = 0,
    realm = 0
}).


%个人单挑场景管理
-record(ets_one_vs_one_scene, {
    scene = 0,                         %挑战场景
    challenge_start_role_id = 0,       %挑战发起方角色ID
    challenge_start_role_name = "",    %挑战发起方角色ID
    challenge_start_role_pos = [],     %挑战发起方原始坐标
    challenge_start_role_wins = 0,     %挑战发起方胜利场数
    challenge_start_role_sex = 0,      %挑战发起方性别
    challenge_start_role_job = 0,      %挑战发起方职业
    challenge_start_role_hander = 0,   %挑战发起方头像
    challenge_start_role_hp = 0,
    challenge_start_role_ready = 0,    %挑战发起方是否已经准备好
    combat_power = 0,    %挑战发起方的战斗力
    challenge_take_role_id  = 0,      %挑战接受方角色ID
    challenge_take_role_name  = "",   %挑战接受方角色ID
    challenge_take_role_pos = [],     %挑战接受方原始坐标
    challenge_take_role_wins = 0,     %挑战接受方胜利场数
    challenge_take_role_sex = 0,      %挑战接受方性别
    challenge_take_role_job = 0,      %挑战接受方职业
    challenge_take_role_hander = 0,   %挑战接受方头像
    challenge_take_role_hp = 0,       %HP
    challenge_take_role_ready = 0,    %挑战接收方是否准备好
    challenge_time = 0,                %挑战发起时间
    challenge_word = "",               %挑战口号
    challenge_type = 1,                %挑战类型
    pid = 0                            %场景管理进程ID
}).

%个人单挑结果管理
-record(ets_one_vs_one_result, {
    scene = 0,
    challenge_start_role_id = 0,       %挑战发起方角色ID
    challenge_start_role_name = "",    %挑战发起方角色ID
    challenge_start_role_sex = 0,      %挑战发起方性别
    challenge_start_role_job = 0,      %挑战发起方职业
    challenge_start_role_hander = 0,   %挑战发起方头像
    challenge_take_role_id  = 0,       %挑战接受方角色ID
    challenge_take_role_name  = "",    %挑战接受方角色ID
    challenge_take_role_sex = 0,      %挑战接受方性别
    challenge_take_role_job = 0,      %挑战接受方职业
    challenge_take_role_hander = 0,   %挑战接受方头像
    turn_1_winner = 0,                 %第一场
    turn_2_winner = 0,                 %第二场
    turn_3_winner = 0,                 %第三场
    turn_4_winner = 0,                 %第四场
    turn_5_winner = 0,                 %第五场
    now_turn = 1,                      %第几轮比赛中
    is_end = 0,                        %是否已经结束
    pid = 0,                           %场景管理进程ID
    challenge_time = 0,
    give_up_role = 0                   %中途放弃的玩家
}).

%% 结拜关系
-record(sworn_friends, {
        id = 0,             %% 结拜Id
        sworn_time = 0,     %% 结拜时间
        member_num = 0,     %% 结拜人数
        prefix_name = <<>>, %% 前缀名字
        loyalty_level = 0   %% 结拜等级
    }).

%% 结拜成员
-record(sworn_member, {
        id = 0,             %% 角色Id
        name = <<>>,        %% 角色名字
        level = 0,          %% 角色等级
        realm = 0,          %% 角色阵营
        career = 0,         %% 角色职业
        sex = 0,            %% 角色性别
        image = 0,          %% 头像
        vip = 0,            %% Vip状态
        line = 0,           %% 所在线路
        sworn_id = 0,       %% 结拜关系Id
        suffix_name = <<>>, %% 后缀名字
        join_time = 0,      %% 加入结拜时间
        sever_time = 0,     %% 解除结拜时间
        loyalty = 0         %% 情义值
    }).

%% 结拜申请信息
-record(apply_sworn_info, {
        id = 0,             %% 根据申请时间与发起者Id生成
        sponsor_id = 0,     %% 发起者（队长）
        apply_time = 0,     %% 申请时间
        prefix_name = <<>>, %% 前缀名字
        member_list = []    %% 成员信息
    }).

%% 申请加入结拜信息
-record(join_sworn_info, {
        id = 0,             %% 申请者角色Id
        name = <<>>,        %% 申请者角色名
        sworn_id = 0,       %% 结拜Id
        prefix_name = <<>>, %% 称号前缀
        suffix_name = <<>>, %% 申请者后缀名
        apply_time = 0,     %% 申请加入时间
        apply_line = 0,     %% 申请线路
        member_list = []    %% 原成员投票列表
    }).

%% 塔奖励保存
-record(ets_tower_reward, {
        player_id = 0,          %% 玩家id
        dungeon_pid = 0,        %% 塔副本进程id
        dungeon_time = 0,       %% 塔副本开启时间
        fin_sid = 0,            %% 完成的层数id
        reward_sid = 0,         %% 已经获取的层数id
        begin_sid = 0,          %% 副本开始场景id
        exreward = 1,           %% 奖励附加的百分比
        active_scene = 0,       %% 跳层场景id
        enter_times = 0         %% 多倍副本次数类型
    }).

%% 暗器配置
-record(base_anqi, {
        grade = 0,                  %% 品阶
        name = <<>>,                %% 暗器名称
        lv = 0,                     %% 最大等级
        mastery = 0,                %% 升级所需熟练度
        skill_id = 0,               %% 基本技能
        skill_list = [],            %% 特殊技能列表，[{技能ID,最大等级},...]
        coin = 0,                   %% 升阶所需铜钱
        goods_id = 0,               %% 升阶所需物品
        goods_num = 0,              %% 物品数量
        luck = 0,                   %% 升阶失败增加幸运值
        luck_lim = 0,               %% 幸运值上限
        luck_ratio = []             %% 幸运值概率列表
    }).

%% 暗器ETS
-record(ets_anqi, {
        role_id = 0,                %% 角色ID
        grade = 0,                  %% 品阶
        name = <<>>,                %% 暗器名称
        lv = 0,                     %% 等级
        mastery = 0,                %% 熟练度
        mastery_tmp = 0,            %% 临时熟练度，记录上次存入数据库时的数据
        luck = 0,                   %% 升阶幸运值
        luck_time = 0,              %% 幸运值刷新时间
        skill_id = 0,               %% 基本技能
        skill_list = [],            %% 特殊技能列表，[{技能ID,最大等级},...]
        ctime = 0                   %% 创建时间
    }).

%% 情缘副本
-record(ets_appointment_dungeon, {
        %dungeon_pid = 0,            %% 进程pid
        scene = 0,                  %% 唯一场景id
        mon_list = []               %% 怪物列表
    }).

%% 摇钱树
-record(money_tree, {
        id = 0,         %% 角色Id
        name = <<>>,    %% 角色名
        guild_id = 0,   %% 帮派Id
        location = 0,   %% 位置
        seed_id = 0,    %% 树种Id
        health = 0,     %% 健康
        color = 0,      %% 颜色
        ripeness = 0,   %% 成熟度
        plant_time = 0, %% 种植时间
        last_grown = 0  %% 上次成长时间
    }).

%% 摇钱树操作记录
-record(money_tree_log, {
        role_id = 0,            %% 角色Id
        role_name = <<>>,       %% 角色名字
        tree_owner = 0,         %% 树苗所有者
        tree_id = 0,            %% 树种类型Id
        plant_time = 0,         %% 种植时间
        color = 0,              %% 颜色
        new_color = 0,          %% 新颜色
        reward_exp = 0,         %% 经验数值
        reward_goods_list = [], %% 物品奖励
        reward_time = 0,        %% 发生时间
        action_type = 0         %% 操作类型
    }).

-record(country, {
        id = 0,                 %% 国家
        power_ranking = 0,      %% 实力排名
        level_ranking = 0,      %% 等级排名
        strongest_power = 0,    %% 强国实力
        total_power = 0,        %% 综合实力
        total_level = 0,        %% 总等级
        num_of_power_rank = 0,  %% 战斗力榜人数
        num_of_level_rank = 0,  %% 等级榜人数
        num_of_gjpt_rank = 0,   %% 国家声望榜人数
        is_weakest = 0,         %% 是否弱国
        world_level = 0,        %% 世界等级
        count_time = 0          %% 统计时间
    }).

%换线排队管理:只针对换到88线
-record(ets_change_line, {
        role_id = 0,   %玩家id
        line = 0,
        scene = 0,     %目标场景id
        x = 0,         %目标X坐标
        y = 0          %目标Y坐标
}).


%% 小号管理
-record(ets_doubt_account, {
        id = 0,                                             %% 玩家id
        today_doubt = 0,                                    %% 今天的疑似度
        yesterday_doubt = 0,                                %% 昨天的疑似度
        before_yesterday_doubt = 0,                         %% 前天的疑似度
        doubt_same_day = 0,                                 %% 疑似度相同的天数
        today_path = [],                                    %% 今天的行动顺序
        yesterday_path = [],                                %% 昨天的行动顺序
        before_yesterday_path = [],                         %% 前天的行动顺序
        path_same_day = 0,                                  %% 行动顺序相同的天数
        today_coin_dungeon_enter_xy = {0, 0},               %% 今天进入钱多多的坐标
        yesterday_coin_dungeon_enter_xy = {0, 0},           %% 昨天进入钱多多的坐标
        before_yesterday_coin_dungeon_enter_xy = {0, 0},    %% 前天进入钱多多的坐标
        coin_dungeon_enter_xy_same_day = 0,                 %% 进入钱多多的坐标相同的天数
        lv_up_time = 0,                                     %% 上次升级时间
        today_online_time = 0,                              %% 今天在线时间
        yesterday_online_time = 0,                          %% 昨天在线时间
        today_activity_value = 0,                           %% 昨天活跃度数值
        is_pass = 1,                                        %% 是否已经输入验证码
        time = 0                                            %% 记录时间
    }).

%% 工作室号
-record(ets_studio_account, {
        id = 0,                  %% 玩家id
        today_mail_num = 0,      %% 今天收取疑似邮件数
        yesterday_mail_num = 0,  %% 昨天天收取疑似邮件数
        more_than_20_day = 0,    %% 大于20封的天数
        time = 0,                %% 时间
        sell_coin = 0,           %% 挂售铜币总量
        coin_values = 0          %% 挂售铜币价格
    }).

%% 玩家血包配置
-record(base_hp_bag, {
        type = 0,               %% 血包类型，1 气血，2 内力，3 回满血包，4 回满蓝包，5 普通血包，6 普通蓝包，7 帮派血包，8 帮派蓝包
        reply_span = 0,         %% 回复间隔时间，单位：秒
        scene_lim = [],         %% 限制的场景
        scene_allow = []        %% 允许的场景
    }).

%% 玩家血包ETS
-record(ets_hp_bag, {
        id = {0,0},             %% 编号 ｛玩家ID,类型｝
        role_id = 0,            %% 玩家id
        type = 0,               %% 血包类型，1 气血，2 内力，3 回满血包，4 回满蓝包，5 普通血包，6 普通蓝包，7 帮派血包，8 帮派蓝包
        bag_num = 0,            %% 血包储量
        reply_num = 0,          %% 血包单次回复量
        goods_id = 0,           %% 血包物品类型ID
        time = 0                %% 更新时间
     }).

%% 神秘商店配置
-record(base_secret_shop, {
        type = 0,               %% 商店类型
        goods_id = 0,           %% 物品类型ID
        price = 0,              %% 物品价格
        bind = 0,               %% 绑定状态，2已绑定
        notice = 0,             %% 公告类型，1全服公告
        ratio = 0,              %% 机率
        ratio_start=0,          %% 机率开始值
        ratio_end=0,            %% 机率结束值
        lim_lv = 0,             %% 等级限制
        lim_min = 0,            %% 最少刷新次数限制
        goods_num = 0           %% 物品数量
    }).

%% 神秘商店ETS
-record(ets_secret_shop, {
        id = {0,0},             %% {玩家id, 商店类型}
        role_id = 0,            %% 玩家id
        type = 0,               %% 商店类型
        num = 0,                %% 单次刷新数
        count = 0,              %% 总刷新次数
        goods_list = [],        %% 物品列表
        lim_goods = [],         %% 限制物品列表：[{物品ID,限制数},...]
        time = 0                %% 更新时间
     }).

%% 幸运转盘配置
-record(base_lucky_box, {
        id = 0,                 %% 编号
        goods_id = 0,           %% 物品类型ID
        ratio = 0,              %% 机率
        ratio_start=0,          %% 机率开始值
        ratio_end=0             %% 机率结束值
    }).

%% 祈福宝箱配置
-record(base_bless_box, {
        goods_id = 0,           %% 物品类型ID
        notice = 0,             %% 是否公告
        high = 0,               %% 是否高级物品
        ratio = 0,              %% 机率
        ratio_start=0,          %% 机率开始值
        ratio_end=0,            %% 机率结束值
        limit_num=0             %% 次数限制
    }).

%% 祈福宝箱ETS
-record(ets_bless_box, {
        role_id = 0,            %% 玩家id
        free = 0,               %% 免费领取状态
        count = 0,              %% 总刷新次数
        goods_list = [],        %% 物品列表
        time = 0,               %% 更新时间
        bless2_goods_list = [], %% 新摇钱包裹
        bless2_open_time  = 0,  %% 新摇钱开启时间
        bless2_open_times = 0,  %% 开启次数
        bless2_buy_price  = 0,  %% 购买价格
        bless2_buy_times  = 0,  %% 购买次数
        bless2_reset_time = 0,  %% 重置时间
        bless2_reset_times= 0,  %% 重置次数
        bless2_goods_condition= [], %% 新摇钱限制物品
        bless2_goods_count= 0,  %% 高级物品次数
        bless2_goods_limit= 0   %% 下次开启高级物品限制次数
     }).

%% 福袋配置
-record(base_happy_bag, {
        id = 0,                 %% 编号
        no_login_day = 0,       %% 连续未登录天数
        continued_day = 0,      %% 持续天数
        span_time = 0,          %% 间隔时间
        goods_list = []         %% 奖励物品列表
    }).

%% 福袋题库配置
-record(base_happy_bag_subject, {
        id = 0,                 %% 编号
        correct = 0,            %% 答案
        content = <<>>,         %% 题目
        option1 = <<>>,         %% 选项1
        option2 = <<>>,         %% 选项2
        option3 = <<>>,         %% 选项3
        option4 = <<>>          %% 选项4
    }).

%% 福袋ETS
-record(ets_happy_bag, {
        role_id = 0,            %% 角色ID
        bag_id = 0,             %% 福袋ID
        subjects = [],          %% 题目列表，[题目ID，...]
        goods_list = [],        %% 奖励物品列表
        ntime = 0,              %% 下个福袋开始时间
        ztime = 0,              %% 今天零点时间
        ctime = 0,              %% 开始时间
        etime = 0               %% 结束时间
    }).


%% 铸魂属性
-record(ets_attribute_medal_soul, {
     id = 0,           % 数据库ID
     medal_id = 0,     % 勋章ID
     attribute_id = 0, % 属性ID
                       %数值属性：1攻击、2防御、3气血、4内力、5命中、6闪避、7暴击、8坚韧、9火抗、10冰抗
                       % 、11毒抗、12全抗、13力量、14灵力、15身法、16体质、17攻击百分比、10防御百分比、
                       % 19气血百分比、20内力百分比、21命中百分比、22闪避百分比、23暴击百分比、24坚韧百
                       % 分比、25火炕百分比、26冰抗百分比、27毒抗百分比、28全抗百分比
                       %特效属性：
                       %29人物头顶最多可显示2-3个称号
                       %30所有等级轻功冷却时间减少5-10秒
                       %31千里相会冷却时间减少10-20分钟
                       %32人物移动速度永久增加8-16
                       %33绝世内功强身术、冥想术当前等级增加10-20
                       %34绝世内功强击术当前等级增加10-20
                       %35绝世内功石肤术当前等级增加10-20
                       %36绝世内功精准术当前等级增加10-20
                       %37绝世内功灵巧术当前等级增加10-20
                       %38绝世内功狂怒术当前等级增加10-20
                       %39绝世内功坚韧术当前等级增加10-20
                       %40绝世内功火抗术、冰抗术、毒抗术当前等级增加5-15
     min_value = 0,    % 属性最小值
     max_value = 0,    % 属性最大值
     ratio = 0,        % 随机概率
     attribute_type = 0% 属性类型：0数值属性、1技能特效属性
    }).

%% 铸魂技能
-record(ets_attribute_medal_skill, {
         id = 0,       % 数据库ID
         medal_id = 0, % 勋章ID
         skill_id = 0, % 技能ID：
                       % （1两个称号、2三个称号、3轻功冷却减少5秒、4轻功冷却减少10秒、5千里相会CD少10分钟、
                       %   6千里相会CD减少20分钟、7移动速度+5、8移动速度+10、9强身术冥想术+10、10强身术冥想
                       %   术+20、11强击术石肤术+10、12强击术石肤术+20、13精准术灵巧术+10、14精准术灵巧术
                       %   +20、15狂怒术坚韧术+10、16狂怒术坚韧术+20、17火炕术冰抗术+10、18火炕术冰抗术+20
                       %   19火炕术毒抗术+10、20火炕术毒抗术+20、21冰抗术毒抗术+10、22冰抗术毒抗术+20、
                       %   23强击术精准术狂怒术+10、24强击术精准术狂怒术+20、25石肤术灵巧术坚韧术+10、26石
                       %   肤术灵巧术坚韧术+20、27火炕术冰抗术毒抗术+10、28火炕术冰抗术毒抗术+20）
         ratio = 0     % 概率
        }).

%% 周日历各种奖励倍数record
-record(week_calendar_ratio, {
        expr = 1,       %% 经验倍数
        coinr = 1,      %% 铜钱倍数
        bcoinr = 1,     %% 绑定铜钱倍数
        scorer = 1,     %% 积分倍数
        llptr = 1       %% 历练声望倍数
    }).

% 新版钱多多副本
-record(ets_coin_dungeon, {
        player_id = 0, % 玩家id
        combo = 0,     % 连斩数
        max_combo = 0, % 最高连斩数
        coin = 0,      % 铜钱
        bcoin = 0      % 绑定铜钱
    }).

% 钱多多副本排行
-record(ets_coin_rank, {
        player_id = 0, % 玩家id
        nickname = [], % 玩家名字
        career = 0,    % 玩家职业
        max_combo = 0, % 最高连斩数
        coin = 0,      % 铜钱
        bcoin = 0,     % 绑定铜钱
        total_coin = 0 % 总共的铜钱
    }).

% 切换线路加入队伍
-record(ets_change_line_into_team, {
        id = 0,
        leader_id = 0,
        line = 0
    }).

% 跨服战队
-record(ets_kfz_team, {
       kfz_team_id = 0,         %战队ID
       kfz_team_members = [],   %战队角色[{角色ID, 角色国家， 角色名字, 角色头像},……]
       kfz_team_name = "",      %战队名称
       kfz_team_leader = 0,     %战队队长
       kfz_team_create_time = 0,%战队时间
       kfz_team_score = 0,      %战队积分
       kfz_team_win  = 0,       %战队胜利次数
       kfz_team_lose = 0,       %战队失败次数
       kfz_team_kill = 0,       %战队击杀次数
       kfz_team_bekill = 0,     %战队死亡次数,
       kfz_unique_key = 0,      %跨服唯一标识
       platform = "",           %平台名称
       server_id = 0            %服务器ID
}).

-record(ets_boss, {
        id = 0,                               %% 数据库ID
        boss_id = 0,                          %% 怪物类型ID
        boss_rate = [],                       %% 刷新概率
        refresh_type = 0,                     %% 刷新类型【0定时长刷新，1定时间点刷新】
        refresh_times = 0,                    %% 刷新时长
        refresh_times_point_6 = [],           %% 刷新时间点[6小时]
        refresh_times_point_3 = [],           %% 刷新时间点[3小时]
        refresh_place = [],                   %% 刷新地点
        notice = 0,                           %% 是否公告
        living_time = 0,                      %% 出生后存活时间
        active = 0,                           %% 是否主动攻击敌人
        starttime = 0,                        %% 怪物开始刷新日期
        endtime = 0                           %% 怪物结束刷新日期
    }).

% 跨服战
-record(kfz_rein, {
  unikey   = "",
  pid_team = 0,
  create_role = 0,
  create_time = 0
}).

%% 跨服送花记录（玩家所在服）
-record(kf_client_flower, {
        id = 0,
        role_id = 0,
        kfz_player_id = 0,
        server_id = 0,
        platform = <<>>,
        kf_player_name = <<>>,
        sex = 0,
        career = 0,
        image = 0,
        flower_num = 0,
        flower_type = 0,
        present_time = 0,
        processed = 0,
        present_type = 0
    }).

%% 跨服送花记录（跨服中心服）
-record(kf_server_flower, {
        id = 0,
        s_kfz_player_id = 0,
        r_kfz_player_id = 0,
        flower_num = 0,
        flower_type = 0,
        present_time = 0
    }).

%% 世界boss伤害列表
-record(ets_world_boss, {
        id = 0,          % ID
        boss_id = 0,     % BOSS ID
        hurt_list = []    % 伤害列表
    }).

%% 跨服战3v3次数记录
-record(ets_kfz_3v3_times, {
        id = 0,     % 玩家ID
        times = 0,  % 次数
        time = 0    % 零点时间戳
}).

%% 离线消息
-record(offline_msg, {
        id = 0,
        role_id = 0,
        sender_id = 0,
        sender_name = <<>>,
        sex = 0,
        realm = 0,
        career = 0,
        head_image = 0,
        sent_time = 0,
        msg = <<>>
    }).

%% 邮件充值活动
-record(mail_activity, {
        id = 0,                 % 编号
        type = 0,               % 充值类型，1单笔充值，2累积充值
        lim_gold = 0,           % 充值元宝限制，下限值
        lim_gold2 = 0,          % 充值元宝限制，上限值
        time1 = 0,              % 查询开始时间
        time2 = 0,              % 查询结束时间
        status = 0              % 状态，0未生效，1生效
    }).

%% 帮派boss预约
-record(ets_guildbossmgr, {
        guild_id = 0,                 % 帮派ID
        guild_boss_call_time = 0      % 召唤时间
    }).

%% 玩家奥运会答题信息
-record(ets_role_olympiad, {
        id = {0,0},             % 编号，{角色ID,天数}
        role_id = 0,            % 角色ID
        day = 0,                % 天数
        answer = 0,             % 选择的答案
        atime = 0,              % 答题时间
        rtime = 0               % 领奖时间
    }).

%% 帮派工资宝库
-record(guild_wage, {
        id              = 0,        % 帮派Id
        wage_amount     = 0,        % 工资总额
        wage_base_amount= 0,        % 分配时总额
        wage_base_num   = 0,        % 分配时基本工资之和
        wage_allot_num  = 0,        % 工资实发
        wage_allot_time = 0,        % 发放时间
        wage_allot_state= 0,        % 发放状态
        wage_last_allot = 0,        % 上次发放数额
        allot_last_time = 0,        % 上次发放时间
        wage_income     = []        % 工资收入
    }).

%% 工资宝库分配
-record(guild_member_wage, {
        id                  = 0,    % 角色Id
        name                = <<>>, % 角色名
        guild_id            = 0,    % 帮派Id
        wage_chief_evalu    = [],   % 帮主评价
        wage_guild_contrib  = [],   % 工资贡献
        wage_contrib_ranking= 0,    % 工资贡献排名
        wage_alloted_num    = 0,    % 工资分配数额
        wage_alloted_time   = 0,    % 工资分配时间
        wage_got_time       = 0,    % 工资领取时间
        contrib_counter     = []    % 贡献计数器
    }).

%% 生肖大奖
-record(lucky_animal, {
        id                  = 0,
        gold                = 0,
        silver              = 0,
        bcoin               = 0,
        exp                 = 0,
        base_elem           = 0,    % 五行属性
        ext_type            = 0,    % 生克类型
        start_time          = 0,    % 开始投注时间
        state               = 0,    % 开奖状态
        lucky_num_list      = [],   % 开奖结果
        luckyer_num         = [],   % 获奖人数
        luckyer_awards      = []    % 奖励
    }).

-record(lucky_animal_role, {
        id                  = 0,
        name                = <<>>,
        lucky_id            = 0,    % 活动ID  #lucky_animal.id
        bet_num             = [],   % 投注情况
        result              = 0,    % 获奖结果
        gold                = 0,
        silver              = 0,
        bcoin               = 0,
        exp                 = 0,
        bet_time            = 0,    % 下注时间
        get_time            = 0     % 领取时间
    }).

%%友谊神树
-record(ets_friend_tree, {
        role_id = 0,        		% 角色id
        role_name = "",     		% 角色名
        level = 1,          		% 神树等级
        tree_exp = 0,       		% 神树经验
        ripeness = 0,       		% 成熟度，对应祝福值，为40的时候表示成熟
        last_ripeness = 0,  		% 上次成熟时间
        last_harvest = 0,    		% 上次收获时间
        last_exp_time = 0           % 上次扣经验时间
}).

%%友谊神树祝福记录
-record(ets_friend_tree_blessing,{
        id = {0,0},                 %双方的id,{from_id, to_id}
        time = 0                    %祝福的时间戳
}).

%% 冲级活动领取记录
-record(ets_uplv_act_record, {
        id = 0,                     % 角色id
        record_list = []                 % 领取记录,记录形式为[{Lv1, State1}, {Lv2, State2}...]
}).

%% 新开服登陆领取记录
-record(ets_new_ser_gift,{
        id = 0,               % 角色id
        record_list = [],     % 领取记录
        login_record = []     % 登陆记录
       }).

%% 玩家已激活时装形象
-record(ets_fashion_figure,{
        id = 0,          % 编号
        role_id = 0,     % 角色ID
        figure_id = 0,   % 形象ID
        rec_id = 0,      % 标示ID
        suit_id = 0,     % 套装ID
        color = 0,       % 颜色
        time = 0,        % 激活时间
        active_full = 0, % 是否激活全部颜色
        pur_flag = 0,    % 是否使用紫色激活卷
        org_flag = 0,    % 是否使用紫色激活卷        
        bind = 0         % 是否绑定
       }).

%% 时装激活劵配置
-record(base_fashion_ticket,{
        figure_id   = 0,         % 时装形象物品类型id
        rec_id      = 0,         % 标识id
        suit_id     = 0,         % 套装id
        color       = 0,         % 颜色
        use_effect  = 0,         % 增加的着装度
        type        = 0          % 装备部位
        }).

%% 时装套装数据配置
-record(base_fashion_suit,{
        suit_id = 0,     % 套装id
        name = <<>>,     % 套装名字
        buff = []        % 属性加成   
       }).


%% 港服FaceBook币
-record(fb_card, {
        sn      = <<>>,
        role_id = 0,
        time    = 0
    }).

%% 英雄回归卡号
-record(hero_back_card, {
        card = <<>>,
        role_id = 0,
        time = 0
    }).

%% 纪念碑
-record(hero_monument, {
        id                  = 1,
        devote_total        = 0,
        last_devote_time    = 0,
        last_devote_roles   = [],
        special_effect_list = []
    }).

%% 祝福贺卡
-record(bless_card, {
        id              = 0,
        card_no         = 0,
        sender          = 0,
        sender_name     = <<>>,
        receiver        = 0,
        read_state      = 0,
        send_time       = 0,
        bless_words     = <<>>,
        goods_id        = 0,
        goods_type_id   = 0,
        goods_num       = 0,
        bind            = 0
    }).

%% 古墓兵俑召唤
-record(ets_td_zh, {
        scene = 0,                 % 场景ID
        monmgrpid = 0,             % 塔防服务进程ID
        tdmgrpid = 0,              % 塔防管理进程ID
        hx_info = {0, 0},          % 回血兵俑信息:{技能id, 召唤次数}
        fy_info = {0, 0},          % 防御兵俑信息:{技能id, 召唤次数}
        gj_info = {0, 0},          % 攻击兵俑信息:{技能id, 召唤次数}
        zh_record = [],            % 召唤记录
        hx_list = [],              % 回血兵俑链
        fy_list = [],
        gj_list = []
    }).

%% 帮派转盘物品配置
-record(ets_base_guild_dial,{
    id = 0,            % 编号id
    group_id = 0,      % 组id
    type_id = 0,       % 物品类型id
    award_type = 0,    % 奖励类型
    num = 0,           % 奖励数量
    first_ratio = 0,   % 初选几率
    second_ratio = 0,  % 复选几率
    notice = 0         % 是否公告
}).

% 公共日志表
-record(ets_log_guild_dial, {
      id = 0,         %数据库ID
      role_id = 0,    %玩家ID
      role_name = 0,  %玩家名称
      award_type = 0, %奖励类型
      award_value =0, %奖励物品类型id
      award_time = 0, %获奖时间戳
      realm = 0       %国家
}).

% 个人获得物品日志表
-record(ets_log_guild_dial_person , {
      role_id = 0,    % 玩家id
      list = []       % 物品id
}).

%% 门客基本数据配置
-record(ets_base_hanger, {
        id = 0,          % 门客id
        name = <<>>,     % 门客名字
        group_id = 0,    % 组id
        adopt_limit = 0, % 请教次数上限
        buff = [],       % 属性加成
        need_day = [],   % 需签到天数
        cost_coin = 0,   % 消耗铜币数
        tool_num = [],   % 消耗道具数
        adopt_coin =0,   % 请教消耗铜币
        days = 0,        % 招纳需要的签到天数
        color = 0,       % 门客颜色
        can_get_tool = 0 % 是否可以领取道具
    }).

%% 玩家招纳的门客数据
-record(ets_hanger, {
    id = 0,              % 编号id
    role_id = 0,         % 玩家id
    hanger_id = 0,       % 门客id
    name = <<>>,         % 门客名字
    group_id = 0,        % 门客组别
    adopt_limit = 0,     % 请教上限
    buff = [],           % 属性加成
    need_day = [],       % 需签到天数
    cost_coin = 0,       % 消耗铜币数
    tool_num = [],        % 消耗道具数
    adopt_coin =0,       % 请教消耗铜币
    adopt_times = 0,     % 请教次数
    last_time = 0,       % 最后请教时间
    days = 0,            % 招纳需要的签到天数
    get_flag = 0         % 二次机会领取纳贤帖标识
    }).

%% 百科基本数据配置
-record(ets_base_baike, {
        id = 0,          % 百科id
        name = <<>>,     % 百科名字
        type_id = 0,     % 类别id
        group_id = 0,    % 组id
        adopt_limit = 0, % 请教次数上限
        buff = [],       % 属性加成
        need_day = [],   % 需签到天数
        cost_coin = 0,   % 消耗铜币数
        tool_list = [],  % 消耗道具列表
        adopt_coin =0,   % 请教消耗铜币
        days = 0,        % 招纳需要的签到天数
        color = 0,       % 百科颜色
        can_get_tool = 0 % 是否可以领取道具
    }).
     
%% 玩家招纳的百科数据
-record(ets_baike, {
    id = 0,          % 编号id
    
    baike_id = 0,    % 百科id
    name = <<>>,     % 百科名字
    type_id = 0,     % 类别id
    group_id = 0,    % 组id
    adopt_limit = 0, % 请教次数上限
    buff = [],       % 属性加成
    need_day = [],   % 需签到天数
    cost_coin = 0,   % 消耗铜币数
    tool_list = [],  % 消耗道具列表
    adopt_coin =0,   % 请教消耗铜币
    days = 0,        % 招纳需要的签到天数
    color = 0,       % 百科颜色  
    
    role_id = 0,     % 玩家id
    adopt_times = 0, % 请教次数
    last_time = 0,   % 最后请教时间
    get_flag = 0     % 领取道具标识
    }).



%% 试炼 PVE
-record(versus_mon, {
        scene_id       = 0,
        scene_type_id  = 0,
        scene_kill_mon = 0,
        mon_type_id    = 0,
        can_enter      = 0
    }).

%% VIP特权
-record(ets_vip, {
        role_id             = 0     % 角色ID
        ,vip_lv             = 0     % VIP特权等级
        ,vip_exp            = 0     % VIP特权经验
        ,give_goods_time    = 0     % 领取物品奖励时间
        ,give_buff_time     = 0     % 领取BUFF奖励时间
        ,online_span        = 0     % 上次在线时长
        ,online_time        = 0     % 在线统计时间
        ,expire_time        = 0     % 截止时间
        ,rtime              = 0     % 更新时间
        ,shop_pay_list      = []    % 商城购买列表，[{物品ID,已购买数},...]
        ,shop_pay_time      = 0     % 商城购买时间
    }).

%% 查询曾用名
-record(ets_ever_name, {
        id = 0,           % 编号
        role_id = 0,      % 角色Id
        name = <<>>,      % 曾用名
        time = 0          % 修改时间
    }).

%% 寻宝
-record(ets_treasure_hunt, {
        id = 0,       % 编号
        goods_id = 0, % 物品唯一Id
        role_id = 0,  % 角色Id
        scene_id = 0, % 场景资源Id
        x = 0,        % x坐标
        y = 0         % y坐标
    }).

