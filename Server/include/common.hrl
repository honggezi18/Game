%%%------------------------------------------------
%%% File    : common.hrl
%%% Author  : xyao
%%% Created : 2010-04-22
%%% Description: 公共定义
%%%------------------------------------------------
%-define(SD_SERVERS, 'SD_SERVERS').
-define(ALL_SERVER_PLAYERS, 10000).

%%flash843安全沙箱
-define(FL_POLICY_REQ, <<"<pol">>).
-define(FL_POLICY_REQ_All, <<"<policy-file-request/>\0">>).
-define(FL_POLICY_FILE, <<"<cross-domain-policy><allow-access-from domain='*' to-ports='*' /></cross-domain-policy>">>).

%%tcp_server监听参数
-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}, {nodelay, false}, {delay_send, true}, {send_timeout, 5000}, {keepalive, true}, {exit_on_close, true}]).

%%错误处理
-define(DEBUG(F, A), util:log("debug", F, A, ?MODULE, ?LINE)).
-define(INFO(F, A), util:log("info", F, A, ?MODULE, ?LINE)).
-define(ERR(F, A), util:log("error", F, A, ?MODULE, ?LINE)).

%%数据库
-define(DB, sd_mysql_conn).

-define(DIFF_SECONDS_1970_1900, 2208988800).
-define(DIFF_SECONDS_0000_1900, 62167219200).
-define(ONE_DAY_SECONDS,        86400).

%%ETS
-define(SERVER_STATUS, server_status).                          %% 服务器信息
-define(ETS_SERVER, ets_server).                                %% 线路组
-define(ETS_CHAT, ets_chat).                                    %% 聊天
-define(USER_LOGIN, user_login).                                %% 用户登陆
-define(ETS_ONLINE, ets_online).                                %% 在线
-define(LOGIC_ONLINE, logic_online).                            %% 场景在线
-define(ETS_MON, ets_mon).                                      %% 怪物
-define(ETS_MON_TMP, ets_mon_tmp).                              %% 怪物临时表
-define(ETS_NPC, ets_npc).                                      %% npc
-define(ETS_SCENE, ets_scene).                                  %% 场景
-define(ETS_SCENE_MON, ets_scene_mon).                          %% 每种怪物所在场景
-define(ETS_SCENE_POSES, ets_scene_poses).                      %% 存放场景中可以移动的坐标，用于NPC寻路
-define(ETS_SCENE_SAFE_POSES, ets_scene_safe_poses).            %% 存放场景中不可以PK的坐标
-define(ETS_SCENE_WATER_POSES, ets_scene_water_poses).          %% 存放水面的坐标
-define(ETS_GOODS_ONLINE, ets_goods_online).                    %% 在线玩家的背包物品表
-define(ETS_DROP, ets_goods_drop).                              %% 物品掉落表
-define(ETS_ROLE_TASK, ets_role_task).                          %% 已接任务
-define(ETS_ROLE_TASK_LOG, ets_role_task_log).                  %% 已完成任务
-define(ETS_ROLE_TASK_AUTO, ets_role_task_auto).                %% 委托任务
-define(ETS_TASK_QUERY_CACHE, ets_task_query_cache).            %% 当前所有可接任务
-define(ETS_TASK_CUMULATE, ets_task_cumulate).                  %% 经验累积任务历史日志
-define(ETS_RELA, ets_rela).                                    %% 玩家关系表
-define(ETS_RELA_GROUP, ets_rela_group).                        %% 好友分组列表
-define(ETS_RELA_INFO, ets_rela_info).                          %% 好友资料
-define(ETS_RELA_SET, ets_rela_set).                            %% 玩家好友分组名字表
-define(ETS_MAIL, ets_mail).                                    %% 邮件
-define(ETS_PET,               ets_pet).                        %% 宠物
-define(ETS_PET_SHOP,          ets_pet_shop).                   %% 宠物商城
-define(ETS_PET_SKILL,         ets_pet_skill).                  %% 宠物技能
-define(ETS_PET_MANUAL,        ets_pet_manual).                 %% 宠物图鉴
-define(ETS_BASE_GOODS_PET,    ets_base_goods_pet).             %% 宠物物品配置
-define(ETS_BASE_PET_MANUAL,   ets_base_pet_manual).            %% 宠物图鉴配置
-define(ETS_PET_TREASUREBOX,   ets_pet_treasurebox).            %% 宠物宝箱
-define(ETS_PET_SKILL_TREASUREBOX,ets_pet_skill_treasurebox).   %% 宠物技能宝箱
-define(ETS_PET_POTENTIAL,     ets_pet_potential).              %% 宠物潜能
-define(ETS_GUILD,             ets_guild).                      %% 帮派
-define(ETS_GUILD_EXCLUSIVE,   ets_guild_exclusive).            %% 帮派（线路独占信息）
-define(ETS_GUILD_MEMBER,      ets_guild_member).               %% 帮派成员
-define(ETS_GUILD_APPLY,       ets_guild_apply).                %% 帮派申请
-define(ETS_GUILD_INVITE,      ets_guild_invite).               %% 帮派邀请
-define(ETS_GUILD_BATTLE_SCENE,ets_guild_battle_scene).         %% 帮战场景
-define(ETS_GUILD_BATTLE_STATE_INFO,ets_guild_battle_state_info).%% 帮战状态信息
-define(ETS_GUILD_BATTLE_RESULT,ets_guild_battle_result).       %% 帮战结果
-define(ETS_GUILD_BATTLE_MEMBER_RESULT,ets_guild_battle_member_result). %% 帮战成员结果
-define(ETS_BASE_GUILD_BATTLE_AWARD,ets_base_guild_battle_award).       %% 帮战奖励物品配置
-define(ETS_GUILD_BATTLE_MEMBER_INFO, ets_guild_battle_member_info).    %% 帮战成员
-define(ETS_SIEGE_BATTLE,       ets_siege_battle).              %% 攻城战信息
-define(ETS_SIEGE_BATTLE_STATE_INFO, ets_siege_battle_state_info).%% 攻城战状态信息
-define(ETS_SIEGE_BATTLE_SCENE, ets_siege_battle_scene).        %% 攻城战场景
-define(ETS_SIEGE_BATTLE_RESULT,ets_siege_battle_result).       %% 攻城战结果
-define(ETS_SIEGE_BATTLE_MEMBER_RESULT,ets_siege_battle_member_result). %% 攻城战战成员结果
-define(ETS_BASE_SIEGE_BATTLE_AWARD,ets_base_siege_battle_award).       %% 攻城战奖励物品配置
-define(ETS_GUILD_AWARD,       ets_guild_award).                %% 帮派奖励物品表
-define(ETS_GUILD_AWARD_ALLOC, ets_guild_award_alloc).          %% 帮派奖励分配表
-define(ETS_GUILD_MEMBER_AWARD,ets_guild_member_award).         %% 帮派成员奖励物品表
-define(ETS_MASTER,            ets_master).                     %% 师傅信息
-define(ETS_MASTER_APPRENTICE, ets_master_apprentice).          %% 师徒关系
-define(ETS_ARENA,             ets_arena).                      %% 竞技场.
-define(ETS_ARENA_STATE_INFO,  ets_arena_state_info).           %% 竞技场状态信息.
-define(ETS_ARENA_SCENE,       ets_arena_scene).                %% 竞技场场景
-define(ETS_ARENA_RESULT,      ets_arena_result).               %% 竞技场结果
-define(ETS_GRAB_STONE_STATE_INFO,  ets_grab_stone_state_info). %% 宝石试炼状态信息.
-define(ETS_GRAB_STONE_RESULT,      ets_grab_stone_result).     %% 宝石试炼结果
-define(ETS_SIXIANG,           ets_sixiang).                    %% 远征四项.
-define(ETS_SIXIANG_RESULT,    ets_sixiang_result).             %% 远征四项结果
-define(ETS_SIXIANG_STATE_INFO,ets_sixiang_state_info).         %% 远征四项状态信息.
-define(ETS_SIXIANG_SCENE,     ets_sixiang_scene).              %% 远征四项场景
-define(ETS_KFZ_ARENA_BATTLE,  ets_kfz_arena_battle).           %% 跨服擂台赛战场
-define(ETS_KFZ_ARENA,         ets_kfz_arena).                  %% 跨服擂台赛
-define(ETS_KFZ_ARENA_SCENE,   ets_kfz_arena_scene).            %% 跨服擂台赛场景
-define(ETS_KFZ_ARENA_RESULT,  ets_kfz_arena_result).           %% 跨服擂台赛结果
-define(ETS_KFZ_LEAGUE_TEAM,   ets_kfz_league_team).            %% 跨服联赛队伍
-define(ETS_KFZ_LEAGUE_STATE,  ets_kfz_league_state).           %% 跨服联赛状态
-define(ETS_KFZ_LEAGUE_RESULT, ets_kfz_league_result).          %% 跨服联赛结果
-define(ETS_KFZ_LEAGUE_SCENE,  ets_kfz_league_scene).           %% 跨服联赛场景
-define(ETS_KFZ_LEAGUE_WATCH,  ets_kfz_league_watch).           %% 跨服联赛观赛
-define(ETS_KFZ_ALLIANCE_STATE,  ets_kfz_alliance_state).       %% 跨服天梯排位赛状态
-define(ETS_KFZ_ALLIANCE_RESULT, ets_kfz_alliance_result).      %% 跨服天梯排位赛结果
-define(ETS_KFZ_ALLIANCE_SCENE,  ets_kfz_alliance_scene).       %% 跨服天梯排位赛场景
-define(ETS_KFZ_ALLIANCE_RANK,   ets_kfz_alliance_rank).        %% 跨服天梯排位赛排行
-define(ETS_KFZ_ALLIANCE_LOG,    ets_kfz_alliance_log).         %% 跨服天梯排位赛日志
-define(ETS_KFZ_3V3_RESULT,    ets_kfz_3v3_result).             %% 跨服3v3结果
-define(ETS_KFZ_PLAYER,        ets_kfz_player).                 %% 跨服角色
-define(ETS_KFZ_FRIEND,        ets_kfz_friend).                 %% 跨服好友
-define(ETS_KFZ_BOSS_GUILD_RESULT,ets_kfz_boss_guild_result).   %% 跨服BOSS帮派结果
-define(ETS_KFZ_BOSS_RESULT,    ets_kfz_boss_result).           %% 跨服BOSS玩家结果
-define(ETS_KFZ_BOSS_STATE_INFO,ets_kfz_boss_state_info).       %% 跨服BOSS状态信息
-define(ETS_MERIDIAN_ATTRIBUTE, ets_meridian_attribute).        %% 经脉属性表
-define(ETS_NOTICE, ets_notice).                                %% 系统公告表
-define(ETS_DAILY, ets_daily).                                  %% 每天记录器
-define(ETS_COUNTER, ets_counter).                              %% ETS计数器
-define(ETS_SELL, ets_sell).                                    %% 挂售市场
-define(ETS_SELL_GOODS, ets_sell_goods).                        %% 挂售市场物品
-define(ETS_WTB, ets_wtb).                                      %% 求购市场
-define(ETS_SKILL, ets_skill).                                  %% 技能
-define(ETS_SHOP_LIMIT, ets_shop_limit).                        %% 限购商品
-define(ETS_BUFF, ets_buff).                                    %% 玩家BUFF状态
-define(ETS_PLAYER_MERIDIAN, ets_player_meridian).              %% 玩家经脉
-define(ETS_MON_GOODS_COUNTER, ets_mon_goods_counter).          %% 怪物掉落物品计数器
-define(ETS_DROP_FACTOR, ets_drop_factor).                      %% 物品掉落系数
-define(ETS_HORN_MSG, ets_horn_msg).                            %% 聊天小喇叭消息
-define(ETS_VIP_HORN_MSG, ets_vip_horn_msg).                    %% VIP喇叭消息
-define(ETS_OFFLINE_MSG, ets_offline_msg).                      %% 离线消息
-define(ETS_TMB_OFFLINE, ets_tmb_offline).                      %% 队伍暂离成员列表
-define(ETS_IP_LIMIT, ets_ip_limit).                            %% IP黑名单
-define(ETS_BUFF_WATAR, ets_buff_watar).                        %% 温泉
-define(ETS_COLLECTOR, ets_collector).                          %% 采矿
-define(ETS_LODE, ets_lode).                                    %% 矿脉
-define(BEFORE_LINE, before_line).                              %% 先前线路
-define(ETS_CAPTCHA, ets_captcha).                              %% 验证码
-define(ETS_TEAM_ENLIST, ets_team_enlist).                      %% 组队招募面板
-define(ETS_TOWER_MASTER, ets_tower_master).                    %% 塔各层霸主表
-define(ETS_TOWER_REWARD, ets_tower_reward).                    %% 塔奖励
-define(ETS_FIRE_WOOD, ets_fire_wood).                          %% 篝火木材
-define(PRACTICE_OUTLINE, practice_outline).                    %% 离线经验托管
-define(WINE_OUTLINE, wine_outline).                            %% 烧酒经验托管
-define(ETS_TIME_CONTROL, ets_time_control).                    %% 时间控制
-define(PRACTICE, practice).                                    %% 离线挂机
-define(ETS_GIFT, ets_gift2).                                   %% 活动礼包
-define(ETS_FALLING_STONE, ets_falling_stone).                  %% 天外来石
-define(ETS_APPOINTMENT, ets_appointment).                      %% 仙侣奇缘
-define(ETS_APPOINTMENT_CONFIG, ets_appointment_config).        %% 仙侣奇缘设置
%-define(ETS_BOX_BAG, ets_box_bag).                              %% 宝箱包裹
-define(ETS_BOX_COUNTER, ets_box_counter).                      %% 宝箱全局计数
-define(ETS_BOX_PLAYER_COUNTER, ets_box_player_counter).        %% 宝箱单玩家计数
-define(ETS_DUNGEON_ENLIST, ets_dungeon_enlist).                %% 副本招募
-define(ETS_DUNGEON_ENLIST2, ets_dungeon_enlist2).              %% 副本招募2
-define(ROLE_ACHIEVED_NAME, role_achieved_name).                %% 角色称号
-define(ACHIEVED_NAME, achieved_name).                          %% 称号数据
-define(ETS_WANT_ENTER_DUNGEON, ets_want_enter_dungeon).        %% "我要进入副本"列表
-define(ETS_CHENGJIU, ets_chengjiu).                            %% 成就列表
-define(ETS_CHENGJIU_AWARD, ets_chengjiu_award).                %% 成就奖励列表
-define(PLAYER_CHENGJIU, player_chengjiu).                      %% 角色成就列表
-define(ETS_TARGET_DAY, ets_target_day).                        %% 每日成就
-define(ETS_TARGET_WEEK_TASK, ets_target_week_task).            %% 周目标每日目标
-define(ETS_TARGET_WEEK, ets_target_week).                      %% 周目标
-define(ETS_LOGIN_COUNTER, ets_login_counter).                  %% 连续登录及回归信息
-define(SECONDARY_PASSWORD, secondary_password).                %% 二级密码
-define(ETS_APPOINTMENT_SUBJECT, ets_appointment_subject).      %% 仙侣奇缘题目
-define(ETS_APPOINTMENT_SPECIAL_SUBJECT, ets_appointment_special_subject). %% 仙侣奇缘特殊题目
-define(ETS_QUIZ, ets_quiz).                                    %% 答题活动题目
-define(ETS_QUIZ_S, ets_quiz_s).                                %% 答题活动题目
-define(ETS_TEAM, ets_team).                                    %% 队伍缓存
-define(ETS_TD_SCORE, ets_td_score).                            %% 塔防积分
-define(ETS_TD_ZH, ets_td_zh).                                  %% 塔防兵俑召唤信息
-define(ETS_FORTUNE, ets_fortune).                              %% 玩家运势表
-define(ETS_FORTUNE_LOG, ets_fortune_log).                      %% 玩家运势刷新日志
-define(ETS_ACTIVITY, ets_activity).                            %% 玩家活动信息
-define(ETS_COUPLE, ets_couple).                                %% 夫妻
-define(ETS_PROPOSE, ets_propose).                              %% 求婚信息
-define(ETS_MARRIAGE_EVENT, ets_marriage_event).                %% 婚姻记事
-define(CURRENT_WEDDING, current_wedding).                      %% 当前婚宴记录
-define(WEDDING, wedding).                                      %% 婚宴时间安排
-define(WEDDING_GIFT, wedding_gift).                            %% 婚宴送礼
-define(CURRENT_CRUISE, current_cruise).                        %% 当前巡游队伍
-define(COUPLE_HOME, couple_home).                              %% 夫妻家园
-define(HOME_VISITOR, home_visitor).                            %% 夫妻家园访问名单
-define(HOME_OBJECT, home_object).                              %% 夫妻家园种植物
-define(COUPLE_INSURE, couple_insure).                          %% 爱情保险
-define(ETS_COUPLE_LOVE_LIFE, ets_couple_love_life).            %% 钟爱一生夫妻信息
-define(ETS_COUPLE_SIGN, ets_couple_sign).                      %% 夫妻签到
-define(ETS_COUPLE_RING, ets_couple_ring).                      %% 婚戒礼盒
-define(ETS_COUPLE_RING_LIMIT, ets_couple_ring_limit).          %% 婚戒礼盒限制
-define(WISHING_PAPER, wishing_paper).                          %% 许愿墙
-define(APPLY_SWORN_INFO, apply_sworn_info).                    %% 结拜申请信息
-define(JOIN_SWORN_INFO, join_sworn_info).                      %% 加入结拜申请信息
-define(SWORN_FRIENDS, sworn_friends).                          %% 结拜关系
-define(SWORN_MEMBER, sworn_member).                            %% 结拜成员
-define(ETS_MOUNT, ets_mount).                                  %% 坐骑信息
-define(ETS_MOUNT_SKILL, ets_mount_skill).                      %% 坐骑技能信息
-define(ETS_MOUNT_SZBF, ets_mount_szbf).                        %% 坐骑孙子兵法信息
-define(ETS_MOUNT_EQUIP, ets_mount_equip).                      %% 坐骑装备信息
-define(ETS_ANQI, ets_anqi).                                    %% 暗器
-define(ETS_MONEY_TREE, ets_money_tree).                        %% 摇钱树
-define(ETS_TREE_MON, ets_tree_mon).                            %% 树怪物
-define(MONEY_TREE_LOG, money_tree_log).                        %% 摇钱树操作记录
-define(MONEY_TREE_REWARD_LOG, money_tree_log).                 %% 摇钱树收益记录
-define(ETS_COUNTRY, ets_country).                              %% 国家
-define(ETS_HP_BAG, ets_hp_bag).                                %% 玩家血包ETS
-define(ETS_SECRET_SHOP, ets_secret_shop).                      %% 玩家神秘商店
-define(ETS_BLESS_BOX, ets_bless_box).                          %% 玩家祈福宝箱
-define(ETS_HAPPY_BAG, ets_happy_bag).                          %% 玩家福袋
-define(GUILD_WAGE, guild_wage).                                %% 帮派周工资
-define(GUILD_MEMBER_WAGE, guild_member_wage).                  %% 帮派成员周工资
-define(LUCKY_ANIMAL, lucky_animal).                            %% 生肖幸运抽奖
-define(LUCKY_ANIMAL_ROLE, lucky_animal_role).                  %% 生肖大奖记录
-define(ETS_LOTTRY_TICK ,ets_lottry_tick).                      %% 彩票池
-define(ETS_LOTTRY_LUCKER,ets_lottry_lucker).                   %% 中奖池
-define(ETS_TICK_POND,ets_tick_pond).                           %% 号码池
-define(ETS_LOTTRY_LEIJI,ets_lottry_leiji).                     %% 累积池
-define(ETS_LOTTRY_TARGET_DAY, ets_lottry_target_day).          %% 彩票活跃度副本
-define(ETS_VIP, ets_vip).                                      %% VIP特权

-define(ETS_FLAG_BATTLE, ets_flag_battle).                      %% 夺旗战数据ETS
-define(ETS_FLAG_BATTLE_BUFF, ets_flag_battle_buff).            %% 夺旗战数据ETS

-define(ETS_FIGURE, ets_figure).                                %% 变身形象
-define(ETS_GUILD_SKILL, ets_guild_skill).                      %% 帮派技能
-define(ETS_GUILD_MEMBER_SKILL, ets_guild_member_skill).        %% 帮派个人技能

-define(ETS_FLAG_BATTLE_MON, ets_flag_battle_mon).              %% 夺旗战怪物管理
-define(ETS_FLAG_BATTLE_WIN_GUILD ,ets_flag_battle_win_guild).  %% 夺旗战胜利方管理

-define(ETS_ONE_VS_ONE_SCENE, ets_one_vs_one_scene).            %% 夺旗战场景管理
-define(ETS_ONE_VS_ONE_RESULT, ets_one_vs_one_result).          %% 夺旗战战果管理

-define(ETS_APPOINTMENT_DUNGEON, ets_appointment_dungeon).      %% 情缘副本

-define(ETS_KFZ_3V3_SCENE, ets_kfz_3v3_scene).                  %% 跨服战3v3场景表

-define(ETS_CHANGE_LINE, ets_change_line).                      %% 换线排队管理
-define(ETS_DOUBT_ACCOUNT, ets_doubt_account).                  %% 小号管理
-define(ETS_STUDIO_ACCOUNT, ets_studio_account).                %% 小号管理
-define(ETS_AI, ets_ai).                                        %% ai用
-define(ETS_CHANGE_LINE_INFO_TEAM, ets_change_line_into_team).  % 玩家切线进入队伍
-define(ETS_COIN_DUNGEON, ets_coin_dungeon).                    %% 新版钱多多副本记录
-define(ETS_COIN_RANK, ets_coin_rank).                          %% 新版钱多多排行榜
-define(ETS_KFZ_TEAM, ets_kfz_team).                            %% 跨服战队
-define(KFZ_REIIN, kfz_rein).                                   %% 跨服战重新进组
-define(ETS_KFZ_ENEMY, ets_kfz_enemy).                          %% 跨服仇人
%% 答题
-define(ETS_QUIZ_ANSWER, ets_quiz_answer).
-define(ETS_QUIZ_MEMBER, ets_quiz_member).
-define(KF_CLIENT_FLOWER, kf_client_flower).                    %% 跨服鲜花（各节点服）
-define(KF_SERVER_FLOWER, kf_server_flower).                    %% 跨服鲜花（中心服）
-define(ETS_WORLD_BOSS, ets_world_boss).                        %% 世界BOSS伤害
-define(ETS_KFZ_3V3_TIMES, ets_kfz_3v3_times).                  %% 跨服战3v3次数
-define(ETS_GUILDBOSSMGR, ets_guildbossmgr).                    %% 跨服战3v3次数
-define(ETS_ROLE_OLYMPIAD, ets_role_olympiad).                  %% 玩家奥运会答题信息表
-define(HERO_BACK_CARD, hero_back_card).                        %% 英雄回归卡号
-define(BLESS_CARD, bless_card).                                %% 祝福贺卡
-define(VERSUS_MON, versus_mon).                                %% 试炼 PVE

%%打开发送消息客户端进程数量
-define(SEND_MSG, 3).

%%PK状态切换时间
-define(PK_CHANGE_TIME, 3600*2).

%%世界频道发言间隔
-define(CHAT_WORLD, 8).
%%其他频道发言间隔
-define(CHAT_OTHER, 3).

%%无法回城的地图ID
-define(FORBIMAP, [999,998,994,992,224,228,230,995,996,997,982,981,978,976,974]).
%%自由杀人场景
-define(FREE_KILL_MAP, [984,982,996,230,600001,600003,600006,600008,600010,976,974]).

%% 需要全场景广播的非副本地图
-define(ALL_BROADCAST_MAP_LIST, [228, 120, 121]).     %% 姻缘厅

%%专线场景
-define(LINE_88,[992,994,231,230,978]).

%%专线转换场景
-define(LINE_88_OUT, [100, 16, 16]).
-define(LINE_88_OUT_1, [220, 70, 110]).

%% 远征岛场景
%-define(TOWER_BEGIN_SCENEID, 401). % 远征岛起始场景
-define(TOWER_END_SCENEID, [430, 460]). % 远征岛结束场景

%% 鲜花
-define(SEND_FLOWER, 2900).
-define(RECV_FLOWER, 2901).

%%boss列表
-define(BOSS, [99501,22043,98908,99008,30046,50045,60046,40046,60047,70023,98909,70046]).

%-------------------------------
% 3v3跨服配置
%-------------------------------
%3v3跨服战：战场ID
-define(SCENE_3V3, [234, 238]).
%3v3跨服战：祭坛类型
-define(ALTAR_WHITE, 23401). %白色祭坛 【无归属】
-define(ALTAR_RED,   23403). %红色祭坛 【归属一队】
-define(ALTAR_BLUE,  23402). %蓝色祭坛 【归属二队】
-define(RED_GUILDER, 23405). %复活点守护-红
-define(BLUE_GUILDER, 23404).%复活点守护-蓝
%3v3跨服战：祭坛坐标
-define(ALTAR_POS, [{39,28}, {25,42}, {7,60}]).
-define(ALTAR_PSS_ICE, [{27, 8}, {9, 39}, {41, 39}]).
%3v3跨服战：复活点坐标
-define(RED_REBORN, [13,30]).
-define(BLUE_REBORN, [37,54]).
-define(RED_REBORN_ICE, [6,16]).
-define(BLUE_REBORN_ICE, [43,16]).
%3v3跨服战：最大占领值
-define(MAX_ZHANLING, 1000).

%每秒经验
-define(EXP_PER_RATE_40_LV, 60).
-define(EXP_PER_RATE_60_LV, 120).

%结束经验
-define(EXP_END_RATE_40_LV, [2.4, 30000]).
-define(EXP_END_RATE_60_LV, [4.8, 60000]).

%胜负系数
-define(XISHU_LOSE, 12).
-define(XISHU_WIN,  24).

%% 活动天数定义
-define(ACTIVITY_DAY_OPEN_PAY,              7).             %% 开服充值返还
-define(ACTIVITY_DAY_POINT_OPEN_PAY,        7).             %% 积分开服充值返利
-define(ACTIVITY_DAY_GIFT_OPEN_PAY,         7).             %% 开服充值礼包回馈
-define(ACTIVITY_DAY_GIFT_MEMBER,           3).             %% 会员礼包
-define(ACTIVITY_DAY_SHOP_LIMIT_OPEN,       7).             %% 开服限时抢购
-define(ACTIVITY_DAY_SHOP_POINT_OPEN,       7).             %% 开服积分商城
-define(ACTIVITY_DAY_NEW_GAME,              7).             %% 新服7天活动
-define(ACTIVITY_DAY_MERGED,                7).             %% 合服7天活动

%% 新服活动与冲级活动
-define(ETS_UPLV_ACT_RECORD, ets_uplv_act_record).          %% 冲级活动记录
-define(ETS_NEW_SER_GIFT, ets_new_ser_gift).                %% 新开服登陆奖励记录

%% 时装新形象
-define(ETS_FASHION_FIGURE, ets_fashion_figure).            %% 玩家已激活时装形象记录

%% 新宠物图鉴
-define(CANDY_TYPE_ID, 624527).        %% 糖果物品类型ID
-define(LOVE_DEGREE_LIMIT, 10000).      %% 爱心度最高值

%% 帮派转盘
-define(ETS_LOG_GUILD_DIAL, ets_log_guild_dial). % 公共日志表
-define(ETS_LOG_GUILD_DIAL_PERSON, ets_log_guild_dial_person). % 个人日志表
%%-define(ETS_BASE_GUILD_DIAL, ets_base_guild_dial). % 帮派转盘物品

%% 门客谱
-define(ETS_HAGNER, ets_hanger). % 玩家招纳的门客数据 

%% 百科
-define(ETS_BASE_BAIKE, ets_base_baike). % 玩家基本百科数据
-define(ETS_BAIKE, ets_baike). % 玩家招纳的百科数据

%% 玩家曾用名
-define(ETS_EVER_NAME, ets_ever_name).

%% 寻宝
-define(ETS_TREASURE_HUNT, ets_treasure_hunt).

%% 宝石战场
-define(ETS_JEWEL_BATTLE_STATE_INFO,  ets_jewel_battle_state_info).  %% 宝石战场状态信息
-define(ETS_JEWEL_BATTLE_SCENE, ets_jewel_battle_scene).        %% 宝石战场场景
-define(ETS_JEWEL_BATTLE_RESULT, ets_jewel_battle_result).        %% 宝石战场结果

%% 官职禁言
-define(ETS_CHAT_WARRIOR_BAN, ets_chat_warrior_ban).

%% 玩家等级->转生次数
-define(LV2ZS(PlayerLv), ((PlayerLv - 1) div 100)).