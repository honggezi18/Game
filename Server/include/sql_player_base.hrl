%%%-------------------------------------------------------------------
%%% @author JoyHuang
%%% @copyright (C) 2014, <COMPANY>
%%% Description: 角色系统sql文件
%%% Created : 14. 八月 2014 15:15
%%%-------------------------------------------------------------------


%% 根据账户名称查找角色个数
-define(sql_role_any_id_by_accname,<<"select id from player_login where accname = '~s'">>).

%% 取得指定帐号的角色列表
-define(sql_role_list,<<"select n.id, n.status, w.nickname, w.sex, w.lv, w.career, w.realm from player_login as n left join player_low as w  on w.id = n.id where n.accname='~s'">>).

%% 根据账户名称查找ID
-define(sql_role_id_by_accname,<<"select id from player_login where accname = '~s' limit 1">>).

%% 更新登陆需要的记录
-define(sql_update_login_data,<<"update `player_login` set `last_login_time`= ~p, `last_login_ip`  = '~s', `online_flag`=1 where id = ~p">>).

%% 根据id查找登陆所需信息
-define(sql_player_login_by_id,<<"select `accname`, `status` from `player_login` where `id` = ~p limit 1">>).

%% 根据角色名称查找ID
-define(sql_role_id_by_name,<<"select id from player_low where nickname = '~s' limit 1">>).

%% 注册角色
-define(sql_insert_player_login_one,<<"insert into `player_login` (accid, accname, reg_time, reg_ip) values (~p,'~s',~p,'~s')">>).
-define(sql_insert_player_attr_one,<<"insert into `player_attr` (`id`, `base_hp`, `base_mp`) values (~p, ~p, ~p)">>).
-define(sql_insert_player_high_one,<<"insert into `player_high` (id) values (~p)">>).
-define(sql_insert_player_low_one,<<"insert into `player_low` (id, `nickname`, `sex`, `lv`, `career`, `realm`) values (~p, '~s', ~p, ~p, ~p, ~p)">>).
-define(sql_insert_player_state_one,<<"insert into `player_state` (id, scene, x, y, hp, mp) values (~p, ~p, ~p, ~p, ~p, ~p)">>).

%%更新player_state数据
-define(sql_update_player_state,<<"update `player_state` set `scene`=~p, `x`=~p, `y`=~p, `hp`=~p, `mp`=~p, `quickbar`='~s' where id=~p">>).

%%获取登录所需角色数据
%% player_login
-define(sql_player_login_data,<<"select accname, reg_time, gm, talk_lim, talk_lim_time, last_logout_time from player_login where id = ~p limit 1">>).

%% player_high
-define(sql_player_high_data,<<"select `gold`, `bgold`, `coin`, `bcoin`, `exp` from `player_high` where id=~p limit 1">>).

%% player_low
-define(sql_player_low_data,<<"select `nickname`, `sex`, `lv`, `career`, `realm`, `guild_id`, `image` from `player_low` where id=~p limit 1">>).

%%player_state
-define(sql_player_state_data,<<"select `scene`, `x`, `y`, `hp`, `mp`, `quickbar` from `player_state` where id=~p limit 1">>).

%% player_attr
-define(sql_player_attr_data,<<"select `base_hp`, `base_mp` from player_attr where id=~p limit 1">>).
