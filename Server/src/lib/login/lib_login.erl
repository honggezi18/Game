%%%--------------------------------------
%%% @Module  : lib_login
%%% @Author  : xyao
%%% @Email   : jiexiaowen@gmail.com
%%% @Created : 2011.06.14
%%% @Description:注册登录
%%%--------------------------------------
-module(lib_login).
-export(
    [
        get_role_list/1,
        create_role/7,
        update_login_data/3,
        get_player_login_by_id/1
    ]
).
-include("common.hrl").
-include("sql_player_base.hrl").
-include("server.hrl").

%% 取得指定帐号的角色列表 
get_role_list(Name) ->
	%1.得到全部角色的属性.
    RoleList = db:get_all(io_lib:format(?sql_role_list, [Name])),

	%2.得到角色的身上装备和时装.
    FunGetEquip =
		fun(Pid, Status, Name1, Sex, Lv, Career, Realm) ->
			[Pid, Status, Name1, Sex, Lv, Career, Realm]
    	end,

	%3.返回角色列表.
	case RoleList of
		List when is_list(List) ->
			[FunGetEquip(Pid, Status, Name2, Sex, Lv, Career, Realm)||
				[Pid, Status, Name2, Sex, Lv, Career, Realm]<-List];
		_ ->
			[]
	end.

%% 创建角色
create_role(AccId, AccName, Name, Realm, Career, Sex, IP) ->
    Time = util:unixtime(),
    % --------------------
    %   初始化人物属性
    % --------------------
    %% player_login 属性
    Reg_time = Time,
    Reg_ip = util:ip2bin(IP),

    % player_high  属性(无)

    % player_low   属性
    Lv = 1,

    % player_state 属性
    SceneId = 100,
    X = 20,
    Y = 30,
    Hp = 100,
    Mp = 100,

    % player_attr  属性
    BaseHp = 100,
    BaseMp = 100,


    Sql = io_lib:format(?sql_insert_player_login_one, [AccId, AccName, Reg_time, Reg_ip]),
    case db:execute(Sql) of
        1 ->
			case lib_player_base:get_role_id_by_accname(AccName) of
				null ->
					0;
				Id ->
					PlayerHighSql = io_lib:format(?sql_insert_player_high_one, [Id]),
					PlayerLowSql = io_lib:format(?sql_insert_player_low_one, [Id, Name, Sex, Lv, Career, Realm]),
					PlayerStateSql = io_lib:format(?sql_insert_player_state_one, [Id, SceneId, X, Y, Hp, Mp]),
					PlayerAttrSql = io_lib:format(?sql_insert_player_attr_one, [Id, BaseHp, BaseMp]),
					F1 = fun() ->
						db:execute(PlayerHighSql),
						db:execute(PlayerLowSql),
						db:execute(PlayerStateSql),
						db:execute(PlayerAttrSql),
						true
					end,
					case
						db:transaction(F1) =:= true
					of
						true -> %% sql执行成功
							Id;
						false ->
							db:execute(io_lib:format("delete from player_login where id = ~p limit 1", [Id])),
							0
					end
				end;
        _Other ->
            0
    end.

%% 更新登陆需要的记录
update_login_data(Id, Ip, Time) ->
    db:execute(io_lib:format(?sql_update_login_data, [Time, util:ip2bin(Ip), Id])).

%% 根据id查找账户名称
get_player_login_by_id(Id) ->
    db:get_row(io_lib:format(?sql_player_login_by_id, [Id])).