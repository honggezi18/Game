%%%--------------------------------------
%%% @Module  : lib_player_base
%%% @Author  : huangcha
%%% @Created : 2014.08.14  1
%%% @Description:角色相关处理
%%%--------------------------------------
-module(lib_player_base).

%% API
-export([get_role_any_id_by_accname/1,
         get_role_id_by_accname/1,
         get_player_login_data/1,
         get_player_low_data/1,
         get_player_high_data/1,
         get_player_state_data/1,
         get_player_attr_data/1,
         get_pid_by_id/1,
         update_player_state/1,
         update_player_exp/1,
         is_exists/1,
         count_player_attribute/1
]).

-include("common.hrl").
-include("server.hrl").
-include("sql_player_base.hrl").
-include("unite.hrl").

%% 根据账户名称查找角色个数
get_role_any_id_by_accname(AccName) ->
    db:get_all(io_lib:format(?sql_role_any_id_by_accname, [AccName])).

%% 根据账户名称查找ID
get_role_id_by_accname(AccName) ->
    db:get_one(io_lib:format(?sql_role_id_by_accname, [AccName])).

%% 获取player_login登陆所需数据
get_player_login_data(Id) ->
    db:get_row(io_lib:format(?sql_player_login_data, [Id])).

%% 获取player_high登陆所需数据
get_player_high_data(Id) ->
    db:get_row(io_lib:format(?sql_player_high_data, [Id])).

%% 获取player_low登陆所需数据
get_player_low_data(Id) ->
    db:get_row(io_lib:format(?sql_player_low_data, [Id])).

%% 获取player_state登陆所需数据
get_player_state_data(Id) ->
    db:get_row(io_lib:format(?sql_player_state_data, [Id])).

%% 获取player_attr登陆所需数据
get_player_attr_data(Id) ->
    db:get_row(io_lib:format(?sql_player_attr_data, [Id])).

%% 根据角色名称查找ID
get_role_id_by_name(Name) ->
    db:get_one(io_lib:format(?sql_role_id_by_name, [Name])).

%% 获用户信息
get_pid_by_id(Id) ->
    case misc:get_player_process(Id) of
        Pid when is_pid(Pid) ->
            Pid;
        _ ->
            false
    end.

%% 检测指定名称的角色是否已存在
is_exists(Name) ->
    case get_role_id_by_name(Name) of
        null -> false;
        _Other -> true
    end.

%% 回写状态数据
update_player_state(Status) ->
    Scene = Status#player_status.scene,
    X = Status#player_status.x,
    Y = Status#player_status.y,
    Hp = Status#player_status.hp,
    Mp = Status#player_status.mp,
    Quickbar = case util:term_to_bitstring(Status#player_status.quickbar) of
        <<"undefined">> -> <<"[]">>;
        A -> A
    end,
    db:execute(io_lib:format(?sql_update_player_state, [Scene, X, Y, Hp, Mp, Quickbar, Status#player_status.id])).

%% 回写经验值
update_player_exp(Status) ->
    db:execute(io_lib:format(<<"update `player_high` set `exp`=~p where id=~p">>, [Status#player_status.exp, Status#player_status.id])).


%% 人物属性计算
count_player_attribute(PlayerStatus) ->
	NewPlayerStatus = PlayerStatus,
    NewPlayerStatus.