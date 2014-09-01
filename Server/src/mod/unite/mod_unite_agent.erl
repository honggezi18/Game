%%%-------------------------------------------------------------------
%%% @author JoyHuang
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%     公共线数据管理
%%% @end
%%% Created : 15. 八月 2014 15:47
%%%-------------------------------------------------------------------
-module(mod_unite_agent).

%% API
-export([start_link/0,
		 get_online_num/0,
         insert/1,
         lookup/1,
		 match/2,
		 update_lv/2,
         delete/1
        ]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("unite.hrl").
-include("common.hrl").

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

get_online_num()->
	gen_server:call(misc:whereis_name(global, ?MODULE), {get_online_num, all}).

%% 插入数据
insert(EtsUnite) ->
    gen_server:cast(misc:whereis_name(global, ?MODULE), {insert, EtsUnite}).

%% 同步等级信息
update_lv(Id, NewLevel) ->
    gen_server:cast(misc:whereis_name(global, ?MODULE), {update, Id, NewLevel}).

%% 删除数据
delete(Id) ->
    gen_server:cast(misc:whereis_name(global, ?MODULE), {delete, Id}).

%% 查找数据
lookup(Id) ->
    gen_server:call(misc:whereis_name(global, ?MODULE), {lookup, Id}).

%% 多条件查找
%%　返回值由使用者自己定义
match(all_ids, Info) ->
	gen_server:call(misc:whereis_name(global, ?MODULE), {match, all_ids, Info});
match(all_ids_by_lv_gap, Info) ->
	gen_server:call(misc:whereis_name(global, ?MODULE), {match, all_ids_by_lv_gap, Info});
match(match_name, Info) ->
	gen_server:call(misc:whereis_name(global, ?MODULE), {match, match_name, Info});
match(find_partners, Info) ->
	gen_server:call(misc:whereis_name(global, ?MODULE), {match, find_partners, Info});
match(guild_id_pid_sid, Info) ->
	gen_server:call(misc:whereis_name(global, ?MODULE), {match, guild_id_pid_sid, Info});
match(guild_members_id_by_id, Info) ->
	gen_server:call(misc:whereis_name(global, ?MODULE), {match, guild_members_id_by_id, Info});
match(guild_members_id_by_name, Info) ->
	gen_server:call(misc:whereis_name(global, ?MODULE), {match, guild_members_id_by_name, Info});
match(get_scene_role_num, Info) ->
	gen_server:call(misc:whereis_name(global, ?MODULE), {match, get_scene_role_num, Info});
match(get_scene_role_list, Info) ->
	gen_server:call(misc:whereis_name(global, ?MODULE), {match, get_scene_role_list, Info});
match(get_scene_all_role_list, Info) ->
	gen_server:call(misc:whereis_name(global, ?MODULE), {match, get_scene_all_role_list, Info});
match(Type, _Info) ->
	catch util:errlog("mod_chat_agent:match error Type [~p] is undefined ", [Type]),
	[].

init([]) ->
    process_flag(trap_exit, true),
    {ok, []}.

handle_call(Request, From, State) ->
    ?HANDLE_CALL(mod_unite_agent_call, Request, From, State).

handle_cast(Msg, State) ->
    ?HANDLE_CAST(mod_unite_agent_cast, Msg, State).

handle_info(Info, State) ->
    ?HANDLE_CAST(mod_unite_agent_info, Info, State).

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.