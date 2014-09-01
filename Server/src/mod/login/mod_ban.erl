%% --------------------------------------------------------
%% @Module:           |mod_ban
%% @Author:           |Wu Zhenhua
%% @Email:            |45517168@qq.com
%% @Created:          |2012
%% @Description:      |玩家禁止(封)管理  Ps:也记录玩家的登陆IP
%% --------------------------------------------------------
-module(mod_ban).
-behaviour(gen_server).

-export([
		 check/2											%% 检查IP
		 , ban_ip/1											%% 添加禁止的IP(后台用)
		 , unban_ip/1										%% 取消一个IP的禁止(后台用)
		 , ban_all/0										%% 禁止所有人登陆(仅仅供重启时候时候)
		 , c_opentime/1										%% 修改开服时间
		 , c_opentime_unite/1
		 , bai_ip/1
		 , unbai_ip/1
		 , check_bai/1
		 , get_opentime/0
		]).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("server.hrl").
-include("unite.hrl").
-include("common.hrl").
-include("record.hrl").

-record(rc_ban, {
				player_ip_dict										%% 玩家的IP字典(Key:Ip,Value:Id列表)
			   , ban_ip_list										%% 被禁止的IP列表
			   , bai_list 											%% IP 白名单
			   , ban_all = false									%% 是否禁止所有人登陆 false:否 true:是
				}).

%% ====================================================================
%% Server functions
%% ====================================================================

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

init([]) ->
	NewDict1 = dict:new(),
	NewList1 = db_ban_get(1),
	NewList2 = db_ban_get(2),
    {ok, #rc_ban{
				player_ip_dict = NewDict1									%% 玩家的IP字典
			   , ban_ip_list = 	NewList1									%% 被禁止的IP列表
			   , bai_list = NewList2										%% IP 白名单
			   , ban_all = false											%% 是否禁止所有人登陆 false:否 true:是
				}}.

%% 检查玩家IP是否被禁止
%% @param 玩家IP地址
%% @return passed => 通过 forbid => 被禁止
check(Id, Ip) ->
	case misc:whereis_name(global, ?MODULE) of
		Pid when is_pid(Pid) ->
			gen_server:call(Pid, {check, [Id, Ip]}, 7000);
		_r ->
			%% 进程不存在,直接允许玩家登陆
			passed
	end.

%% 检查玩家IP是否被禁止
%% @param 玩家IP地址
%% @return passed => 通过 forbid => 被禁止
check_bai(Ip) ->
	make_call({check_bai, [Ip]}).

%% 添加白名单IP
%% @return ok => 禁止成功 error => 操作失败
bai_ip(IpList) ->
	make_call({bai_ip, [IpList]}).

%% 解禁IP
%% @return ok => 禁止成功 error => 操作失败
unbai_ip(IpList) ->
	make_call({unbai_ip, [IpList]}).

%% 添加禁止的IP
%% @param IpList = 被禁止的玩家Ip列表
%% @return ok => 禁止成功 error => 操作失败
ban_ip(IpList) ->
	case misc:whereis_name(global, ?MODULE) of
		Pid when is_pid(Pid) ->
			gen_server:call(Pid, {ban_ip, [IpList]}, 7000);
		_r ->
			%% 进程不存在,直接允许玩家登陆
			error
	end.


%% 解禁IP
%% @param IpList = 解禁的玩家Ip列表
%% @return ok => 禁止成功 error => 操作失败
unban_ip(IpList) ->
	case misc:whereis_name(global, ?MODULE) of
		Pid when is_pid(Pid) ->
			gen_server:call(Pid, {unban_ip, [IpList]}, 7000);
		_r ->
			%% 进程不存在,直接允许玩家登陆
			error
	end.

%% 添加禁止所有人登陆
%% @return ok => 禁止成功 error => 操作失败
ban_all() ->
	case misc:whereis_name(global, ?MODULE) of
		Pid when is_pid(Pid) ->
			gen_server:call(Pid, {ban_all}, 7000);
		_r ->
			%% 进程不存在,直接允许玩家登陆
			error
	end.

%% 更改开服时间()
c_opentime(TimeStart)->
    mod_disperse:cast_to_unite(mod_ban, c_opentime_unite, [TimeStart]).

c_opentime_unite(_TimeStart) ->
    TimeStart = util:unixdate(_TimeStart),
	ets:update_element(?SERVER_STATUS, open_time, {#server_status.value, TimeStart}),
	NodeLists = mod_disperse:node_list(),
	lists:foreach(fun(S) ->
						  rpc:cast(S#node.node, ets, update_element, [?SERVER_STATUS, open_time, {#server_status.value, TimeStart}])
				  end, NodeLists).

get_opentime() ->
	case ets:lookup(?SERVER_STATUS, open_time) of
		[] ->
			0;
		[SSV] when is_record(SSV, server_status) ->
			SSV#server_status.value;
		_ ->
			0
	end.
			

handle_call({check, [Id, Ip]}, _From, State) ->	
	[IpData1,IpData2|_] = tuple_to_list(Ip),
	case IpData1=:=175 andalso IpData2=:=102 of
		true -> Reply = forbidip, NewState= State;
		false -> {Reply, NewState} = private_check(Id, Ip, State)
	end,
    {reply, Reply, NewState};
handle_call({check_bai, [Ip]}, _From, State) ->
    Reply = private_check(Ip, State),
    {reply, Reply, State};
handle_call({ban_ip, [IpList]}, _From, State) ->
    {Reply, NewState} = private_ban_ip(IpList, State),
    {reply, Reply, NewState};
handle_call({unban_ip, [IpList]}, _From, State) ->
    {Reply, NewState} = private_unban_ip(IpList, State),
    {reply, Reply, NewState};
handle_call({bai_ip, [IpList]}, _From, State) ->
    {Reply, NewState} = private_bai_ip(IpList, State),
    {reply, Reply, NewState};
handle_call({unbai_ip, [IpList]}, _From, State) ->
    {Reply, NewState} = private_unbai_ip(IpList, State),
    {reply, Reply, NewState};
handle_call({ban_all}, _From, State) ->
    {Reply, NewState} = private_ban_all(State),
    {reply, Reply, NewState};
handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% 内部方法
%% --------------------------------------------------------------------

private_ban_all(State) ->
	NewState = State#rc_ban{ban_all = true},
	{ok, NewState}.

%% 检查是否可以新建帐号
private_check(Ip, State) ->
    BaiIpList = State#rc_ban.bai_list,
	case lists:member(Ip, BaiIpList) of
		true ->
			true;
		false ->
			false
	end.

%% 检查玩家IP是否被禁止
private_check(Id, Ip, State) ->
    Time = util:unixtime(),
    LoginSate = case get({login_time, Id}) of
        undefined ->
            true;
        Lasetime ->
            Time - Lasetime > 6
    end,
    BaiIpList = State#rc_ban.bai_list,
    BanIpList = State#rc_ban.ban_ip_list,
    case LoginSate of
        true -> %% 登陆成功记录下时间
            put({login_time, Id}, Time),
            case State#rc_ban.ban_all =:= true of
                true ->
                    {forbidall, State};
                false ->
					case lists:member(Ip, BaiIpList) of
						true ->
		                    NewPlayerIpDict = dict:store(Id, util:ip2bin(Ip), State#rc_ban.player_ip_dict),
							{passed, State#rc_ban{player_ip_dict = NewPlayerIpDict}};
						false ->
		                    case lists:member(Ip, BanIpList) of
		                        true ->
		                            {forbidip, State};
		                        false ->
		                            NewPlayerIpDict = dict:store(Id, util:ip2bin(Ip), State#rc_ban.player_ip_dict),
		                            {passed, State#rc_ban{player_ip_dict = NewPlayerIpDict}}
		                    end
					end
            end;
        false -> %%　登陆过于平凡
            {login_more, State}
    end.

%% 添加白名单IP(不会立刻处理,只在下次登录或建立帐号的时候生效)
private_bai_ip(IpR, State) ->
	IpL = [IpR],
	{A, B, C, D} = IpR, 
	db_ban_put(2, {A, B, C, D}),
	NewIpList = IpL ++ State#rc_ban.bai_list,
	{ok, State#rc_ban{bai_list = NewIpList}}.

%% 删除白名单IP(不会立刻处理,只在下次登录或建立帐号的时候生效)
private_unbai_ip(IpR, State) ->
	IpL = [IpR],
	{A, B, C, D} = IpR,
	db_ban_del(2, {A, B, C, D}),
	NewIpList = lists:dropwhile(fun(Ip) -> lists:member(Ip, IpL) end,  State#rc_ban.bai_list),
	{ok, State#rc_ban{bai_list = NewIpList}}.

%% 添加禁止的IP
private_ban_ip(IpR, State) ->
%% 	io:format("IpR ~p~n", [IpR]),
	IpList = [IpR],
	F = fun
        (0) ->
            skip;
        (Ip) ->
			erlang:spawn(fun() -> kick_out_player(Ip, State) end)
    end,
	case length(IpList) > 20 of
        true ->
            spawn(
	                fun() ->
	                        lists:foldl(
	                            fun(Ip, Counter) ->
	                                    catch F(Ip),
	                                    case Counter < 20 of
	                                        true ->
	                                            Counter + 1;
	                                        false ->
	                                            timer:sleep(200),
	                                            1
	                                    end
	                            end, 1, IpList)
	                end
				 );
        false ->
            lists:foreach(fun(Id) -> catch F(Id) end, IpList)
    end,
	NewBanIpList = IpList ++ State#rc_ban.ban_ip_list,
	{ok, State#rc_ban{ban_ip_list = NewBanIpList}}.


%% 解禁IP
private_unban_ip(IpR, State) ->
	IpList = [IpR],
	F = fun
        (0) ->
            skip;
        (Ip) ->
			{A, B, C, D} = Ip,
			db_ban_login_ip({A, B, C, D}, 0),
			db_ban_del(1, {A, B, C, D})
    end,
	case length(IpList) > 20 of
        true ->
            spawn(
	                fun() ->
	                        lists:foldl(
	                            fun(Ip, Counter) ->
	                                    catch F(Ip),
	                                    case Counter < 20 of
	                                        true ->
	                                            Counter + 1;
	                                        false ->
	                                            timer:sleep(200),
	                                            1
	                                    end
	                            end, 1, IpList)
	                end
				 );
        false ->
            lists:foreach(fun(Id) -> catch F(Id) end, IpList)
    end,
	NewBanIpList = lists:dropwhile(fun(Ip) -> lists:member(Ip, IpList) end,  State#rc_ban.ban_ip_list),
	{ok, State#rc_ban{ban_ip_list = NewBanIpList}}.

%%　插入数据并把玩家踢下线(根据IP)
kick_out_player(Ip, State) ->
	db_ban_login_ip(Ip, 1),
	db_ban_put(1, Ip),
	PlayerIpDict = State#rc_ban.player_ip_dict,
	IpBin = util:ip2bin(Ip),
	dict:map(fun(Key, Value) ->
		case Value == IpBin of
			true ->
				mod_disperse:cast_to_unite(lib_unite_send, send_to_uid,  [Key, close]),
    			lib_server_send:send_to_uid(Key, close),
				timer:sleep(200);
			false ->
				skip
		end
	end, PlayerIpDict).

%% 获取一个类型的禁止数据
db_ban_get(Type) ->
	Data = [Type],
	SQL  = io_lib:format("select value from ban_info where type=~p", Data),
	case db:get_all(SQL) of
		[] ->
			[];
		D ->
			[Dv||[Dv]<-D]
	end.

%% 更改指定IP的玩家的数据库
db_ban_login_ip(Ip, Type) ->
	Data = [Type, util:ip2bin(Ip)],
	SQL  = io_lib:format("update player_login set status = ~p where last_login_ip = '~s' and status!=9", Data),
    db:execute(SQL).


%% 插入一条新的规则
db_ban_put(Type, Ip) ->
	Data = [Type, util:ip2bin(Ip)],
	SQL  = io_lib:format("insert into ban_info set type=~p, value='~s'", Data),
	db:execute(SQL),
	ok.

%% 删除一条规则(指定类型 1 封号 2 白名单)
db_ban_del(Type, Ip) ->
	Data = [Type, util:ip2bin(Ip)],
	SQL  = io_lib:format("delete from ban_info where type=~p and value='~s'", Data),
	db:execute(SQL),
	ok.

%% 同步调用
make_call(Info)->
	case misc:whereis_name(global, ?MODULE) of
		Pid when is_pid(Pid) ->
			gen_server:call(Pid, Info, 5000);
		_r ->
			[]
	end.