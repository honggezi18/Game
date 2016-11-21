
%%%-----------------------------------
%%% @Module  : mod_login
%%% @Author  : xyao
%%% @Email   : jiexiaowen@gmail.com
%%% @Created : 2011.06.14
%%% @Description: 登陆模块
%%%-----------------------------------
-module(mod_login).
-export([
        login/2,
        logout/1,
        stop_player/1,
        stop_all/0,
        send_msg/1,
        save_online/1,
		save_online/2
]).
-include("common.hrl").
-include("record.hrl").
-include("server.hrl").
-include("unite.hrl").

%%用户登陆
login(do, [Id, Ip, Socket])  ->
    %% 检查用户登陆和状态已经登陆的踢出出去
    check_player(Id),
    {ok, Pid} = mod_server:start(),
    Time = util:unixtime()+1, % 11
    %%更新
    case lib_login:update_login_data(Id, Ip, Time) of
        1 -> %%登陆启动
            case catch server_login([Id, Socket, Time, Pid]) of
                {'EXIT', _R1} ->
                    catch util:errlog("login error :~p", [_R1]),
                    {error, 0};
                _R2 ->
                    %% 登录日志
                    {ok, Pid}
            end;
        _Error ->
            {error, 0}
    end;

%%登陆检查入口
%%Data:登陆验证数据
%%Arg:tcp的Socket进程,socket ID
login(start, [Id, Accname, Ip, Socket]) ->
    case lib_login:get_player_login_by_id(Id) of
        [] ->
            {error, 0};
        [Aname, Status] ->
            case Status of
                0 -> %%正常
                    case binary_to_list(Aname) =:= Accname of
                        true ->
                            login(do, [Id, Ip, Socket]);
                        false ->
                            {error, 0}
                    end;
                1 -> %% 封号
                    {error, 6};
                2 -> %% 买卖元宝封号
                    {error, 7};
                3 -> %% 不正当竞争封号
                    {error, 8};
				9 -> %% 升级版封号
					{error, 6};
                _ -> %% 状态不正常
                    {error, 9}
            end
    end;

login(_R, _S) ->
    {error, 0}.

%%退出登陆
logout(Pid) when is_pid(Pid) ->
    case misc:is_process_alive(Pid) of
        true ->
            mod_server:stop(Pid);
        false ->
            skip
    end;

%%退出游戏系统
logout(PS) when is_record(PS, player_status)->
	%% 更新离线时间------
	TimeLogout = util:unixtime()+1,

    %%删除ETS记录
    ets:delete(?ETS_ONLINE, PS#player_status.id),
    %% 回写关键数据
    lib_player_base:update_player_state(PS),
    lib_player_base:update_player_exp(PS),
	db:execute(io_lib:format(<<"update `player_login` set `last_logout_time`= ~p, `online_flag`=0  where id = ~p">>
							 , [TimeLogout, PS#player_status.id])),

%% 	%% 下线每日记录器清除
%%     mod_daily:stop(PS#player_status.dailypid),

    %% 公共线清理数据
    lib_unite_init:off_line(PS),

    %% 场景服务器清理数据
    mod_scene_agent:leave(PS),

    ok.

%% 把玩家踢出去
stop_player(PlayerId) ->
    case lib_player_base:get_pid_by_id(PlayerId) of
        false -> skip;
        Pid -> logout(Pid)
    end.

%% 把所有在线玩家踢出去
stop_all() ->
    mod_ban:ban_all(),
    do_stop_all(ets:tab2list(?ETS_ONLINE)).

%% 让所有玩家自动退出
do_stop_all([]) ->
    ok;
do_stop_all([H | T]) ->
    %% 关闭socket
    lib_server_send:send_to_sid(H#ets_online.sid, close),
    timer:sleep(50),
    do_stop_all(T).
    
%%发消息
send_msg(Socket) ->
    receive
        {send, close} ->
            gen_tcp:close(Socket),
            send_msg(Socket);
        {send, Bin} ->
            gen_tcp:send(Socket, Bin),
            send_msg(Socket);
        _ ->
            send_msg(Socket)
    end.

save_online(PlayerStatus) when is_record(PlayerStatus, player_status)->
    ets:insert(?ETS_ONLINE, #ets_online{
            id = PlayerStatus#player_status.id,
            sid = PlayerStatus#player_status.sid,
            pid = PlayerStatus#player_status.pid
        });

save_online(R) ->
    util:errlog("save_online error :~p", [R]).

save_online(R, _) ->
    util:errlog("save_online base error :~p", [R]).

%% 游戏服务器登陆
server_login([Id, Socket, LastLoginTime, Pid]) ->
	_NowTime = util:unixtime(),
    %% 玩家玩家数据
    [Accname, RegTime, Gm, TalkLim, TalkLimTime, _Last_logout_time] = lib_player_base:get_player_login_data(Id),
    [NickName, Sex, Lv, Career, Realm, _Guild_id, Image] = lib_player_base:get_player_low_data(Id),
	[Gold, Bgold, Coin, Bcoin, Exp] = lib_player_base:get_player_high_data(Id),
	[Scene, X, Y, Hp, Mp, Quickbar] = lib_player_base:get_player_state_data(Id),
	[_BaseHp, _BaseMp]  = lib_player_base:get_player_attr_data(Id),

	%% 打开广播信息进程
    Sid = list_to_tuple(lists:map(
        fun(Wid)->
                SendPro = spawn_link(fun()->send_msg(Socket) end),
                misc:register(global, misc:player_send_process_name(Id, Wid), SendPro),
                SendPro
        end,lists:seq(1, ?SERVER_SEND_MSG))),

    %% 设置mod_player 状态
    _PlayerStatus = #player_status {
        id = Id,
        sid = Sid,
        accname = binary_to_list(Accname),
        nickname = binary_to_list(NickName),
        reg_time = RegTime,
        sex = Sex,
        lv = Lv,
        scene = Scene,
        x = X,
        y = Y,
        exp = Exp,
        exp_lim = 1000000000000,
        hp = Hp,
        mp = Mp,
        gm = Gm,
        talk_lim = TalkLim,
        talk_lim_time = TalkLimTime,
        socket = Socket,
        last_login_time = LastLoginTime,
        pid = Pid,
        career = Career,
        realm = Realm,
        gold = Gold,
        bgold = Bgold,
        coin = Coin,
        bcoin = Bcoin,
        battle_status = [],
        quickbar = case util:bitstring_to_term(Quickbar) of undefined -> []; Qb -> Qb end,

		dailypid = none,
		image = Image,
        platform = config:get_platform(),
        server_num = config:get_server_num()
    },

    PlayerStatus1 = lib_player_base:count_player_attribute(_PlayerStatus),
    PlayerStatus = PlayerStatus1,

    %% 初始化任务
    %% todo: 默认触发第一个任务，目前先放到这里

    %% 把数据传到玩家进程里
    gen_server:cast(Pid, {'base_set_data', PlayerStatus}),
    PlayerProcessName = misc:player_process_name(Id),
    misc:register(global, PlayerProcessName, Pid),

    %%更新ETS_ONLINE在线表
    save_online(PlayerStatus),

    %%更新公共线
    lib_unite_init:save_online(PlayerStatus),

    %%场景服务器加载数据(测试)
    mod_scene_agent:load_scene(PlayerStatus),

    ok.

%%检查用户是否登陆了
check_player(Id) ->
    case lib_player_base:get_pid_by_id(Id) of
        false ->
            skip;
        Pid ->
            login_outside(Id, Pid),
            timer:sleep(2000)
    end.

login_outside(_Id, Pid) ->
    %通知客户端账户在别处登陆
%%     {ok, BinData} = pt_590:write(59004, 5),
%%     lib_server_send:send_to_uid(Id, BinData),
    logout(Pid),
    ok.
