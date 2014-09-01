%%%--------------------------------------
%%% @Module  : pp_login
%%% @Author  : xyao
%%% @Email   : jiexiaowen@gmail.com
%%% @Created : 2010.04.29
%%% @Description:  注册登录系统
%%%--------------------------------------
-module(pp_login2).
-export([handle/2, 
         check_heart_time/2,
         validate_name/1]).
-include("common.hrl").
-include("server.hrl").
-include("unite.hrl").
-include("pt100_pb.hrl").

%%记录用户初始数据
-record(player, {
        socket = none,      % socket
        pid = none,         % 玩家进程
        login  = 0,         % 是否登录
        accid  = 0,         % 账户id
        accname = none,     % 账户名
        timeout = 0,        % 超时次数
        req_count = 0,      % 请求次数
        req_list = [],      % 请求列表
        req_time = 0        % 请求时间
    }).


%%登陆验证
handle(Player, #pt10000_tos{acc_id=Accid, accname=Accname}=Data) ->
    try is_bad_pass(Data) of
        true ->
            UserInfo = lib_player_base:get_role_any_id_by_accname(Accname),
            Uid = case UserInfo of
                [] ->
                    0;
                [[Id] | _] ->
                    Id
            end,
		    %%取选择最小人数的职业来返回
		    Career = 0,
            {ok, BinData} = pt:pack(10000, #pt10000_toc{code=Uid, num=length(UserInfo), career=Career, time=util:unixtime()}),
            lib_server_send:send_one(Player#player.socket, BinData),
            {ok, Player#player{login = 1, accid = Accid, accname = Accname}};
        false -> 
            {ok, Player}
    catch
        _:_ -> 
			{ok, Player}
    end;

%% 获取角色列表
handle(Player, #pt10002_tos{}) when Player#player.login == 1 ->
    L = lib_login:get_role_list(Player#player.accname),
    Roles = case L of
        [] -> [];
        _ ->
            lists:map(
                fun([Pid, Status, Name, Sex, Lv, Career, Realm]) -> 
                    #pt10002_toc_player{id = Pid
                                        ,status = Status
                                        ,career = Career
                                        ,sex = Sex
                                        ,lv = Lv
                                        ,name = Name
                                        ,realm = Realm
                                   }
                           end,  L)
    end,
    {ok, BinData} = pt:pack(10002, #pt10002_toc{players=Roles}),
    lib_server_send:send_one(Player#player.socket, BinData);

%% 创建角色
handle(Player, #pt10003_tos{realm=Realm, career=Career, sex=Sex, name=Name})
    when is_list(Player#player.accname), is_list(Name), Player#player.login == 1 ->
    Accid = Player#player.accid,
    Accname = Player#player.accname,
    IP = util:get_ip(Player#player.socket),
    case validate_name(existed, Name) of  %% 角色名合法性检测
        {false, Msg} ->
            {ok, BinData} = pt:pack(10003, #pt10003_toc{code=Msg, id=0}),
            lib_server_send:send_one(Player#player.socket, BinData);
        true ->
            case lib_login:create_role(Accid, Accname, Name, Realm, Career, Sex, IP) of
                0 ->
                    %%角色创建失败
                    {ok, BinData} = pt:pack(10003, #pt10003_toc{code=0, id=0}),
                    lib_server_send:send_one(Player#player.socket, BinData);
                Id ->
                    %%创建角色成功
                    {ok, BinData} = pt:pack(10003,  #pt10003_toc{code=1, id=Id}),
                    lib_server_send:send_one(Player#player.socket, BinData)
            end
    end;

%% 进入游戏
handle(Player, #pt10004_tos{id=Id, time=_Time, ticket=_Ticket})  when Player#player.login == 1 ->
%%     case util:check_char_encrypt(Id, Time, Ticket) of
    case true of
        true ->
            %% 获取IP
            Ip = util:get_ip(Player#player.socket),
            case mod_login:login(start, [Id, Player#player.accname, Ip, Player#player.socket]) of
                {error, MLR} ->
                    %%告诉玩家登陆失败
                    {ok, BinData} = pt:pack(10004, #pt10004_toc{code=MLR}),
                    lib_server_send:send_one(Player#player.socket, BinData),
                    {ok, Player};
                {ok, Pid} ->
                    %%告诉玩家登陆成功
                    {ok, BinData} = pt:pack(10004, #pt10004_toc{code=1}),
                    lib_server_send:send_one(Player#player.socket, BinData),
                    %% 进入逻辑处理
                    {ok, enter, Player#player{pid = Pid}}
            end;
        false ->
            {ok, BinData} = pt:pack(10004, #pt10004_toc{code=9}),
            lib_server_send:send_one(Player#player.socket, BinData),
            {ok, Player}
    end;

%% %%登录心跳包
handle(Player, #pt10006_tos{}) when is_record(Player, player) ->
    {ok, BinData} = pt:pack(10006, #pt10006_toc{}),
    lib_server_send:send_one(Player#player.socket, BinData);

%%进入游戏后心跳包
handle(Status, #pt10006_tos{}) when is_record(Status, player_status) ->
    Time = util:longunixtime(),
    T = case get("pp_base_heartbeat_last_time") of
        undefined->
            0;
        _T ->
            _T
    end,
    put("pp_base_heartbeat_last_time", Time),
    case Time - T < 4800 of
        true ->
            catch util:errlog("heart time error = 2, role = ~p", [Status#player_status.id]),
            {ok, BinData} = pt_590:write(59004, 4),
            lib_server_send:send_one(Status#player_status.socket, BinData),
            ok;
            %% 关闭socket
%%             lib_server_send:send_to_sid(Status#player_status.sid, close);
        false ->
            skip
    end,
    {ok, BinData2} = pt:pack(10006, #pt10006_toc{}),
    lib_server_send:send_one(Status#player_status.socket, BinData2),
    ok;

%% 检查名字
handle(Status, #pt10010_tos{name=Name}) when Status#player.login == 1 ->
    case validate_name(existed, Name) of  %% 角色名合法性检测
        true ->
            {ok, BinData} = pt:pack(10010, #pt10010_toc{code=1}),
            lib_server_send:send_one(Status#player.socket, BinData);
		_R ->
            {ok, BinData} = pt:pack(10010, #pt10010_toc{code=0}),
            lib_server_send:send_one(Status#player.socket, BinData)
    end,
    ok;

handle(_Status, _Data) ->
    io:format("pp_login2 handle no match : ~p~n", [[_Data]]),
    ?DEBUG("pp_base no match", []),
    {error, "pp_base no match"}.

%% ------------ 私有函数 --------------
%%通行证验证
is_bad_pass(#pt10000_tos{acc_id=Accid, accname=Accname, time=Tstamp, ticket=TK}) ->
    TICKET = config:get_ticket(),
    Hex = util:md5(lists:concat([Accid, Accname, Tstamp, TICKET])),
    E = util:unixtime() - Tstamp,
    E < 86400 andalso Hex =:= TK. %%失效时间

%% 角色名合法性检测
validate_name(Name) ->
    validate_name(len, Name).

%% 角色名合法性检测:长度
validate_name(len, Name) ->
    case unicode:characters_to_list(list_to_binary(Name)) of
        CharList when is_list(CharList)->
            Len = string_width(CharList),
            case Len < 17 andalso Len > 2 of
                true ->
                    validate_name(keyword, Name);
                false ->
                    %%角色名称长度为2~6个汉字
                    {false, 5}
            end;
        {error, _Reason} ->
            %%非法字符
            {false, 4}
    end;

%%判断角色名是否已经存在
%%Name:角色名
validate_name(existed, Name) ->
    case lib_player_base:is_exists(Name) of
        true ->
            %角色名称已经被使用
            {false, 3};
        false ->
            true
    end;

%%判断角色名是有敏感词
%%Name:角色名
validate_name(keyword, Name) ->
    case util:check_keyword(Name) of
        false ->
            validate_name(existed, Name);
        _ ->
            {false, 7}
    end;

validate_name(_, _Name) ->
    {false, 2}.

%% 字符宽度，1汉字=2单位长度，1数字字母=1单位长度
string_width(String) ->
    string_width(String, 0).
string_width([], Len) ->
    Len;
string_width([H | T], Len) ->
    case H > 255 of
        true ->
            string_width(T, Len + 2);
        false ->
            string_width(T, Len + 1)
    end.

%% 检查心跳包发送的频率
check_heart_time(NowTime, LimTime) ->
    case get("pp_base_heartbeat_last_time") of
        undefined->
            put("pp_base_heartbeat_last_time", 0),
            false;
        T ->
            NowTime - T > LimTime
    end.
