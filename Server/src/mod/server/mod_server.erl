%%%------------------------------------
%%% @Module  : mod_server
%%% @Author  : xyao
%%% @Email   : jiexiaowen@gmail.com
%%% @Created : 2011.06.14
%%% @Description: 角色处理
%%%------------------------------------
-module(mod_server).
-behaviour(gen_server).
-export([start/0, stop/1, set_dungeon_pid/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3,routing2/3]).
-include("common.hrl").
-include("server.hrl").

%% 设置副本进程PID
set_dungeon_pid(Pid, Val) ->
    case is_pid(Pid) of
        false ->
            false;
        true ->
            gen_server:cast(Pid, {set_dungeon_pid, Val})
    end.

%%开始
start() ->
    gen_server:start(?MODULE, [], []).

init([]) ->
    process_flag(priority, max),
    {ok, none}.

%%停止本游戏进程
stop(Pid) ->
    catch gen:call(Pid, '$gen_call', stop).

%%游戏进程死掉修改状态
terminate(_Reason, _Status) ->
    ok.

%% 停止游戏进程
handle_cast(stop, Status) ->
    catch mod_login:logout(Status),
    {stop, normal, Status};

%% handle_cast信息处理
handle_cast(Event, Status) ->
    misc:monitor_pid(handle_cast, Event),
    mod_server_cast:handle_cast(Event, Status).

%%停止游戏进程
handle_call(stop, _From, Status) ->
    catch mod_login:logout(Status),
    {stop, normal, Status};

%%处理socket协议
%%cmd：命令号
%%data：协议体
handle_call({'SOCKET_EVENT', Cmd, Bin}, _From, Status) ->
	try routing(Cmd, Status, Bin) of
        {ok, Status1} when is_record(Status1, player_status) ->
            {reply, ok, Status1};
        {ok, V, Status1} when is_record(Status1, player_status) ->
            do_return_value(V, Status1),
            {reply, ok, Status1};
        {ok, Status1} ->
            catch util:errlog("badrecord: cmd:~p:~p", [Cmd, Status1]),
            {reply, ok, Status};
        {ok, V, Status1} ->
            catch util:errlog("syn badrecord: cmd:~p:state:~p:~p", [Cmd, V, Status1]),
            {reply, ok, Status};
        {'EXIT', R} ->
            catch util:errlog("cmd:~p:~p:~p", [Cmd, R, Bin]),
            {reply, ok, Status};
        _ ->
            {reply, ok, Status}
    catch
		Error1:Error2 ->
		catch util:errlog("catch cmd:~p ===> ~p:~p  ~n at ~n ~p", [Cmd, Error1, Error2, erlang:get_stacktrace()]),
		{reply, ok, Status}
	end;

%% handle_call信息处理
handle_call(Event, From, Status) ->
    misc:monitor_pid(handle_call, Event),
    mod_server_call:handle_call(Event, From, Status).

%% handle_info信息处理
handle_info(Info, Status) ->
    misc:monitor_pid(handle_info, Info),
    mod_server_info:handle_info(Info, Status).

code_change(_oldvsn, Status, _extra) ->
    {ok, Status}.

%%
%% ------------------------私有函数------------------------
%%
%% 路由
%%cmd:命令号
%%Socket:socket id
%%data:消息体
routing(Cmd, Status, Bin) ->
    %%取前面二位区分功能类型
    [H1, H2, H3, _, _] = integer_to_list(Cmd),
    case cd_cmd(Cmd) of
        true ->
            case [H1, H2, H3] of
                "100" -> pp_login:handle(Status, Bin);
                "120" -> pp_scene:handle(Status, Bin);
                _ ->
                    ?ERR("[~P]路由失败.", [Cmd]),
                    {error, "Routing failure"}
            end;
        false ->
            skip
    end.
    
%%直接执行协议内容
%% return ok | #player_status
routing2(Cmd, Status, Args) ->
     case catch routing(Cmd, Status, Args) of
        {ok, Status1} when is_record(Status1, player_status) ->
            Status1;
        {ok, V, Status1} when is_record(Status1, player_status) ->
            do_return_value(V, Status1),
            Status1;
        {ok, Status1} ->
            catch util:errlog("routing2 badrecord: cmd:~p:~p", [Cmd, Status1]),
            Status;
        {ok, V, Status1} ->
            catch util:errlog("routing2 syn badrecord: cmd:~p:state:~p:~p", [Cmd, V, Status1]),
            Status;
        {'EXIT', R} ->
            catch util:errlog("routing2 cmd:~p:~p:~p", [Cmd, R, Args]),
            Status;
        _ ->
            Status
    end
.
	
%% 处理路由返回值
do_return_value(Value, _Status) ->
    case Value of
        _ ->
            skip
    end.

%% 需要加cd时间的协议
%%List = [{12001, 3},{120001,4}];  12001是需要加cd的协议号,3是cd的时间长度单位秒
cd_cmd(_Cmd) ->
    true.
