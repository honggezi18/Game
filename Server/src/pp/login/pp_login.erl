%%%--------------------------------------
%%% @Module  : pp_login
%%% @Author  : xyao
%%% @Email   : jiexiaowen@gmail.com
%%% @Created : 2010.04.29
%%% @Description:  注册登录系统
%%%--------------------------------------
-module(pp_login).
-export([handle/2, 
         check_heart_time/2,
         validate_name/1]).

handle(Status, Data) ->
	try pp_login2:handle(Status, Data) of
		Result -> Result
	catch 
		Error1:Error2 ->
		catch util:errlog("catch cmd:~p ===> ~p:~p  ~n at ~n ~p", [ Error1, Error2, erlang:get_stacktrace(), Data]),
		{error, Status}
	end.

%% 角色名合法性检测
validate_name(Name) ->
    try pp_login2:validate_name(Name) of
		Result -> Result
	catch 
		Error1:Error2 ->
		catch util:errlog("catch pp_login:validate_name error ===> ~p:~p  ~n at ~n ~p", [Error1, Error2, erlang:get_stacktrace()]),
		false
	end.

%% 检查心跳包发送的频率
check_heart_time(NowTime, LimTime) ->
	try pp_login2:check_heart_time(NowTime, LimTime) of
		Result -> Result
	catch 
		Error1:Error2 ->
		catch util:errlog("catch pp_login:check_heart_time error ===> ~p:~p  ~n at ~n ~p", [Error1, Error2, erlang:get_stacktrace()]),
		true
	end.
