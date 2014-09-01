%%%------------------------------------
%%% @Module  : mod_server_call
%%% @Author  : xyao
%%% @Email   : jiexiaowen@gmail.com
%%% @Created : 2011.12.16
%%% @Description: 角色call处理
%%%------------------------------------
-module(mod_server_call).
-export([handle_call/3]).
-include("server.hrl").

%%==========基础功能base============ 
%%获取用户信息
handle_call('base_data', _from, Status) ->
    {reply, Status, Status};

%% 获取分类的玩家信息_根据模块调用
handle_call({'get_data', Type}, _from, Status) ->
    %% 根据类型_调用各个功能自己的解析模块获取相关的数据
    case Type of
		_ ->
	    	{reply, Status, Status}
    end;

%% 默认匹配
handle_call(Event, _From, Status) ->
    catch util:errlog("mod_server:handle_call not match: ~p", [Event]),
    {reply, ok, Status}.
