%%%------------------------------------
%%% @Module  : mod_scene_agent_call
%%% @Author  : huangcha
%%% @Email   : jiexiaowen@gmail.com
%%% @Created : 2012.05.18
%%% @Description: 场景管理call处理
%%%------------------------------------
-module(mod_scene_agent_call).
-export([handle_call/3]).
-include("scene.hrl").
-include("common.hrl").


%% 统一模块+过程调用(call)
handle_call({'apply_call', Module, Method, Args}, _From, State) ->
    {reply, apply(Module, Method, Args), State};

%% 默认匹配
handle_call(Event, _From, Status) ->
    catch util:errlog("mod_server:handle_call not match: ~p", [Event]),
    {reply, ok, Status}.
