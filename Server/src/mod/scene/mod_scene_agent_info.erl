%%%------------------------------------
%%% @Module  : mod_scene_agent_info
%%% @Author  : huangcha
%%% @Email   : jiexiaowen@gmail.com
%%% @Created : 2012.05.18
%%% @Description: 场景管理info处理
%%%------------------------------------
-module(mod_scene_agent_info).
-include("scene.hrl").
-export([handle_info/2]).

%% 默认匹配
handle_info(Info, Status) ->
    catch util:errlog("mod_server:handle_info not match: ~p", [Info]),
    {noreply, Status}.
