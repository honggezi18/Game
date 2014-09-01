%%%------------------------------------
%%% @Module  : mod_server_info
%%% @Author  : xyao
%%% @Email   : jiexiaowen@gmail.com
%%% @Created : 2011.12.16
%%% @Description: 角色info处理
%%%------------------------------------
-module(mod_server_info).
-export([handle_info/2]).
-include("common.hrl").
-include("server.hrl").

%% 默认匹配
handle_info(Info, Status) ->
    catch util:errlog("mod_server:handle_info not match: ~p~nby ps:", [Info, Status]),
    {noreply, Status}.
