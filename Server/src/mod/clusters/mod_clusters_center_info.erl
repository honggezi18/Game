%%%------------------------------------
%%% @Module  : mod_clusters_center_info
%%% @Author  : xyao
%%% @Email   : jiexiaowen@gmail.com
%%% @Created : 2012.10.29
%%% @Description: 跨服中心info处理
%%%------------------------------------
-module(mod_clusters_center_info).
-export([handle_info/2]).

%% 默认匹配
handle_info(Info, Status) ->
    catch util:errlog("mod_clusters_center_info:handle_info not match: ~p", [Info]),
    {noreply, Status}.
