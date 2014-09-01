%%%-------------------------------------------------------------------
%%% @author JoyHuang
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%     公共线数据管理(info)
%%% @end
%%% Created : 15. 八月 2014 16:44
%%%-------------------------------------------------------------------
-module(mod_unite_agent_info).
-author("JoyHuang").

%% API
-export([handle_info/2]).

-include("unite.hrl").
-include("common.hrl").

handle_info(Info, State) ->
    ?ERR("unknown info ~w", [Info]),
    {noreply, State}.
