%%%-------------------------------------------------------------------
%%% @author JoyHuang
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%     公共线数据管理(cast)
%%% @end
%%% Created : 15. 八月 2014 16:44
%%%-------------------------------------------------------------------
-module(mod_unite_agent_cast).

%% API
-export([handle_cast/2]).
-include("unite.hrl").
-include("common.hrl").

handle_cast({insert, EtsUnite}, State) ->
    put(EtsUnite#ets_unite.id, EtsUnite),
    {noreply, State};

handle_cast({update, Id, NewLevel}, State) ->
	case get(Id) of
        undefined ->
            skip;
        Data ->
            put(Data#ets_unite.id, Data#ets_unite{lv = NewLevel})
    end,
    {noreply, State};

handle_cast({delete, Id}, State) ->
    erase(Id),
    {noreply, State};

handle_cast(Request, State) ->
    ?ERR("unknown request ~w", [Request]),
    {noreply, State}.