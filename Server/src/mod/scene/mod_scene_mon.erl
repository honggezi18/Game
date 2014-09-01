%%%------------------------------------
%%% @Module  : mod_scene_mon
%%% @Author  : xyao
%%% @Created : 2012.07.05
%%% @Description: 收集场景怪物种类
%%%------------------------------------
-module(mod_scene_mon).
-behaviour(gen_server).
-export([start_link/0, 
         match/1,
         insert/1,
         lookup/1
        ]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("scene.hrl").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% 获取数据
match(Q) ->
    gen_server:call(?MODULE, {match, Q}).

%% 插入数据
insert(SceneMon) ->
    gen_server:cast(?MODULE, {insert, SceneMon}).

%% 查找数据
lookup(Id) ->
    gen_server:call(?MODULE, {lookup, Id}).

init([]) ->
    {ok, []}.

handle_call({match, Q}, _From, State) ->
    Data = get(),
    Data1 = [ V || {_K, V} <-Data, V#ets_scene_mon.scene =:= Q],
    {reply, Data1, State};

%% 查找数据
handle_call({lookup, Id}, _From, State) ->
    Data1 = case get(Id) of
        undefined ->
            [];
        Data ->
            Data
    end,
    {reply, Data1, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({insert, SceneMon}, State) ->
    put(SceneMon#ets_scene_mon.id, SceneMon),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
