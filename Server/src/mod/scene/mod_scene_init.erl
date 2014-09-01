%%%------------------------------------
%%% @Module  : mod_server_init
%%% @Author  : xyao
%%% @Email   : jiexiaowen@gmail.com
%%% @Created : 2011.07.18
%%% @Description: 场景初始化
%%%------------------------------------
-module(mod_scene_init).
-behaviour(gen_server).
-export([start_link/0, start_new_scene/1, start_scene/1, get_scene_num/0, close_scene/1, clear_scene/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("record.hrl").

%% 开启一个新场景
%% 从拥有场景最少的节点里开启
start_new_scene(Id) ->
    F = fun(N) ->
        N#node.node
    end,
    List = [F(N) || N <- mod_disperse:node_list(), N#node.id =:= 10],
    CurNode = mod_disperse:node_id(),
    case List of
        [Node] ->
            case rpc:call(Node, mod_scene_init, start_scene, [Id]) of
                ScenePid when is_pid(ScenePid) ->
                    ScenePid;
                _ ->
                    undefined
            end;
        _ ->
            %% 判断当前节点是否为游戏服
            case CurNode >= 10 orelse CurNode =:= 0 of
                true ->
                    start_scene(Id);
                false ->
                    undefined
            end
    end.

start_scene(Id) ->
    gen_server:call(?MODULE, {'start_scene', Id}).

close_scene(Id) ->
    gen_server:call(?MODULE, {'close_scene', Id}).

clear_scene(Id) ->
    gen_server:call(?MODULE, {'clear_scene', Id}).

%% 获取场景个数
get_scene_num() ->
    gen_server:call(?MODULE, {'get_scene_num'}).

start_link() ->
    gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

init([]) ->
    case mod_disperse:node_id() =:= 10 of
        true ->
            spawn(fun() -> timer:sleep(300000), open_all_scene() end);
        false ->
            skip
    end,
    {ok, 0}.

handle_cast(_R , Status) ->
    {noreply, Status}.

%% 获取场景个数
handle_call({'get_scene_num'} , _FROM, Status) ->
    {reply, Status, Status};

%% 开启新场景
handle_call({'start_scene', Id} , _FROM, Status) ->
    ScenePid = case catch mod_scene_agent:start_mod_scene_agent(Id) of
        {'EXIT', R} ->
            util:errlog("mod_scene_init:start_scene~p~n", [R]),
            undefined;
        _ScenePid ->
            _ScenePid
    end,
    {reply, ScenePid, Status+1};

handle_call({'close_scene', Id} , _FROM, Status) ->
    mod_scene_agent:close_scene(Id),
    {reply, ok, Status};

handle_call({'clear_scene', Id} , _FROM, Status) ->
    mod_scene_agent:clear_scene(Id),
    {reply, ok, Status};

handle_call(_R , _FROM, Status) ->
    {reply, ok, Status}.

handle_info(_Reason, Status) ->
    {noreply, Status}.

terminate(_Reason, Status) ->
    {ok, Status}.

code_change(_OldVsn, Status, _Extra)->
    {ok, Status}.

%% 开启所有场景
open_all_scene() ->
%%     List = data_scene:get_id_list(),
    List = [],
    F = fun(Id) ->
        mod_scene_agent:get_scene_pid(Id),
        timer:sleep(2000)
    end,
    [ F(Id) || Id <-List ].
