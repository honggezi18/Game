%%%-------------------------------------------------------------------
%%% @author Ryuu
%%% @copyright (C) 2014, <PLANT>
%%% @doc
%%%     动态配置生成
%%% @end
%%%-------------------------------------------------------------------
-module(conf_dyn).
-author("Ryuu").

-include_lib("kernel/include/file.hrl").
-include("conf_dyn.hrl").

%% API
-export([
    find/2,
    list/1,
    reload_all/0,
    reload_config/1,
    reload_changed/0,
    reload_parallel/0,
    reload_changed_parallel/0
]).

%% @doc 获取配置
find(Conf, Key) ->
    case Conf:get(Key) of
        undefined ->
            [];
        V ->
            [V]
    end.

%% @doc 列出配置文件的内容
list(Conf) ->
    case Conf:list() of
        undefined ->
            [];
        V ->
            V
    end.

%% @doc 生成所有配置文件
reload_all() ->
    Maps = get_config_maps(),
    ?FOREACH(fun generate_file/1, Maps),
    ok.

%% @doc 并行生成所有配置文件
reload_parallel() ->
    Maps = get_config_maps(),
	parallel_generate(Maps).

%% @doc 生成一个
reload_config(ConfName) ->
    Maps = get_config_maps(),
    case lists:keyfind(ConfName, #conf_map.name, Maps) of
        #conf_map{} = Map ->
            generate_file(Map);
        _ ->
            {error, notexist}
    end.

%% @doc 仅生成有变化的配置文件
reload_changed() ->
    Maps = get_changed_maps(),
    ?FOREACH(fun generate_file/1, Maps),
    ok.

%% @doc 并行生成所有配置文件
reload_changed_parallel() ->
    Maps = get_changed_maps(),
	parallel_generate(Maps).


%% ====================================================================
%% Local Functions
%% ====================================================================

%% @doc 获取所有的配置文件列表
get_config_maps() ->
    lists:foldl(fun({ConfName, RelPath, MapType}, Acc) ->
        [#conf_map{name = ConfName, path = RelPath, type = MapType} | Acc];
        ({ConfName, RelPath}, Acc) ->
            [#conf_map{name = ConfName, path = RelPath} | Acc];
        (_, Acc) ->
            Acc
    end, [], ?CONFIG_MAP).

%% @doc 获取修改过的配置文件列表
get_changed_maps() ->
    Maps = get_config_maps(),
    lists:filter(fun conf_need_update/1, Maps).

%% @doc 判断配置是否需要更新
conf_need_update(Map) ->
    #conf_map{name = ConfName, path = RelPath} = Map,
    BeamFile = get_beam_path(ConfName),
    ConfPath = get_config_path(RelPath),
    case file:read_file_info(BeamFile) of
        {ok, #file_info{mtime = TBeam}} ->
            case file:read_file_info(ConfPath) of
                {ok, #file_info{mtime = Time}} when Time < TBeam ->
                    false;
                _ ->
                    true
            end;
        _ ->
            true
    end.

%% @doc 并行生成
parallel_generate(Maps) ->
    MaxWorker = erlang:system_info(logical_processors_available),
    JobList = do_split_list(Maps, MaxWorker),
    %% 启动进程
    Ref = make_ref(),
    Pids =
        [begin
             start_worker(Jobs, self(), Ref)
         end || Jobs <- JobList, Jobs =/= []],
    do_wait_worker(length(Pids), Ref).

%% @doc 等待结果, adapted from mmake
do_wait_worker(0, _Ref) ->
    ok;
do_wait_worker(N, Ref) ->
    receive
        {ack, Ref} ->
            do_wait_worker(N - 1, Ref);
        {error, Error, Ref} ->
            throw({error, Error});
        {'EXIT', _P, _Reason} ->
            do_wait_worker(N, Ref);
        _Other ->
            io:format("receive unknown msg:~p~n", [_Other]),
            do_wait_worker(N, Ref)
    end.

%% @doc 启动worker进程
start_worker(Jobs, Parent, Ref) ->
    spawn_link(fun() ->
        [begin
             case generate_file(Map) of
                 {error, Error} ->
                     Parent ! {error, Error, Ref},
                     exit(error);
                 _ ->
                     ok
             end
         end || Map <- Jobs],
        Parent ! {ack, Ref}
    end).

%% @doc 将L分割成最多包含N个子列表的列表
do_split_list(L, N) ->
    Len = length(L),
    % 每个列表的元素数
    LLen = (Len + N - 1) div N,
    do_split_list(L, LLen, []).

do_split_list([], _N, Acc) ->
    lists:reverse(Acc);
do_split_list(L, N, Acc) ->
    {L2, L3} = lists:split(erlang:min(length(L), N), L),
    do_split_list(L3, N, [L2 | Acc]).

%% @doc 根据映射生成配置
generate_file(Map) ->
    #conf_map{name = ConfName, path = RelPath, type = MapType} = Map,
    case file:path_consult([get_config_dir()], RelPath) of
        {ok, TermList, FullName} ->
            output_file(TermList, ConfName, FullName, MapType);
        {error, Error} ->
            {error, Error}
    end.

%% @doc 生成Erl文件
output_file(TermList, ConfName, FullName, MapType) ->
    Content = get_source(TermList, ConfName, MapType),
    case catch dynamic_compile:from_string(Content) of
        {_, Bin} ->
            %% 写.beam文件
            Path = get_beam_path(ConfName),
            file:write_file(Path, Bin, [write, binary]),
            %% 加载
            code:load_binary(ConfName, FullName, Bin),
            ok;
        {_, _, Error} ->
            {error, Error}
    end.

%% @doc 构造源代码
get_source(TermList, ConfName, MapType) ->
    Dic = get_dict(TermList, MapType),
%%     Default = if MapType =:= set -> undefined; ture -> [] end,
    format("-module(~w).~n-export([get/1, list/0]).~n~sget(_) -> undefined.~nlist() -> ~w.~n", [
        ConfName, [io_lib:format("get(~w) -> ~w;~n", [Key, Val]) || {Key, Val} <- dict:to_list(Dic)], TermList]).

%% @doc 格式化
format(Temp, Content) ->
    lists:flatten(io_lib:format(Temp, Content)).

%% @doc 解析
get_dict(TermList, MapType) ->
    lists:foldl(fun(Term, Acc) ->
        case {MapType, Term} of
            {kv_con, {Key, Val}} -> %% 键值
                dict:store(Key, Val, Acc);
            {kv_list, {Key, Val}} ->
                dict:append(Key, Val, Acc);
            {rec_con, Record} ->    %% record
                Key = erlang:element(2, Record),
                dict:store(Key, Record, Acc);
            {rec_list, Record} ->
                Key = erlang:element(2, Record),
                dict:append(Key, Record, Acc);
            _ ->
                Acc
        end
    end, dict:new(), TermList).

%% @doc 代码目录
get_base_dir() ->
    {ok, [[Path]]} = init:get_argument(root_dir),
    Path.

%% @doc 配置文件目录
get_config_dir() ->
    BaseDir = get_base_dir(),
    filename:join([BaseDir, "config/"]).

%% @doc 配置文件路径
get_config_path(RelPath) ->
    ConfDir = get_config_dir(),
    filename:join([ConfDir, RelPath]).

%% @doc .beam文件目录
get_beam_dir() ->
    BaseDir = get_base_dir(),
    filename:join([BaseDir, "ebin/"]).

%% @doc .beam文件路径
get_beam_path(ConfName) ->
    BeamDir = get_beam_dir(),
    filename:join([BeamDir, erlang:atom_to_list(ConfName) ++ ".beam"]).

