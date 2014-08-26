%%%--------------------------------------
%%% @Module  : sd
%%% @Author  : xyao
%%% @Email   : jiexiaowen@gmail.com
%%% @Created : 2010.04.15
%%% @Description:  游戏开启
%%%--------------------------------------
-module(sd).
-export([server_start/0, server_stop/0, info/0, all_server_stop/0, install/0, check_info/0]).
-define(SERVER_APPS, [sasl, server]).
-include("common.hrl").
-include("record.hrl").

%%游戏服务器
server_start()->
    try
        ok = start_applications(?SERVER_APPS)
    after
        timer:sleep(100)
    end.

%%停止游戏服务器
server_stop() ->
    io:format("close line [~p]~n", [mod_disperse:server_id()]),
    ok = mod_login:stop_all(),
    ok = stop_applications(?SERVER_APPS),
    erlang:halt().

%%正常停止所有游戏服务器
all_server_stop()->
    io:format("close Allline ~n", []),
    case mod_disperse:server_list() of
        [] ->
            [];
        Server ->
            F = fun(SS) ->
                    Id = SS#server.id,
                    case Id == 0 orelse Id > 90 of
                        true->
                            ok;
                        false ->
                            case net_adm:ping(SS#server.node) of
                                pong ->
                                    case catch rpc:cast(SS#server.node, sd, server_stop, []) of
                                        _ ->
                                            ok
                                    end;
                                pang ->
                                    ok
                            end
                    end,
                    timer:sleep(3000)
                end,
            [F(S) || S <- Server]
    end.

%% 安装mnesia
install() ->
    sd_schema:install().

info() ->
    SchedId      = erlang:system_info(scheduler_id),
    SchedNum     = erlang:system_info(schedulers),
    ProcCount    = erlang:system_info(process_count),
    ProcLimit    = erlang:system_info(process_limit),
    ProcMemUsed  = erlang:memory(processes_used),
    ProcMemAlloc = erlang:memory(processes),
    MemTot       = erlang:memory(total),
    io:format( "abormal termination:
        ~n   Scheduler id:                         ~p
        ~n   Num scheduler:                        ~p
        ~n   Process count:                        ~p
        ~n   Process limit:                        ~p
        ~n   Memory used by erlang processes:      ~p
        ~n   Memory allocated by erlang processes: ~p
        ~n   The total amount of memory allocated: ~p
        ~n",
        [SchedId, SchedNum, ProcCount, ProcLimit,
            ProcMemUsed, ProcMemAlloc, MemTot]),
    {{Y, M, D}, _} = erlang:localtime(),
    File1 = "logs/" ++ integer_to_list(Y) ++ integer_to_list(M) ++ integer_to_list(D) ++ "info.log",
    A = lists:foldl( 
        fun(P, Acc0) -> 
                case is_pid(P) andalso is_process_alive(P)  of
                    true ->
                        {memory, Mem} = erlang:process_info(P, memory),
                        case Mem  > 10000000 of
                            true ->
                                [{P, erlang:process_info(P, registered_name), erlang:process_info(P, memory), erlang:process_info(P, message_queue_len), erlang:process_info(P, current_function), erlang:process_info(P, initial_call)} | Acc0];
                            false ->
                                [{} |Acc0]
                        end;
                    false ->
                        []
                end
        end, [], erlang:processes()),
    [B] = io_lib:format("~p", [A]),
    file:write_file(File1, B),
    ok.

check_info() ->
    lists:foreach( 
        fun(P) -> 
                case is_pid(P) andalso is_process_alive(P)  of
                    true ->
                        {memory, Mem} = erlang:process_info(P, memory),
                        case Mem  > 500000000 of
                            true ->
                                case erlang:process_info(P, current_function) of
                                    {current_function,{mysql_conn,loop,1}} ->
                                        erlang:whereis(mysql_dispatcher) ! {'DOWN', none, process, P, normal},
                                        timer:sleep(3000),
                                        erlang:garbage_collect(P);
                                    _ ->
                                        erlang:garbage_collect(P)
                                        %exit(P,kill)
                                end;
                            false ->
                                []
                        end;
                    false ->
                        []
                end
        end, erlang:processes()).

%%############辅助调用函数##############
manage_applications(Iterate, Do, Undo, SkipError, ErrorTag, Apps) ->
    Iterate(fun (App, Acc) ->
                case Do(App) of
                    ok -> [App | Acc];%合拢
                    {error, {SkipError, _}} -> Acc;
                    {error, Reason} ->
                        lists:foreach(Undo, Acc),
                        throw({error, {ErrorTag, App, Reason}})
                end
        end, [], Apps),
    ok.

start_applications(Apps) ->
    manage_applications(fun lists:foldl/3,
        fun application:start/1,
        fun application:stop/1,
        already_started,
        cannot_start_application,
        Apps).

stop_applications(Apps) ->
    manage_applications(fun lists:foldr/3,
        fun application:stop/1,
        fun application:start/1,
        not_started,
        cannot_stop_application,
        Apps).
