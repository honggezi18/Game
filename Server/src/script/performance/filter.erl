-module(filter).
-export([filter1/0,filter2/0,file0/0,file/0, filter/1,filter_do/1,read0/2, ticket/3]).
-complie(export_all).
file0() ->
    {ok, S} = file:open("data.txt", read),
    {ok, S1} = file:open("data1.txt", write),
    ok = read_write(S, S1),
    file:close(S),
    file:close(S1).
read_write(S, S1) ->
    Data = io:get_line(S, ''),
    if
        Data =/= eof ->
            Data1 = lists:delete(lists:last(Data), Data),
            [Data2] = io_lib:format("~p", [Data1]),
            DataBin = erlang:list_to_binary(Data2),
            io:format(S1, "~s~n", [DataBin]),
            read_write(S, S1);
        true ->
            ok
    end.
file() ->
    {ok, S} = file:open("data.txt", read),
    L = read(S, []),
    io:format("Text size : ~p Filter key size : ~p~n", [length(L), length(data_filter:talk())]),
%    statistics(runtime),
%    filter(L),
%    {_, Time} = statistics(runtime),
%    U = Time*1000/length(L),
%    io:format("filter average time=~p microseconds~n",[U]),
%    {Time1, _} = timer:tc(?MODULE, filter, [L]),
%    U1 = Time1/length(L),
%    io:format("timer:tc filter average time=~p microseconds~n",[U1]),
    {Time2, _} = timer:tc(util, filter_text, ["五彩风车变换卷"]),
    io:format("timer:tc filter single text time=~p microseconds~n",[Time2]).
read(S, L) ->
    Data = io:get_line(S, ''),
    if
        Data =/= eof ->
            Data1 = lists:delete(lists:last(Data), Data),
            L2 = lists:append(L, [Data1]),
            read(S, L2);
        true ->
            L
    end.
filter([H|L]) ->
    util:filter_text(H),
    filter(L);
filter([]) ->
    ok.
filter1() ->
    mod_word:init(),
    {ok, S} = file:open("data1.txt", read),
    Terms = read0(S, []),
    {Time, _} = timer:tc(?MODULE, filter_do, [Terms]),
    U = Time/length(Terms),
    io:format("timer:tc filter average time=~p microseconds~n",[U]),
    [Term] = io_lib:format("~ts", ["阿扁万岁"]),
    {Time1,_} = timer:tc(mod_word, replace_sensitive_talk, [Term]),
    io:format("timer:tc filter time=~p microseconds~n",[Time1]).
filter2() ->
    mod_word:init(),
    {ok, S} = file:open("data1.txt", read),
    Terms = read0(S, []),
    statistics(runtime),
    filter_do(Terms),
    {_, Time} = statistics(runtime),
    U = Time*1000/length(Terms),
    io:format("mod_word average time=~p ms~n", [U]),
    filter(Terms),
    {_, Time1} = statistics(runtime),
    U1 = Time1*1000/length(Terms),
    io:format("erlang reg average time=~p ms~n", [U1]).
filter_do(Terms) ->
    lists:foreach(fun(X) -> mod_word:replace_sensitive_talk(X) end, Terms).
read0(S,Terms) ->
    Data = io:get_line(S, ''),
    if
        Data =/= eof ->
            Data1 = lists:delete(lists:last(Data), Data),
            [Term] = io_lib:format("~ts", [Data1]),
            read0(S, [Term|Terms]);
        true ->
            Terms
    end.

ticket(Accid, Accname, Tstamp) ->
    TICKET = config:get_ticket(),
    Hex = util:md5(lists:concat([Accid, Accname, Tstamp, TICKET])),
    io:format("ticket ~p~n", [Hex]).
	
