%%%-----------------------------------
%%% @Module  : util
%%% @Author  : xyao
%%% @Email   : jiexiaowen@gmail.com
%%% @Created : 2011.06.14
%%% @Description: 公共函数
%%%-----------------------------------
-module(util).
-include("common.hrl").
-include("record.hrl").
-export([
    log/5,
    log_utf8/1,
    unixtime/0,
    unixtime/1,
    unixdate/0,
    unixdate/1,
    unixtime_to_now/1,
    longunixtime/0,
    diff_day/1,
    md5/1,
    rand/2,
    ceil/1,
    floor/1,
    sleep/1,
    sleep/2,
    get_list/2,
    implode/2,
    implode/3,
    explode/2,
    explode/3,
    for/3,
    for/4,
    string_to_term/1,
    bitstring_to_term/1,
    term_to_string/1,
    term_to_string2/1,
    term_to_bitstring/1,
    filter_text/2,
    filter_text_gm/1,
    filter_name/1,
    check_keyword/1,
    check_length/2,
    check_length/3,
    string_width/1,
    string_width/2,
    ip2bin/1,
    all_to_binary/1,
    errlog/2,
    errlog/5,
    get_day_of_week/0,
    get_day_of_week/1,
    list_rand/1,
    to_atom/1,
    get_ip/1,
    check_kfz/0,
    make_sure_list/1,
    make_sure_binary/1,
    filter_string/2,
    seconds_to_localtime/1,
    is_same_date/2,
    object_to_list/1,
    foreach_ex/3,
    map_ex/3,
    get_list_elem_index/2,
    replace_list_elem/3,
    is_same_week/2,
    get_midnight_seconds/1,
    get_diff_days/2,
    get_today_midnight/0,
    get_today_midnight/1,
    get_seconds_from_midnight/0,
    get_seconds_from_midnight/1,
    check_open_day/1,
    check_open_day/2,
    get_open_time/0,
    to_term/1,
    boolean_to_integer/1,
    integer_to_boolean/1,
    unixtime_to_time_string/1,
    get_week_time/0,
    get_week_time/1,
    keyfind/3,
    get_last_day_string/1,
    get_last_day_local_time/1,
    get_date_string/1,
    get_open_day/0,
    list_shuffle/1,
    counter/1,
    counter/2,
    record_to_list/1,
    list_to_record/2,
    check_char_encrypt/3,
    get_server_id/0,
    min_ex/2,
    max_ex/2,
    is_all_same/2,
    add_days/2,
    get_midnight_seconds/0,
    format/2,
    get_config_zone/2,
    get_config_zone/3,
    list_to_text/1
]).

%% 在List中的每两个元素之间插入一个分隔符
implode(_S, []) ->
    [<<>>];
implode(S, L) when is_list(L) ->
    implode(S, L, []).
implode(_S, [H], NList) ->
    lists:reverse([thing_to_list(H) | NList]);
implode(S, [H | T], NList) ->
    L = [thing_to_list(H) | NList],
    implode(S, T, [S | L]).

%% 字符->列
explode(S, B) ->
    re:split(B, S, [{return, list}]).
explode(S, B, int) ->
    [list_to_integer(Str) || Str <- explode(S, B), length(Str) > 0].

thing_to_list(X) when is_integer(X) -> integer_to_list(X);
thing_to_list(X) when is_float(X) -> float_to_list(X);
thing_to_list(X) when is_atom(X) -> atom_to_list(X);
thing_to_list(X) when is_binary(X) -> binary_to_list(X);
thing_to_list(X) when is_list(X) -> X.

%% 日志记录函数

%% 调试状态可以把此部分关闭
log(_T, _F, _A, _Mod, _Line) ->
    ok.

log_utf8(Msg) when is_list(Msg) ->
    {ok, S} = file:open("log_utf8.txt", append),
    [DescList] = io_lib:format("~ts", [Msg]),
    DescBin = erlang:iolist_to_binary(DescList),
    DescList2 = unicode:characters_to_list(DescBin),
    _Bin = unicode:characters_to_binary(DescList2),
    file:close(S).
%log(T, F, A, Mod, Line) ->
%    {{Y, M, D},{H, I, S}} = erlang:localtime(),
%    Format = list_to_binary("#" ++ T ++" ~s[~w:~w] " ++ F ++ "\r\n~n"),
%    Date = list_to_binary([integer_to_list(Y),"-", integer_to_list(M), "-", integer_to_list(D), " ", integer_to_list(H), ":", integer_to_list(I), ":", integer_to_list(S)]),
%    io:format(unicode:characters_to_list(Format), [Date, Mod, Line] ++ A).

%% 日志记录函数
errlog(F, A) ->
    {{Y, M, D}, {H, I, S}} = erlang:localtime(),
    case get("errlog") of
        undefined ->
            LogPath = config:get_log_path(),
            File1 = LogPath ++ "/errlog_" ++ integer_to_list(Y) ++ "_" ++ integer_to_list(M) ++ "_" ++ integer_to_list(D) ++ ".txt",
            {ok, Fl} = file:open(File1, [write, append]),
            put("errlog", Fl),
            F3 = Fl;
        F2 ->
            F3 = F2
    end,
    Format = list_to_binary("#error" ++ " ~s \r\n" ++ F ++ "\r\n~n"),
    Date = list_to_binary([integer_to_list(Y), "-", integer_to_list(M), "-", integer_to_list(D), " ", integer_to_list(H), ":", integer_to_list(I), ":", integer_to_list(S)]),
    io:format(F3, unicode:characters_to_list(Format), [Date] ++ A).
errlog(T, F, A, Mod, Line) ->
    {{Y, M, D}, {H, I, S}} = erlang:localtime(),
    case get("errlog") of
        undefined ->
            LogPath = config:get_log_path(),
            File1 = LogPath ++ "/errlog_" ++ integer_to_list(Y) ++ "_" ++ integer_to_list(M) ++ "_" ++ integer_to_list(D) ++ ".txt",
            {ok, Fl} = file:open(File1, [write, append]),
            put("errlog", Fl),
            F3 = Fl;
        F2 ->
            F3 = F2
    end,
    Format = list_to_binary("#" ++ T ++ " ~s[~w:~w] \r\n" ++ F ++ "\r\n~n"),
    Date = list_to_binary([integer_to_list(Y), "-", integer_to_list(M), "-", integer_to_list(D), " ", integer_to_list(H), ":", integer_to_list(I), ":", integer_to_list(S)]),
    io:format(F3, unicode:characters_to_list(Format), [Date, Mod, Line] ++ A).

%% 取得当前的unix时间戳（秒）
unixtime() ->
    {M, S, _} = mod_time:now(),
    M * 1000000 + S.

%% 取得某个时间点的unix时间戳
% LocalTime = {{Y,M,D},{H,M,S}} 
unixtime(LocalTime) ->
    [UniversalTime] = calendar:local_time_to_universal_time_dst(LocalTime),
    S1 = calendar:datetime_to_gregorian_seconds(UniversalTime),
    S2 = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
    S1 - S2.

%% 取得当天零点的unix时间戳
unixdate() ->
    Now = mod_time:now(),
    {_, Time} = calendar:now_to_local_time(Now),
    Ds = calendar:time_to_seconds(Time),
    {M, S, _} = Now,
    M * 1000000 + S - Ds.

unixdate(UnixTime) ->
    Now = unixtime_to_now(UnixTime),
    {_, Time} = calendar:now_to_local_time(Now),
    Ds = calendar:time_to_seconds(Time),
    {M, S, _} = Now,
    M * 1000000 + S - Ds.

unixtime_to_now(Time) ->
    M = Time div 1000000,
    S = Time rem 1000000,
    {M, S, 0}.

diff_day(UnixTime) ->
    {Date1, _} = calendar:now_to_local_time(unixtime_to_now(UnixTime)),
    {Date2, _} = calendar:local_time(),
    calendar:date_to_gregorian_days(Date2) - calendar:date_to_gregorian_days(Date1).

longunixtime() ->
    {M, S, Ms} = mod_time:now(),
    (M * 1000000000000 + S*1000000 + Ms) div 1000.

%% 转换成HEX格式的md5
md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).

%% 产生一个介于Min到Max之间的随机整数
rand(Same, Same) -> Same;
rand(Min, Max) when Max < Min -> 0;
rand(Min, Max) ->
    %% 以保证不同进程都可取得不同的种子
    case get("rand_seed") of
        undefined ->
            random:seed(erlang:now()),
            put("rand_seed", 1);
        _ -> skip
    end,
    M = Min - 1,
    if
        Max - M =< 0 ->
            0;
        true ->
            random:uniform(Max - M) + M
    end.

%%向上取整
ceil(N) ->
    T = trunc(N),
    case N =< T of
        true -> T;
        false -> 1 + T
    end.

%%向下取整
floor(X) ->
    T = trunc(X),
    case (X < T) of
        true -> T - 1;
        _ -> T
    end.

sleep(T) ->
    receive
    after T -> ok
    end.

sleep(T, F) ->
    receive
    after T -> F()
    end.

get_list([], _) ->
    [];
get_list(X, F) ->
    F(X).

%% for循环
for(Max, Max, F) ->
    F(Max);
for(I, Max, F) ->
    F(I),
    for(I + 1, Max, F).

%% 带返回状态的for循环
%% @return {ok, State}
for(Max, Min, _F, State) when Min < Max -> {ok, State};
for(Max, Max, F, State) -> F(Max, State);
for(I, Max, F, State) -> {ok, NewState} = F(I, State), for(I + 1, Max, F, NewState).

%% term序列化，term转换为string格式，e.g., [{a},1] => "[{a},1]"
term_to_string(Term) ->
    binary_to_list(list_to_binary(io_lib:format("~w", [Term]))).

%% term 保留字符串显示
term_to_string2(Term) ->
    binary_to_list(list_to_binary(io_lib:format("~p", [Term]))).

%% term序列化，term转换为bitstring格式，e.g., [{a},1] => <<"[{a},1]">>
term_to_bitstring(Term) ->
    erlang:list_to_bitstring(io_lib:format("~w", [Term])).

%% term反序列化，string转换为term，e.g., "[{a},1]"  => [{a},1]
string_to_term(String) ->
    case erl_scan:string(String ++ ".") of
        {ok, Tokens, _} ->
            case erl_parse:parse_term(Tokens) of
                {ok, Term} -> Term;
                _Err -> undefined
            end;
        _Error ->
            undefined
    end.

%% term反序列化，bitstring转换为term，e.g., <<"[{a},1]">>  => [{a},1]
bitstring_to_term(undefined) -> undefined;
bitstring_to_term(BitString) ->
    string_to_term(binary_to_list(BitString)).

filter_text_gm(Text) when is_bitstring(Text) ->
    Text;
filter_text_gm(Text) when is_list(Text) ->
    list_to_bitstring(Text).

%% 敏感词过滤
%% @param Text list() | bitstring()
%% @return bitstring() 过滤后的文本
filter_text(Text, Lv) when is_bitstring(Text) ->
    S = bitstring_to_list(Text),
    filter_text(S, Lv);
filter_text(Text, Lv) when is_list(Text) ->
    [Term] = io_lib:format("~ts", [Text]),
    mod_word:replace_sensitive_talk(Term, Lv).

%% 名字过滤
%% @param Text list() | bitstring()
%% @return bitstring() 过滤后的文本
filter_name(Text) when is_bitstring(Text) ->
    S = bitstring_to_list(Text),
    filter_name(S);
filter_name(Text) when is_list(Text) ->
    [Term] = io_lib:format("~ts", [Text]),
    mod_word:replace_sensitive_name(Term).

%% 敏感词检测
%% @return true 存在关键词
%%          false 不存在关键词
%% @var Text：字符串
check_keyword(Text) ->
    if
        is_list(Text) ->
            mod_word:word_is_sensitive_name(Text);
        true ->
            true
    end.

%% 长度合法性检查
check_length(Item, LenLimit) ->
    check_length(Item, 1, LenLimit).

check_length(Item, MinLen, MaxLen) ->
    case asn1rt:utf8_binary_to_list(list_to_binary(Item)) of
        {ok, UnicodeList} ->
            Len = string_width(UnicodeList),
            Len =< MaxLen andalso Len >= MinLen;
        {error, _Reason} ->
            false
    end.

%% 字符宽度，1汉字=2单位长度，1数字字母=1单位长度
string_width(String) ->
    string_width(String, 0).
string_width([], Len) ->
    Len;
string_width([H | T], Len) ->
    case H > 255 of
        true ->
            string_width(T, Len + 2);
        false ->
            string_width(T, Len + 1)
    end.

%% IP元组转字符
ip2bin({A, B, C, D}) ->
    [integer_to_list(A), ".", integer_to_list(B), ".", integer_to_list(C), ".", integer_to_list(D)].


%% 将列里的不同类型转行成字节型，如 [<<"字节">>, 123, asdasd, "asdasd"] 输出 <<"字节123asdasdasdasd">>
all_to_binary(List) -> all_to_binary(List, []).

all_to_binary([], Result) -> list_to_binary(Result);
all_to_binary([P | T], Result) when is_list(P) -> all_to_binary(T, lists:append(Result, P));
all_to_binary([P | T], Result) when is_integer(P) -> all_to_binary(T, lists:append(Result, integer_to_list(P)));
all_to_binary([P | T], Result) when is_binary(P) -> all_to_binary(T, lists:append(Result, binary_to_list(P)));
all_to_binary([P | T], Result) when is_float(P) -> all_to_binary(T, lists:append(Result, float_to_list(P)));
all_to_binary([P | T], Result) when is_atom(P) -> all_to_binary(T, lists:append(Result, atom_to_list(P))).

%今天是星期几
get_day_of_week() ->
    Seconds = util:unixtime(),
    {{Year, Month, Day}, _Time} = seconds_to_localtime(Seconds),
    calendar:day_of_the_week(Year, Month, Day).
get_day_of_week(Seconds) ->
    {{Year, Month, Day}, _Time} = seconds_to_localtime(Seconds),
    calendar:day_of_the_week(Year, Month, Day).

%% 从一个list中随机取出一项
%% null | term()
list_rand([]) -> null;
list_rand([I]) -> I;
list_rand(List) ->
    Len = length(List),
    Index = rand(1, Len),
    get_term_from_list(List, Index).
get_term_from_list(List, 1) ->
    [Term | _R] = List,
    Term;
get_term_from_list(List, Index) ->
    [_H | R] = List,
    get_term_from_list(R, Index - 1).

%% @doc convert other type to atom
to_atom(Msg) when is_atom(Msg) ->
    Msg;
to_atom(Msg) when is_binary(Msg) ->
    list_to_atom2(binary_to_list(Msg));
to_atom(Msg) when is_list(Msg) ->
    list_to_atom2(Msg);
to_atom(_) ->
    throw(other_value).

list_to_atom2(List) when is_list(List) ->
    case catch (list_to_existing_atom(List)) of
        {'EXIT', _} -> erlang:list_to_atom(List);
        Atom when is_atom(Atom) -> Atom
    end.

%%获取客户端ip
get_ip(Socket) ->
    case inet:peername(Socket) of
        {ok, {Ip, _Port}} -> Ip;
        {error, _Reason} -> {0, 0, 0, 0}
    end.

%%=========================================================================
%% 辅助函数
%%=========================================================================

%% -----------------------------------------------------------------
%% 确保字符串类型为二进制
%% -----------------------------------------------------------------
make_sure_binary(String) when is_binary(String) ->
    String;
make_sure_binary(String) when is_list(String) ->
    list_to_binary(String);
make_sure_binary(String) when is_atom(String) ->
    list_to_binary(atom_to_list(String));
make_sure_binary(String) ->
    errlog("make_sure_binary: Error string=[~p]", [String]),
    <<>>.


%% -----------------------------------------------------------------
%% 确保字符串类型为列表
%% -----------------------------------------------------------------
make_sure_list(String) ->
    case is_list(String) of
        true -> String;
        false when is_binary(String) -> binary_to_list(String);
        false ->
            errlog("make_sure_list: Error string=[~w]~n", [String]),
            String
    end.

%% -----------------------------------------------------------------
%% 过滤掉字符串中的特殊字符
%% -----------------------------------------------------------------
filter_string(String, CharList) ->
    case is_list(String) of
        true ->
            filter_string_helper(String, CharList, []);
        false when is_binary(String) ->
            ResultString = filter_string_helper(binary_to_list(String), CharList, []),
            list_to_binary(ResultString);
        false ->
            ?ERR("filter_string: Error string=[~w]", [String]),
            String
    end.

filter_string_helper([], _CharList, ResultString) ->
    ResultString;
filter_string_helper([H | T], CharList, ResultString) ->
    case lists:member(H, CharList) of
        true -> filter_string_helper(T, CharList, ResultString);
        false -> filter_string_helper(T, CharList, ResultString ++ [H])
    end.


%% 转换为list
object_to_list(Object) when is_binary(Object) ->
    binary_to_list(Object);
object_to_list(Object) when is_list(Object) ->
    Object;
object_to_list(_) ->
    [].

%% -----------------------------------------------------------------
%% 根据1970年以来的秒数获得日期
%% -----------------------------------------------------------------
seconds_to_localtime(Seconds) ->
    DateTime = calendar:gregorian_seconds_to_datetime(Seconds + ?DIFF_SECONDS_0000_1900),
    calendar:universal_time_to_local_time(DateTime).

%% -----------------------------------------------------------------
%% 根据日期获得1970年以来的秒数(测试用)
%% -----------------------------------------------------------------
%localtime_to_seconds({Data, Time}) ->
%    DateTime = calendar:local_time_to_universal_time({Data, Time}),
%    calendar:datetime_to_gregorian_seconds(DateTime)-?DIFF_SECONDS_0000_1900.

%% -----------------------------------------------------------------
%% 判断是否同一天
%% -----------------------------------------------------------------
is_same_date(Seconds1, Seconds2) ->
    {{Year1, Month1, Day1}, _Time1} = seconds_to_localtime(Seconds1),
    {{Year2, Month2, Day2}, _Time2} = seconds_to_localtime(Seconds2),
    if ((Year1 /= Year2) or (Month1 /= Month2) or (Day1 /= Day2)) -> false;
        true -> true
    end.

%% -----------------------------------------------------------------
%% 判断是否同一星期
%% -----------------------------------------------------------------
is_same_week(Seconds1, Seconds2) ->
    {{Year1, Month1, Day1}, Time1} = seconds_to_localtime(Seconds1),
    % 星期几
    Week1 = calendar:day_of_the_week(Year1, Month1, Day1),
    % 从午夜到现在的秒数
    Diff1 = calendar:time_to_seconds(Time1),
    Monday = Seconds1 - Diff1 - (Week1 - 1) * ?ONE_DAY_SECONDS,
    Sunday = Seconds1 + (?ONE_DAY_SECONDS - Diff1) + (7 - Week1) * ?ONE_DAY_SECONDS,
    if ((Seconds2 >= Monday) and (Seconds2 < Sunday)) -> true;
        true -> false
    end.

%% -----------------------------------------------------------------
%% 获取当天0点和第二天0点
%% -----------------------------------------------------------------
get_midnight_seconds() ->
    get_midnight_seconds(util:unixtime()).

get_midnight_seconds(Seconds) ->
    {{_Year, _Month, _Day}, Time} = seconds_to_localtime(Seconds),
    % 从午夜到现在的秒数
    Diff = calendar:time_to_seconds(Time),
    % 获取当天0点
    Today = Seconds - Diff,
    % 获取第二天0点
    NextDay = Seconds + (?ONE_DAY_SECONDS - Diff),
    {Today, NextDay}.

%% -----------------------------------------------------------------
%% 计算相差的天数
%% -----------------------------------------------------------------
get_diff_days(Seconds1, Seconds2) ->
    {{Year1, Month1, Day1}, _} = seconds_to_localtime(Seconds1),
    {{Year2, Month2, Day2}, _} = seconds_to_localtime(Seconds2),
    Days1 = calendar:date_to_gregorian_days(Year1, Month1, Day1),
    Days2 = calendar:date_to_gregorian_days(Year2, Month2, Day2),
    DiffDays = abs(Days2 - Days1),
    DiffDays + 1.

%% -----------------------------------------------------------------
%% 增加天数
%% -----------------------------------------------------------------
add_days(Date, Days) ->
    GreDays = calendar:date_to_gregorian_days(Date) + Days,
    calendar:gregorian_days_to_date(GreDays).

%% -----------------------------------------------------------------
%% 扩展的foreach函数
%% -----------------------------------------------------------------
foreach_ex(_Fun, [], _Arg) ->
    void;
foreach_ex(Fun, [H | T], Arg) ->
    Fun(H, Arg),
    foreach_ex(Fun, T, Arg).

%% -----------------------------------------------------------------
%% 扩展的map函数
%% -----------------------------------------------------------------
map_ex(_Fun, [], _Arg) ->
    [];
map_ex(Fun, [H | T], Arg) ->
    [Fun(H, Arg) | map_ex(Fun, T, Arg)].

%% -----------------------------------------------------------------
%% 根据lists的元素值获得下标
%% -----------------------------------------------------------------
get_list_elem_index(Elem, List) ->
    get_lists_elem_index_helper(List, Elem, 0).
get_lists_elem_index_helper([], _Elem, _Index) ->
    0;
get_lists_elem_index_helper([H | T], Elem, Index) ->
    if H =:= Elem ->
        Index + 1;
        true ->
            get_lists_elem_index_helper(T, Elem, Index + 1)
    end.

%% -----------------------------------------------------------------
%% 增加lists相应元素的值并获得新lists
%% -----------------------------------------------------------------
replace_list_elem(Index, NewElem, List) ->
    replace_list_elem_helper(List, Index, NewElem, 1, []).
replace_list_elem_helper([], _Index, _NewElem, _CurIndex, NewList) ->
    NewList;
replace_list_elem_helper([H | T], Index, NewElem, CurIndex, NewList) ->
    if Index =:= CurIndex ->
        replace_list_elem_helper(T, Index, NewElem, CurIndex + 1, NewList ++ [NewElem]);
        true ->
            replace_list_elem_helper(T, Index, NewElem, CurIndex + 1, NewList ++ [H])
    end.

%% -----------------------------------------------------------------
%% 获取当天0点
%% -----------------------------------------------------------------
get_today_midnight() ->
    NowTime = util:unixtime(),
    {{_Year, _Month, _Day}, Time} = seconds_to_localtime(NowTime),
    % 从午夜到现在的秒数
    Diff = calendar:time_to_seconds(Time),
    % 获取当天0点
    NowTime - Diff.

get_today_midnight(Seconds) ->
    {{_Year, _Month, _Day}, Time} = seconds_to_localtime(Seconds),
    % 从午夜到现在的秒数
    Diff = calendar:time_to_seconds(Time),
    % 获取当天0点
    Seconds - Diff.

%% -----------------------------------------------------------------
%% 获取当天0点到现在的秒数
%% -----------------------------------------------------------------
get_seconds_from_midnight() ->
    NowTime = util:unixtime(),
    {{_Year, _Month, _Day}, Time} = seconds_to_localtime(NowTime),
    calendar:time_to_seconds(Time).

get_seconds_from_midnight(Seconds) ->
    {{_Year, _Month, _Day}, Time} = seconds_to_localtime(Seconds),
    calendar:time_to_seconds(Time).

%% 判断是否是中心节点 -> true (是跨服节点)| false (不是是跨服节点)
check_kfz() ->
    Kfz = case ets:lookup(?SERVER_STATUS, kfz) of
              [] -> 0;
              [S] -> S#server_status.value
          end,
    Kfz =:= 1.

%% 判断开服天数是否在多少天内
check_open_day(Day) ->
    Now_time = unixtime(),
    check_open_day(Day, Now_time).

check_open_day(Day, UnixTime) ->
    Open_time = get_open_time(),
    ((Open_time + 86400 * Day) > UnixTime).

%% 获取开服时间
get_open_time() ->
    case ets:lookup(?SERVER_STATUS, open_time) of
        [] -> 0;
        [S] -> S#server_status.value
    end.

%% 获取开服天数
get_open_day() ->
    Now = unixtime(),
    OpenTime = get_open_time(),
    Day = (Now - OpenTime) div 86400,
    Day + 1.

to_term(BinString) ->
    case util:bitstring_to_term(BinString) of
        undefined -> [];
        Term -> Term
    end.

boolean_to_integer(Boolean) ->
    case Boolean of
        true -> 1;
        false -> 0
    end.

integer_to_boolean(Integer) ->
    case Integer of
        0 -> false;
        _ -> true
    end.

%% 时间戳（秒级）转为易辨别时间字符串
%% 例如： 1291014369 -> "2010年11月29日15时6分"
unixtime_to_time_string(Timestamp) ->
    {{Year, Month, Day}, {Hour, Minute, _Second}} = calendar:now_to_local_time({Timestamp div 1000000, Timestamp rem 1000000, 0}),
    lists:concat([Year, "年", Month, "月", Day, "日", Hour, "时", Minute, "分"]).

%% 查询一周时间范围
get_week_time() ->
    Timestamp = util:unixtime(),
    get_week_time(Timestamp).

get_week_time(Timestamp) ->
    {MegaSecs, Secs, MicroSecs} = util:unixtime_to_now(Timestamp),
    {Date, Time} = calendar:now_to_local_time({MegaSecs, Secs, MicroSecs}),
    TodaySecs = calendar:time_to_seconds(Time),
    WeekDay = calendar:day_of_the_week(Date),
    Monday = Timestamp - ?ONE_DAY_SECONDS * (WeekDay - 1) - TodaySecs,
    NextMonday = Monday + 7 * ?ONE_DAY_SECONDS,
    {Monday, NextMonday}.

%% 查找ListOfList中某个字段值为Key的结果
%% @spec keyfind(Key, N, ListList) -> false | Value
keyfind(_, _, []) ->
    false;
keyfind(Key, N, List) ->
    [List1 | NewList] = List,
    case lists:nth(N, List1) of
        Key ->
            List1;
        _ ->
            keyfind(Key, N, NewList)
    end.

get_last_day_string(Date) ->
    {LastDate, _} = get_last_day_local_time({Date, {0, 0, 0}}),
    get_date_string(LastDate).

get_last_day_local_time(LocalTime) ->
    NowTime = util:unixtime(LocalTime),
    LastDayNowTime = NowTime - ?ONE_DAY_SECONDS,
    LastDayNow = util:unixtime_to_now(LastDayNowTime),
    calendar:now_to_local_time(LastDayNow).

get_date_string(Date) ->
    {Y, M, D} = Date,
    io_lib:format("~p年~p月~p日", [Y, M, D]).

%% 随机打乱list元素顺序
list_shuffle(L) ->
    Len = length(L),
    List1 = [{rand(1, Len + 10000), X} || X <- L],
    List2 = lists:sort(List1),
    [E || {_, E} <- List2].

%%计数器
counter(Type) ->
    counter(Type, 1).
counter(Type, Inc) ->
    ets:update_counter(ets_counter, Type, Inc).

%% record 转 list 
%% @return 一个LIST 成员同 Record一样
record_to_list(Record) when erlang:is_tuple(Record) ->
    RecordList = erlang:tuple_to_list(Record),
    [_ | ListLeft] = RecordList,
    ListLeft;
record_to_list(_Record) ->
    [].

%% list 转 record 
%% @param List 必须长度跟rocrod一样
%% @param RecordAtom record的名字,是一个原子
%% @return RecordAtom类型的RECORD
list_to_record(List, RecordAtom) ->
    RecordList = [RecordAtom | List],
    erlang:list_to_tuple(RecordList).

%% 字符加密
check_char_encrypt(Id, Time, TK) ->
    TICKET = "7YnELt8MmA4jVED7",
    Hex = util:md5(lists:concat([Time, Id, TICKET])),
    %NowTime = util:unixtime(),
    %Hex =:= TK andalso NowTime - Time >= -10 andalso NowTime - Time < 300.
    Hex =:= TK.

%% 获取运营服ID
get_server_id() ->
    Server = case application:get_env(gs, card_server) of
                 {ok, [_Ser]} -> _Ser;
                 _ -> ""
             end,
    %{ok, [Server]} = application:get_env(card_server),
    Len = string:len(Server),
    %io:format("Server:~p, Len:~p~n", [Server, Len]),
    case Len > 1 of
        true ->
            ServerId = string:sub_string(Server, 2, Len),
            %io:format("ServerId:~p, New:~p~n", [ServerId, string:to_integer(ServerId)]),
            case string:to_integer(ServerId) of
                {Id, _} -> Id;
                _ -> 0
            end;
        false ->
            0
    end.

%% 扩展版lists:min/1
%% @param: (List, N), List为元组列表，N为元组中第N个元素
min_ex([H | T], N) -> min_ex(T, H, N).

min_ex([H | T], Min, N) when element(N, H) < element(N, Min) -> min_ex(T, H, N);
min_ex([_ | T], Min, N) -> min_ex(T, Min, N);
min_ex([], Min, _) -> Min.


%% 扩展版lists:max/1
%% @param: (List, N), List为元组列表，N为元组中第N个元素
max_ex([H | T], N) -> max_ex(T, H, N).

max_ex([H | T], Max, N) when element(N, H) > element(N, Max) -> max_ex(T, H, N);
max_ex([_ | T], Max, N) -> max_ex(T, Max, N);
max_ex([], Max, _) -> Max.

%% 列表中的元素是否全部相同
%% @param: (List, N), List为元组列表，N为元组中第N个元素
is_all_same([H | T], N) -> is_all_same(T, H, N).

is_all_same([H | T], Min, N) when element(N, H) =:= element(N, Min) -> is_all_same(T, H, N);
is_all_same(L, _, _) when L =/= [] -> false;
is_all_same([], _, _) -> true. 

%% @doc 格式化
format(Temp, Content) ->
    lists:flatten(io_lib:format(Temp, Content)).

%% @doc 获取数值所在区间对应的值
get_config_zone(R, ZoneList) ->
    get_config_zone(R, ZoneList, undefined).

get_config_zone(_R, [], Default) ->
    Default;
get_config_zone(R, [{{Low, High}, V} | Rest], Default) ->
    case R >= Low andalso (R =< High orelse High =:= -1) of
        true ->
            V;
        _ ->
            get_config_zone(R, Rest, Default)
    end.

%% List转为Text封装函数
list_to_text(List) ->
    List2 = case is_list(List) of
                false ->
                    [];
                true ->
                    List
            end,
    util:term_to_bitstring(List2).

