%%%------------------------------------
%%% @Module  : mod_server_cast
%%% @Author  : xyao
%%% @Email   : jiexiaowen@gmail.com
%%% @Created : 2011.12.16
%%% @Description: 角色cast处理
%%%------------------------------------
-module(mod_server_cast).
-export([handle_cast/2, set_data/2]).
-include("server.hrl").
-include("common.hrl").
-include("sql_player_base.hrl").

%% 设置玩家属性(按属性列表更新)远程调用
%% @spec set_data(AttrKeyValueList,Pid) -> noprc | ok
%% AttrKeyValueList 属性列表 [{Key,Value},{Key,Value},...] Key为原子类型，Value为所需参数数据
%% Pid 玩家进程
%% @end
set_data(AttrKeyValueList, Pid) ->
    case is_pid(Pid) andalso misc:is_process_alive(Pid) of
        true -> case catch gen_server:cast(Pid, {'set_data', AttrKeyValueList}) of
                    {'EXIT', _R} ->
                        util:errlog("ERROR mod_server_cast:set_data/2, Arg:[~p, ~p], Reason = ~p~n", [AttrKeyValueList, Pid, _R]);
                    _ -> ok
                end;
        false -> noprc
    end.

%% 设置玩家属性(按属性列表更新)
%% @param AttrKeyValueList 属性列表 [{Key,Value},{Key,Value},...] Key为原子类型，Value为所需参数数据
%% @param Status 当前玩家状态
%% @return NewStatus 新玩家状态
set_data_sub(AttrKeyValueList, Status) ->
    case AttrKeyValueList of
        [] -> Status;
        _ ->
            [{Key, _Value} | T] = AttrKeyValueList,
            NewStatus = case Key of
                _ ->
                    Status
            end,
            set_data_sub(T,NewStatus)
    end.

%%==========基础功能base============
%%写入用户信息
handle_cast({'base_set_data', PlayerStatus}, _Status) ->
    {noreply, PlayerStatus};

%% 分类设置/更新玩家信息|(因为各属性更改规则不一致，故需要特殊写规则，请维护上面set_data_sub()函数)
%% @param Type 更新数据的类型_根据此类型来对Info解包
%% @param Info 更新所包含的数据
handle_cast({'set_data', AttrKeyValueList}, Status) ->
    %% 根据类型_调用各个功能自己的组合函数来更新玩家信息，请维护set_data_sub()函数。
    NewPlayerStatus = set_data_sub(AttrKeyValueList, Status),
    {noreply, NewPlayerStatus};

%% 默认匹配
handle_cast(Event, Status) ->
    catch util:errlog("mod_server_cast:handle_cast not match: ~p~n", [Event]),
    {noreply, Status}.
