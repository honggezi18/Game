%%%-----------------------------------
%%% @Module  : pt_120
%%% @Author  : xyao
%%% @Email   : jiexiaowen@gmail.com
%%% @Created : 2011.06.23
%%% @Description: 12场景信息
%%%-----------------------------------
-module(pt_120).
-export([read/2, write/2, pack_elem_list/1, pack_npc_list/1]).
-include("server.hrl").
-include("scene.hrl").

%%
%%客户端 -> 服务端 ----------------------------
%%

%%走路
read(12001, <<X:16, Y:16, Fly:8, D1:8, D2:8, Skill:32, Scene:32>>) ->
    {ok, [X, Y, Fly, D1, D2, Skill, Scene]};

%%加载场景
read(12002, _) ->
    {ok, load};

%%切换场景
read(12005, <<Sid:32>>) ->
    {ok, Sid};

read(_Cmd, _R) ->
    {error, no_match}.

%%
%%服务端 -> 客户端 ------------------------------------
%%

%%走路
write(12001, [X, Y, F, D1, D2, Id, Platform, SerNum, Skill]) ->
    Platform1 = pt:write_string(Platform),
    {ok, pt:pack(12001, <<X:16, Y:16, Id:32, Platform1/binary, SerNum:16, F:8, D1:8, D2:8, Skill:32>>)};

%%加场景信息
write(12002, {User, Mon, Mech, Elem, Npc}) ->
    Data1 = pack_elem_list(Elem),
    Data2 = pack_scene_user_list(User),
    Data3 = pack_mon_list(Mon),
    Data4 = pack_npc_list(Npc),
    Data5 = pt_420:pack_mech_list(Mech),

    Data = << Data1/binary, Data2/binary, Data3/binary, Data4/binary, Data5/binary>>,
    {ok, pt:pack(12002, Data, 1)};

	
%%进入新场景广播给本场景的人
write(12003, []) ->
    {ok, pt:pack(12003, <<>>)};
write(12003, D) ->
{ok, pt:pack(12003, binary_12003(trans_to_12003(D)))};

%%离开场景
write(12004, [Id, Platform, SerNum]) ->
    Platform1 = pt:write_string(Platform),
    {ok, pt:pack(12004, <<Id:32, Platform1/binary, SerNum:16>>)};

%%切换场景
write(12005, [Id, X, Y, Name, Sid]) ->
    Name1 = pt:write_string(Name),
    Data = <<Id:32, X:16, Y:16, Name1/binary, Sid:32>>,
    {ok, pt:pack(12005, Data)};

write(12006, [Id]) ->
    Data = <<Id:32>>,
    {ok, pt:pack(12006, Data)};

%% 有怪物进入场景
write(12007, Info) ->
    {ok, pt:pack(12007, binary_12007(Info))};

%%怪物移动
write(12008, [X, Y, Id, Pix, Piy]) ->
    Data = <<Id:32, X:16, Y:16, Pix:32, Piy:32>>,
    {ok, pt:pack(12008, Data)};

%% 血量变化
write(12009, [PlayerId, Platform, SerNum, Hp, Hp_lim]) ->
    Platform1 = pt:write_string(Platform),
	{ok, pt:pack(12009, <<PlayerId:32, Platform1/binary, SerNum:16, Hp:32, Hp_lim:32>>)};

%%加场景信息
write(12011, [User1, User2]) ->
    Data1 = pack_scene_user_list(User1),
    Data2 = pack_leave_list(User2),
    Data = << Data1/binary, Data2/binary>>,
    {ok, pt:pack(12011, Data, 1)};

write(_Cmd, _R) ->
    {ok, pt:pack(0, <<>>)}.

%% =====私有函数=======

%% 打包元素列表
pack_elem_list([]) ->
    <<0:16, <<>>/binary>>;
pack_elem_list(Elem) ->
    Rlen = length(Elem),
    F = fun([Sid, Name, X, Y]) ->
        Name1 = pt:write_string(Name),
        <<Sid:32, Name1/binary, X:16, Y:16>>
    end,
    RB = list_to_binary([F(D) || D <- Elem]),
    <<Rlen:16, RB/binary>>.

%% 打包怪物列表
pack_mon_list([]) ->
    <<0:16, <<>>/binary>>;
pack_mon_list(Mon) ->
    F = fun(D) ->
        binary_12007(D)
    end,
    MonBin = [F(D) || D <- Mon, D#ets_mon.hp > 0],
    Rlen = length(MonBin),
    RB = list_to_binary(MonBin),
    <<Rlen:16, RB/binary>>.

%% 打包NPC列表
pack_npc_list([]) ->
    <<0:16, <<>>/binary>>;
pack_npc_list(Npc) ->
    Rlen = length(Npc),
    F = fun(EtsNpc) ->
        Id = EtsNpc#ets_npc.id, 
        Name = pt:write_string(EtsNpc#ets_npc.name), 
        X = EtsNpc#ets_npc.x, 
        Y = EtsNpc#ets_npc.y, 
        Icon = EtsNpc#ets_npc.icon, 
        Image = EtsNpc#ets_npc.image, 
        Func = EtsNpc#ets_npc.func, 
        Realm = EtsNpc#ets_npc.realm,
        <<Id:32, Id:32, Name/binary, X:16, Y:16, Icon:32, Func:8, Realm:8, Image:32>>
    end,
    RB = list_to_binary([F(D) || D <- Npc]),
    <<Rlen:16, RB/binary>>.

pack_scene_user_list([]) ->
    <<0:16, <<>>/binary>>;
pack_scene_user_list(User) ->
    UserBin = pack_scene_user_list_helper(User, []),
    Rlen = length(UserBin),
    RB = list_to_binary(UserBin),
    <<Rlen:16, RB/binary>>.

pack_scene_user_list_helper([], List) ->
    List;
pack_scene_user_list_helper([D | T], List) ->
    case trans_to_12003(D) of
        [] ->
            pack_scene_user_list_helper(T, List);
        D1 ->
            pack_scene_user_list_helper(T, [binary_12003(D1) | List])
    end.

%% 打包玩家离开列表
pack_leave_list([]) ->
    <<0:16, <<>>/binary>>;
pack_leave_list(User) ->
    Rlen = length(User),
    F = fun([Id, Platform, SerNum]) ->
            Platform1 = pt:write_string(Platform),
            <<Id:32, Platform1/binary, SerNum:16>> 
    end,
    RB = list_to_binary([F(D) || D <- User]),
    <<Rlen:16, RB/binary>>.

binary_12003([]) ->
    <<>>.

binary_12007(S) ->
    <<>>.

%% 人物
trans_to_12003(D) when is_record(D, ets_scene_user)->
    [];

trans_to_12003(Status) when is_record(Status, player_status)->
    [];

trans_to_12003(_Status) ->
    [].