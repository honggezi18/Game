%%%-------------------------------------------------------------------
%%% @author JoyHuang
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%     公共线数据管理
%%% @end
%%% Created : 15. 八月 2014 16:09
%%%-------------------------------------------------------------------
-module(lib_unite_init).

%% API
-export([save_online/1,
         off_line/1
        ]).

-include("server.hrl").
-include("unite.hrl").

save_online(Status) ->
    UniteStatus = #ets_unite{
        id = Status#player_status.id,
        name = Status#player_status.nickname,
        gm = Status#player_status.gm,
        talk_lim = Status#player_status.talk_lim,
        talk_lim_time = Status#player_status.talk_lim_time,
        scene = Status#player_status.scene,
        copy_id = Status#player_status.copy_id,
        sex = Status#player_status.sex,
        realm = Status#player_status.realm,
        career = Status#player_status.career,
        lv = Status#player_status.lv,
        image = Status#player_status.image,
        sid = Status#player_status.sid,
        socket = Status#player_status.socket,
        last_login_time = Status#player_status.last_login_time,
        pid = Status#player_status.pid
    },
    mod_unite_agent:insert(UniteStatus),
    ok.

off_line(Status) ->
    mod_unite_agent:delete(Status#player_status.id),
    ok.

