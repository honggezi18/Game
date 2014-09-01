-ifndef(PT_PLAYER_PB_H).
-define(PT_PLAYER_PB_H, true).
-record(pt_player, {
    id = erlang:error({required, id})
}).
-endif.

-ifndef(PT_MON_PB_H).
-define(PT_MON_PB_H, true).
-record(pt_mon, {
    id = erlang:error({required, id})
}).
-endif.

-ifndef(PT12001_TOS_PB_H).
-define(PT12001_TOS_PB_H, true).
-record(pt12001_tos, {
    x = erlang:error({required, x}),
    y = erlang:error({required, y}),
    fly = erlang:error({required, fly}),
    d1 = erlang:error({required, d1}),
    d2 = erlang:error({required, d2}),
    skill = erlang:error({required, skill}),
    scene = erlang:error({required, scene})
}).
-endif.

-ifndef(PT12001_TOC_PB_H).
-define(PT12001_TOC_PB_H, true).
-record(pt12001_toc, {
    x = erlang:error({required, x}),
    y = erlang:error({required, y}),
    id = erlang:error({required, id}),
    platform = erlang:error({required, platform}),
    server_id = erlang:error({required, server_id}),
    fly = erlang:error({required, fly}),
    d1 = erlang:error({required, d1}),
    d2 = erlang:error({required, d2}),
    skill = erlang:error({required, skill})
}).
-endif.

-ifndef(PT12002_TOS_PB_H).
-define(PT12002_TOS_PB_H, true).
-record(pt12002_tos, {
    
}).
-endif.

-ifndef(PT12002_TOC_PB_H).
-define(PT12002_TOC_PB_H, true).
-record(pt12002_toc, {
    players = [],
    mons = []
}).
-endif.

-ifndef(PT12003_TOC_PB_H).
-define(PT12003_TOC_PB_H, true).
-record(pt12003_toc, {
    player = erlang:error({required, player})
}).
-endif.

-ifndef(PT12004_TOC_PB_H).
-define(PT12004_TOC_PB_H, true).
-record(pt12004_toc, {
    id = erlang:error({required, id}),
    platform = erlang:error({required, platform}),
    server_id = erlang:error({required, server_id})
}).
-endif.

-ifndef(PT12005_TOS_PB_H).
-define(PT12005_TOS_PB_H, true).
-record(pt12005_tos, {
    scene_id = erlang:error({required, scene_id})
}).
-endif.

-ifndef(PT12005_TOC_PB_H).
-define(PT12005_TOC_PB_H, true).
-record(pt12005_toc, {
    id = erlang:error({required, id}),
    x = erlang:error({required, x}),
    y = erlang:error({required, y}),
    name = erlang:error({required, name}),
    scene_id = erlang:error({required, scene_id})
}).
-endif.

-ifndef(PT12006_TOC_PB_H).
-define(PT12006_TOC_PB_H, true).
-record(pt12006_toc, {
    id = erlang:error({required, id})
}).
-endif.

-ifndef(PT12007_TOC_PB_H).
-define(PT12007_TOC_PB_H, true).
-record(pt12007_toc, {
    mon = erlang:error({required, mon})
}).
-endif.

-ifndef(PT12008_TOC_PB_H).
-define(PT12008_TOC_PB_H, true).
-record(pt12008_toc, {
    id = erlang:error({required, id}),
    x = erlang:error({required, x}),
    y = erlang:error({required, y}),
    pix = erlang:error({required, pix}),
    piy = erlang:error({required, piy})
}).
-endif.

-ifndef(PT12009_TOC_PB_H).
-define(PT12009_TOC_PB_H, true).
-record(pt12009_toc, {
    id = erlang:error({required, id}),
    platform = erlang:error({required, platform}),
    server_id = erlang:error({required, server_id}),
    hp = erlang:error({required, hp}),
    hp_lim = erlang:error({required, hp_lim})
}).
-endif.

