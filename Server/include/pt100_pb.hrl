-ifndef(PT10000_TOS_PB_H).
-define(PT10000_TOS_PB_H, true).
-record(pt10000_tos, {
    acc_id = erlang:error({required, acc_id}),
    accname = erlang:error({required, accname}),
    time = erlang:error({required, time}),
    ticket = erlang:error({required, ticket})
}).
-endif.

-ifndef(PT10000_TOC_PB_H).
-define(PT10000_TOC_PB_H, true).
-record(pt10000_toc, {
    code = erlang:error({required, code}),
    num = erlang:error({required, num}),
    career = erlang:error({required, career}),
    time = erlang:error({required, time})
}).
-endif.

-ifndef(PT10002_TOS_PB_H).
-define(PT10002_TOS_PB_H, true).
-record(pt10002_tos, {
    
}).
-endif.

-ifndef(PT10002_TOC_PB_H).
-define(PT10002_TOC_PB_H, true).
-record(pt10002_toc, {
    players = []
}).
-endif.

-ifndef(PT10003_TOS_PB_H).
-define(PT10003_TOS_PB_H, true).
-record(pt10003_tos, {
    realm = erlang:error({required, realm}),
    career = erlang:error({required, career}),
    sex = erlang:error({required, sex}),
    name = erlang:error({required, name})
}).
-endif.

-ifndef(PT10003_TOC_PB_H).
-define(PT10003_TOC_PB_H, true).
-record(pt10003_toc, {
    code = erlang:error({required, code}),
    id = erlang:error({required, id})
}).
-endif.

-ifndef(PT10004_TOS_PB_H).
-define(PT10004_TOS_PB_H, true).
-record(pt10004_tos, {
    id = erlang:error({required, id}),
    time = erlang:error({required, time}),
    ticket = erlang:error({required, ticket})
}).
-endif.

-ifndef(PT10004_TOC_PB_H).
-define(PT10004_TOC_PB_H, true).
-record(pt10004_toc, {
    code = erlang:error({required, code})
}).
-endif.

-ifndef(PT10005_TOS_PB_H).
-define(PT10005_TOS_PB_H, true).
-record(pt10005_tos, {
    id = erlang:error({required, id})
}).
-endif.

-ifndef(PT10005_TOC_PB_H).
-define(PT10005_TOC_PB_H, true).
-record(pt10005_toc, {
    code = erlang:error({required, code})
}).
-endif.

-ifndef(PT10006_TOS_PB_H).
-define(PT10006_TOS_PB_H, true).
-record(pt10006_tos, {
    
}).
-endif.

-ifndef(PT10006_TOC_PB_H).
-define(PT10006_TOC_PB_H, true).
-record(pt10006_toc, {
    
}).
-endif.

-ifndef(PT10010_TOS_PB_H).
-define(PT10010_TOS_PB_H, true).
-record(pt10010_tos, {
    name = erlang:error({required, name})
}).
-endif.

-ifndef(PT10010_TOC_PB_H).
-define(PT10010_TOC_PB_H, true).
-record(pt10010_toc, {
    code = erlang:error({required, code})
}).
-endif.

-ifndef(PT10002_TOC_PLAYER_PB_H).
-define(PT10002_TOC_PLAYER_PB_H, true).
-record(pt10002_toc_player, {
    id = erlang:error({required, id}),
    status = erlang:error({required, status}),
    career = erlang:error({required, career}),
    sex = erlang:error({required, sex}),
    lv = erlang:error({required, lv}),
    name = erlang:error({required, name}),
    realm = erlang:error({required, realm})
}).
-endif.

