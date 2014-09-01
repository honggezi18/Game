%%%-------------------------------------------------------------------
%%% @author Ryuu
%%% @copyright (C) 2014, <PLANT>
%%% @doc
%%%     动态配置生成头文件
%%% @end
%%% Created : 16. 八月 2014 15:43
%%%-------------------------------------------------------------------
-author("Ryuu").

-define(BASE_PATH, "settings/").
-define(OUTPUT_PATH, "ebin/").

-define(FOREACH(Fun, List), lists:foreach(fun(E) -> Fun(E) end, List)).

%% @doc 配置映射表，格式
-define(CONFIG_MAP, [
    {mora, "mora.config", kv_con}
]).

%% @doc 格式{配置名, 配置路径, 格式[kv_con, kv_list, rec_con, rec_list]}
-record(conf_map, {name, path, type = kv_con}).




