%%%-------------------------------------------------------------------
%%% @author Ryuu
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%     协议辅助生成
%%% @end
%%% Created : 28. 八月 2014 14:41
%%%-------------------------------------------------------------------
-module(proto_dyn).
-author("Ryuu").

%% API
-export([generate_all/0]).

-define(IF(Flag, T, F), case Flag of true -> T; _ -> F end).

generate_all() ->
    parse_proto().

%% @doc 解析.proto文件
parse_proto() ->
    ProtoDir = "./proto",
    FileList = filelib:wildcard("*.proto", ProtoDir),
    Options = [{output_src_dir, "./src/pt"}, {output_include_dir, "./include"}, {output_ebin_dir, "./ebin"}],
    [begin
         Path = filename:join(["./proto", FileName]),
         protobuffs_compile:scan_file(Path, Options),
         protobuffs_compile:generate_source(Path, Options)
     end || FileName <- FileList].

