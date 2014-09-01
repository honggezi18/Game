%%%-----------------------------------
%%% @Module  : lib_clusters
%%% @Author  : xyao
%%% @Email   : jiexiaowen@gmail.com
%%% @Created : 2012.10.29
%%% @Description: 跨服共用方法
%%%-----------------------------------
-module(lib_clusters).
-export([
        add_node/1,
        del_node/1
	 ]).
-include("clusters.hrl").

%% 添加节点
add_node(Node) ->
    catch db:execute(io_lib:format(<<"replace into `clusters` (node) values('~s')">>,[Node])).

%% 删除节点
del_node(Node) ->
    catch db:execute(io_lib:format(<<"delete from `clusters` where `node` = '~s'">>,[Node])).

