%%%------------------------------------------------
%%% File    : record.hrl
%%% Author  : xyao
%%% Created : 2011-06-13
%%% Description: 这里只存在公共常用的record
%%%------------------------------------------------

%% 所有线路记录
-record(node, {
        id,
        ip,
        port,
        node,
        cookie,
        num = 0,
        state = 0  %是否开放 0开放 1关闭
    }).

%% 服务器信息
-record(server_status, {
        name,
        value
    }).
