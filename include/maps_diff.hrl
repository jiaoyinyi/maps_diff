%%%-------------------------------------------------------------------
%%% @author jiaoyinyi
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% maps比较头文件
%%% @end
%%% Created : 2024-06-24 00:00:00
%%%-------------------------------------------------------------------

-ifndef(MAPS_DIFF_HER).
-define(MAPS_DIFF_HER, 1).

%% maps对比信息
-record(maps_diff, {
    op :: add | delete | update
    , old
    , new
}).

-endif.