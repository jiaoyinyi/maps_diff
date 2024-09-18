%%%-------------------------------------------------------------------
%%% @author jiaoyinyi
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% maps比较简单测试
%%% @end
%%% Created : 2024-06-24 00:00:00
%%%-------------------------------------------------------------------
-module(maps_diff_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-compile(export_all).

all() ->
    [
        diff_test
        , merge_test
    ].

diff_test(Config) ->
    Map1 = #{<<"a">> => 10, <<"b">> => [5, 10, 20, 30], <<"c">> => #{<<"msg">> => <<"hello">>, <<"del">> => 10}, <<"del">> => 10},
    Map2 = #{<<"a">> => 10, <<"b">> => [10, 20, 30, 40, 50], <<"c">> => #{<<"msg">> => <<"hello234">>, <<"new">> => 10}, <<"new">> => 10},
    Diff = #{<<"b">> =>
    #{0 => {maps_diff, update, 5, 10},
        1 => {maps_diff, update, 10, 20},
        2 => {maps_diff, update, 20, 30},
        3 => {maps_diff, update, 30, 40},
        4 => {maps_diff, add, undefined, 50}},
        <<"c">> =>
        #{<<"del">> => {maps_diff, delete, 10, undefined},
            <<"msg">> => {maps_diff, update, <<"hello">>, <<"hello234">>},
            <<"new">> => {maps_diff, add, undefined, 10}},
        <<"del">> => {maps_diff, delete, 10, undefined},
        <<"new">> => {maps_diff, add, undefined, 10}},
    Diff = maps_diff:diff(Map1, Map2),
    Config.

merge_test(Config) ->
    Map1 = #{<<"a">> => 10, <<"b">> => [5, 10], <<"del">> => 10},
    Map2 = #{<<"a">> => 10, <<"b">> => [10, 20], <<"c">> => #{<<"msg">> => <<"hello">>, <<"data">> => 10}, <<"new">> => 10},
    Map3 = #{<<"a">> => 10, <<"b">> => [10, 20], <<"c">> => #{<<"msg">> => <<"hello">>}},
    Map4 = #{<<"a">> => 10, <<"b">> => [10, 20], <<"c">> => #{<<"msg">> => <<"hello">>, <<"data">> => #{<<"int">> => 1}}},
    Map5 = #{<<"a">> => 10, <<"b">> => [10, 20], <<"c">> => #{<<"msg">> => <<"hello">>, <<"data">> => #{<<"int">> => 2, <<"float">> => 1.2}}},
    Diff1 = maps_diff:diff(Map1, Map2),
    Diff2 = maps_diff:diff(Map2, Map3),
    Diff3 = maps_diff:diff(Map3, Map4),
    Diff4 = maps_diff:diff(Map4, Map5),
    Merge = #{<<"b">> =>
    #{0 => {maps_diff, update, 5, 10},
        1 => {maps_diff, update, 10, 20}},
        <<"c">> =>
        {maps_diff, add, undefined,
            #{<<"data">> => #{<<"float">> => 1.2, <<"int">> => 2},
                <<"msg">> => <<"hello">>}},
        <<"del">> => {maps_diff, delete, 10, undefined}},
    Merge = maps_diff:merge([Diff1, Diff2, Diff3, Diff4]),
    Config.

%% 1> Diff1 = {maps_diff,add,undefined,#{hello => #{test => []}}}.
%%{maps_diff,add,undefined,#{hello => #{test => []}}}
%%2> Diff2 = #{hello =>
%%2>       #{test =>
%%2>             #{0 => {maps_diff,add,undefined,#{sdf => 123}},
%%2>               1 => {maps_diff,add,undefined,#{sdf => 123}}}}}.
%%#{hello =>
%%      #{test =>
%%            #{0 => {maps_diff,add,undefined,#{sdf => 123}},
%%              1 => {maps_diff,add,undefined,#{sdf => 123}}}}}
%%3> 
%%3>
%%3> maps_diff:merge(Diff1,Diff2).