%%%-------------------------------------------------------------------
%%% @author jiaoyinyi
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% maps差异转mongodb更新语句
%%% @end
%%% Created : 2024-06-24 00:00:00
%%%-------------------------------------------------------------------
-module(maps_mongo).
-export([
    translate/1
]).

-include("maps_diff.hrl").

-define(IS_MAP_KEY(Key), (is_binary(Key) orelse is_atom(Key))).
-define(IS_ARRAY_KEY(Key), (is_integer(Key) andalso Key >= 0)).

-spec translate(map()) -> map().
translate(DiffMap) ->
    translate(DiffMap, #{}).
translate(DiffMap, UpdateMap) ->
    maps:fold(
        fun(Key, Val, Acc) when ?IS_MAP_KEY(Key) ->
            do_translate(to_binary(Key), Val, Acc)
        end, UpdateMap, DiffMap
    ).

do_translate(Key, #maps_diff{op = add, new = New}, UpdateMap) ->
    set_update_map(<<"$set">>, Key, New, UpdateMap);
do_translate(Key, #maps_diff{op = update, new = New}, UpdateMap) ->
    set_update_map(<<"$set">>, Key, New, UpdateMap);
do_translate(Key, #maps_diff{op = delete}, UpdateMap) ->
    set_update_map(<<"$unset">>, Key, <<>>, UpdateMap);
do_translate(Key, ValMap, UpdateMap) when is_map(ValMap) andalso (?IS_MAP_KEY(Key) orelse ?IS_ARRAY_KEY(Key)) -> %% 内嵌子项处理
    maps:fold(
        fun(SubKey, SubVal, Acc) ->
            NewKey = key_join(Key, SubKey),
            do_translate(NewKey, SubVal, Acc)
        end, UpdateMap, ValMap
    ).

set_update_map(Label, Key, Val, UpdateMap) ->
    LabelMap = maps:get(Label, UpdateMap, #{}),
    NewLabelMap = maps:put(Key, Val, LabelMap),
    maps:put(Label, NewLabelMap, UpdateMap).

key_join(Key, SubKey) ->
    <<(to_binary(Key))/binary, ".", (to_binary(SubKey))/binary>>.

to_binary(Bin) when is_binary(Bin) ->
    Bin;
to_binary(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8);
to_binary(Int) when is_integer(Int) ->
    integer_to_binary(Int).
