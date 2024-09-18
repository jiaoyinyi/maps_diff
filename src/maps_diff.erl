%%%-------------------------------------------------------------------
%%% @author jiaoyinyi
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% maps比较
%%% @end
%%% Created : 2024-06-24 00:00:00
%%%-------------------------------------------------------------------
-module(maps_diff).
-export([
    diff/2
    , merge/1, merge/2
]).

%% 对比情况
%% 值类型是基础类型，直接对比
%% 值类型是数据类型，遍历对比
%% 值类型是对象，递归对比

%% maps对比信息
-record(maps_diff, {
    op :: add | delete | update
    , old
    , new
}).

%% @doc 比对
-spec diff(map() | list(), map() | list()) -> map().
diff(OldMap, NewMap) when is_map(OldMap) andalso is_map(NewMap) ->
    diff_map(OldMap, NewMap);
diff(OldList, NewList) when is_list(OldList) andalso is_list(NewList) ->
    diff_list(OldList, NewList);
diff(Old, New) when (is_map(Old) andalso is_list(New)) orelse (is_list(Old) andalso is_map(New)) ->
    #maps_diff{op = update, old = Old, new = New}.

diff_map(OldMap, NewMap) when map_size(OldMap) =:= 0 andalso map_size(NewMap) =/= 0 ->
    #maps_diff{op = add, new = NewMap};
diff_map(OldMap, NewMap) when map_size(OldMap) =/= 0 andalso map_size(NewMap) =:= 0 ->
    #maps_diff{op = delete, old = OldMap};
diff_map(OldMap, NewMap) ->
    DiffMap = diff_map_delete(OldMap, NewMap, #{}),
    diff_map_add_update(OldMap, NewMap, DiffMap).

diff_map_delete(OldMap, NewMap, DiffMap) ->
    maps:fold(
        fun(Key, Val, Acc) ->
            case maps:is_key(Key, NewMap) of
                false ->
                    Diff = #maps_diff{op = delete, old = Val},
                    maps:put(Key, Diff, Acc);
                true ->
                    Acc
            end
        end, DiffMap, OldMap
    ).

diff_map_add_update(OldMap, NewMap, DiffMap) ->
    maps:fold(
        fun(Key, Val, Acc) ->
            case maps:find(Key, OldMap) of
                {ok, Val} -> %% 相同
                    Acc;
                {ok, OldVal} -> %% 更新
                    Diff = diff_update(OldVal, Val),
                    maps:put(Key, Diff, Acc);
                error -> %% 新增
                    Diff = #maps_diff{op = add, new = Val},
                    maps:put(Key, Diff, Acc)
            end
        end, DiffMap, NewMap
    ).

diff_list(OldList, NewList) ->
    diff_list(OldList, NewList, #{}, 0).

diff_list([], [], DiffMap, _Idx) ->
    DiffMap;
diff_list([I | OldList], [I | NewList], DiffMap, Idx) ->
    diff_list(OldList, NewList, DiffMap, Idx + 1);
diff_list([OldI | OldList], [NewI | NewList], DiffMap, Idx) ->
    Diff = diff_update(OldI, NewI),
    NewDiffMap = maps:put(Idx, Diff, DiffMap),
    diff_list(OldList, NewList, NewDiffMap, Idx + 1);
diff_list([OldI | OldList], [], DiffMap, Idx) ->
    Diff = #maps_diff{op = delete, old = OldI},
    NewDiffMap = maps:put(Idx, Diff, DiffMap),
    diff_list(OldList, [], NewDiffMap, Idx + 1);
diff_list([], [NewI | NewList], DiffMap, Idx) ->
    Diff = #maps_diff{op = add, new = NewI},
    NewDiffMap = maps:put(Idx, Diff, DiffMap),
    diff_list([], NewList, NewDiffMap, Idx + 1).

diff_update(OldVal, NewVal) when is_map(OldVal) andalso is_map(NewVal) ->
    diff_map(OldVal, NewVal);
diff_update(OldVal, NewVal) when is_list(OldVal) andalso is_list(NewVal) ->
    diff_list(OldVal, NewVal);
diff_update(OldVal, NewVal) ->
    #maps_diff{op = update, old = OldVal, new = NewVal}.

%% @doc 合并对比
-spec merge([map()]) -> map().
merge([DiffMap]) ->
    DiffMap;
merge([OldDiffMap, NewDiffMap | DiffMaps]) ->
    merge([merge(OldDiffMap, NewDiffMap) | DiffMaps]).

merge(OldDiff = #maps_diff{}, NewDiff = #maps_diff{}) ->
    merge_diff(OldDiff, NewDiff);
merge(OldDiff = #maps_diff{}, NewDiffMap) when is_map(NewDiffMap) ->
    merge_diff(OldDiff, NewDiffMap);
merge(OldDiffMap, NewDiff = #maps_diff{}) when is_map(OldDiffMap) ->
    merge_diff(OldDiffMap, NewDiff);
merge(OldDiffMap, NewDiffMap) when is_map(OldDiffMap) andalso is_map(NewDiffMap) ->
    maps:fold(
        fun(Key, Diff, Acc) ->
            case maps:find(Key, Acc) of
                {ok, NewDiff0} ->
                    case merge_diff(Diff, NewDiff0) of
                        remove ->
                            maps:remove(Key, Acc);
                        NewDiff ->
                            maps:put(Key, NewDiff, Acc)
                    end;
                error ->
                    maps:put(Key, Diff, Acc)
            end
        end, NewDiffMap, OldDiffMap
    ).

merge_diff(#maps_diff{op = add}, #maps_diff{op = update, new = NewVal}) -> %% 新增操作合并更新操作，更新新增操作的值
    #maps_diff{op = add, new = NewVal};
merge_diff(#maps_diff{op = add}, #maps_diff{op = delete}) -> %% 新增操作合并删除操作，移除该操作
    remove;
merge_diff(#maps_diff{op = update, old = OldVal}, #maps_diff{op = update, new = NewVal}) when OldVal =:= NewVal -> %% 更新操作合并更新操作，且更新回原值
    remove;
merge_diff(#maps_diff{op = update, old = OldVal}, #maps_diff{op = update, new = NewVal}) -> %% 更新操作合并更新操作，更新更新操作的值
    #maps_diff{op = update, old = OldVal, new = NewVal};
merge_diff(#maps_diff{op = update}, #maps_diff{op = delete, old = OldVal}) -> %% 更新操作合并删除操作，更新为删除操作
    #maps_diff{op = delete, old = OldVal};
merge_diff(#maps_diff{op = delete, old = OldVal}, #maps_diff{op = add, new = NewVal}) when OldVal =:= NewVal -> %% 删除操作合并新增操作，更新为新增操作
    remove;
merge_diff(#maps_diff{op = delete, old = OldVal}, #maps_diff{op = add, new = NewVal}) -> %% 删除操作合并新增操作，更新为新增操作
    #maps_diff{op = update, old = OldVal, new = NewVal};
merge_diff(OldDiff, #maps_diff{op = update, old = OldVal, new = NewVal}) when is_map(OldDiff) -> %% 子值操作合并更新操作，更新更新操作的值
    #maps_diff{op = update, old = OldVal, new = NewVal};
merge_diff(OldDiff, #maps_diff{op = delete, old = OldVal}) when is_map(OldDiff) -> %% 子值操作合并删除操作，更新为删除操作
    #maps_diff{op = delete, old = OldVal};
merge_diff(#maps_diff{op = add, new = NewVal0}, NewDiff) when is_map(NewVal0) andalso is_map(NewDiff) -> %% 新增操作合并子值操作，更新新增操作的值
    Ret =
        case merge_diff_vals(NewVal0, NewDiff) of
            remove ->
                remove;
            NewVal ->
                #maps_diff{op = add, new = NewVal}
        end,
    Ret;
merge_diff(#maps_diff{op = update, old = OldVal, new = NewVal0}, NewDiff) when is_map(NewVal0) andalso is_map(NewDiff) -> %% 更新操作合并子值操作，更新更新操作的值
    case merge_diff_vals(NewVal0, NewDiff) of
        remove ->
            remove;
        NewVal ->
            #maps_diff{op = update, old = OldVal, new = NewVal}
    end;
merge_diff(OldDiff, NewDiff) when is_map(OldDiff) andalso is_map(NewDiff) -> %% 子值操作合并子值操作，递归按前面规则合并处理
    merge(OldDiff, NewDiff).

merge_diff_vals(ValMap, DiffMap) when is_map(ValMap) ->
    io:format("ValMap:~w, DiffMap:~w~n", [ValMap, DiffMap]),
    Ret = maps:fold(
        fun(Key, Diff, Acc) ->
            io:format("Key:~w, Diff:~w, Acc:~w~n", [ValMap, DiffMap, Acc]),
            merge_diff_val(Acc, Key, Diff)
        end, ValMap, DiffMap
    ),
    case map_size(Ret) =:= 0 of
        true ->
            remove;
        _ ->
            Ret
    end.
merge_diff_val(ValMap, Key, #maps_diff{op = add, new = Val}) ->
    maps:put(Key, Val, ValMap);
merge_diff_val(ValMap, Key, #maps_diff{op = update, new = Val}) ->
    maps:put(Key, Val, ValMap);
merge_diff_val(ValMap, Key, #maps_diff{op = delete}) ->
    maps:remove(Key, ValMap);
merge_diff_val(ValMap, Key, DiffMap) when is_map(DiffMap) -> %% 子值还是maps格式，递归处理 #{<<"data">> => #{<<"int">> => 1},<<"msg">> => <<"hello">>} #{<<"data">> => #{<<"float">> => {maps_diff,add,undefined,1.2}, <<"int">> => {maps_diff,update,1,2}}}
    Val = maps:get(Key, ValMap),
    case merge_diff_vals(Val, DiffMap) of
        remove ->
            maps:remove(Key, ValMap);
        NewVal ->
            maps:put(Key, NewVal, ValMap)
    end.