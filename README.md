maps_diff
=====

对比json形式的maps数据差异  
Comparison of json like maps data differences

### 对比差异（compare diff）
```
Map1 = #{
    <<"a">> => 10
    , <<"b">> => [5, 10]
    , <<"del">> => 10
    },
Map2 = #{
    <<"a">> => 10
    , <<"b">> => [10, 20]
    , <<"c">> => #{<<"msg">> => <<"hello">>, <<"data">> => 10}
    , <<"new">> => 10
    },

#{<<"b">> =>
      #{0 => {maps_diff,update,5,10},
        1 => {maps_diff,update,10,20}},
  <<"c">> =>
      {maps_diff,add,undefined,#{<<"data">> => 10,<<"msg">> => <<"hello">>}},
  <<"del">> => {maps_diff,delete,10,undefined},
  <<"new">> => {maps_diff,add,undefined,10}
} = maps_diff:diff(Map1, Map2).
```

### 整合差异（merge diff）
```
Map1 = #{
    <<"a">> => 10
    , <<"b">> => [5, 10]
    , <<"del">> => 10
    },
Map2 = #{
    <<"a">> => 10
    , <<"b">> => [10, 20]
    , <<"c">> => #{<<"msg">> => <<"hello">>, <<"data">> => 10}
    , <<"new">> => 10
    },
Map3 = #{
    <<"a">> => 10
    , <<"b">> => [10, 20]
    , <<"c">> => #{<<"msg">> => <<"hello">>}
    },
    
Diff1 = #{
    <<"b">> =>
        #{0 => {maps_diff,update,5,10},
          1 => {maps_diff,update,10,20}},
    <<"c">> =>
        {maps_diff,add,undefined,#{<<"data">> => 10,<<"msg">> => <<"hello">>}},
    <<"del">> => {maps_diff,delete,10,undefined},
    <<"new">> => {maps_diff,add,undefined,10}
} = maps_diff:diff(Map1, Map2),

Diff2 = #{
    <<"c">> => #{<<"data">> => {maps_diff,delete,10,undefined}},
    <<"new">> => {maps_diff,delete,10,undefined}
} = maps_diff:diff(Map2, Map3),

#{
    <<"b">> =>
        #{0 => {maps_diff,update,5,10},
        1 => {maps_diff,update,10,20}},
    <<"c">> =>
        {maps_diff,add,undefined,#{<<"msg">> => <<"hello">>}},
    <<"del">> => {maps_diff,delete,10,undefined}
} = maps_diff:merge(Diff1, Diff2).
```

### TODO
* 待支持限定层级对比