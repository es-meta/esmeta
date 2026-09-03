# 추상 연산

[← 프로그램 실행](running.md) · [투명 내장 연산 →](builtins.md)

여기 있는 연산들은 추론 규칙이 기대어 쓰는 것들이고, 명세가 하나하나 직접
정의한다.
ECMA-262라면 추상 연산 이름을 부를 자리에서 규칙도 이 중 하나를 부른다. 정의가
여기 따로 있으니 규칙은 자기가 말하려는 것만 말하면 된다.

명세가 선언만 하고 정의하지 않는 연산은 [투명 내장 연산](builtins.md)에 따로
모아 두었다.

## 일반 연산

아래 전체에서 쓰는 리스트, 선택 값, 사상, 집합 연산이다. 집합은 괄호로 묶은
열이고 사상은 쌍의 집합인데, 이것이 표준 라이브러리가 읽어 들이는 모양이다.

<a id="set"></a>
<a id="pair"></a>
<a id="map"></a>
```spectec
syntax set<K> = 
   | `{K*}

syntax pair<K, V> = 
   | {K : V}

syntax map<K, V> = set<pair<K, V>>
```

### $get_

<a id="get_"></a>
`$get_<X>(X'?)`:
The contents of `X'?`

```spectec
def $get_<X>(X) = X
```

1. [Let!<sub>type</sub>](index.md#check_let) `X` be `X'?`.
1. Return `X`.

### $is_some_

<a id="is_some_"></a>
`$is_some_<X>(X''?)`:
`X''?` is defined

```spectec
def $is_some_<X>(eps) = false

def $is_some_<X>(X) = true
```

1. If `X''?` matches pattern `()`: return `false`.
1. Else if let `X` be `X''?`: return `true`.

### $filter_some_

<a id="filter_some_"></a>
`$filter_some_<X>(X'''?*)`:
The defined elements of `X'''?*`

```spectec
def $filter_some_<X>(eps) = eps

def $filter_some_<X>(eps :: (X?)*) = $filter_some_<X>((X?)*)

def $filter_some_<X>(X_1 :: (X?)*) = X_1 :: $filter_some_<X>((X?)*)
```

1. Let `X?` be `X'''?`, for all `X'''?` in `X'''?*` and `X?` in `X?*`.
1. If `X?*` matches pattern `[]`:
   1. Return `·`.
1. Else if let `X'? :: X?*` be `X?*`:
   1. If `X'?` matches pattern `()`:
      1. Return [the defined elements of `X?*`](abstract-operations.md#filter_some_).
   1. Else if let `X_1` be `X'?`:
      1. Return `X_1 :: $filter_some_<X>(X?*)`.

### $insert_sorted_

<a id="insert_sorted_"></a>
`$insert_sorted_<X>(i, X, (int, X)*)`:
`(int, X)*` with `X` inserted at key `i`

```spectec
def $insert_sorted_<X>(i, X, eps) = (i, X) :: eps

def $insert_sorted_<X>(i, X, (i_h, X_h) :: (i_t, X_t)*) = (i, X) :: (i_h, X_h) :: (i_t, X_t)*
 -- if i <= i_h

def $insert_sorted_<X>(i, X, (i_h, X_h) :: (i_t, X_t)*) = (i_h, X_h) :: $insert_sorted_<X>(i, X, (i_t, X_t)*)
 -- otherwise
```

1. If `(int, X)*` matches pattern `[]`:
   1. Return `( i, X ) :: ·`.
1. Else if let `( i_h, X_h ) :: ( i_t, X_t )*` be `(int, X)*`:
   1. Check that `i` is less than or equal to `i_h`.
   1. Return `( i, X ) :: ( i_h, X_h ) :: ( i_t, X_t )*`.

1. Otherwise:
   1. Otherwise:
      1. Check that `(int, X)*` is a non-empty list.
      1. Let `( i_h, X_h ) :: ( i_t, X_t )*` be `(int, X)*`.
      1. Return `( i_h, X_h ) :: $insert_sorted_<X>(i, X, ( i_t, X_t )*)`.

### $sort_

<a id="sort_"></a>
`$sort_<X>((int, X)*)`:
`(int, X)*` ordered by its first components

```spectec
def $sort_<X>(eps) = eps

def $sort_<X>((i_h, X_h) :: (i_t, X_t)*) = $insert_sorted_<X>(i_h, X_h, $sort_<X>((i_t, X_t)*))
```

1. If `(int, X)*` matches pattern `[]`:
   1. Return `·`.
1. Else if let `( i_h, X_h ) :: ( i_t, X_t )*` be `(int, X)*`:
   1. Return [`( i_t, X_t )*` ordered by its first components with `X_h` inserted at key `i_h`](abstract-operations.md#insert_sorted_).

### $empty_set

<a id="empty_set"></a>
`$empty_set<K>`

```spectec
def $empty_set<K> = `{eps}
```

1. Return `{·}`.

### $in_set

<a id="in_set"></a>
`$in_set<K>(K, {K_e*})`:
`K` is an element of the set `{K_e*}`

```spectec
def $in_set<K>(K, `{K_e*}) = K <- K_e*
```

1. Return `K` is in `K_e*`.

### $empty_map

<a id="empty_map"></a>
`$empty_map<K, V>`:
An empty map

```spectec
def $empty_map<K, V> = `{eps}
```

1. Return `{·}`.

### $dom_map

<a id="dom_map"></a>
`$dom_map<K, V>({( K : V )*})`:
The domain of the map `{( K : V )*}`

```spectec
def $dom_map<K, V>(`{(K : V)*}) = `{K*}
```

1. Return `{K*}`.

### $codom_map

<a id="codom_map"></a>
`$codom_map<K, V>({( K : V )*})`:
The codomain of the map `{( K : V )*}`

```spectec
def $codom_map<K, V>(`{(K : V)*}) = `{V*}
```

1. Return `{V*}`.

## <a id="state-operations"></a>상태 연산

함수 표와 환경, 힙, 출력 흐름을 읽고 쓴다.

### $empty_state

<a id="empty_state"></a>
`$empty_state(context)`:
An empty state with context `context`

```spectec
def $empty_state(context) = {FUNC $empty_map<id, func>, CTX context, STACK eps, GLOBAL $empty_map<global, value>, HEAP {MAP $empty_map<addr, obj>, SIZE 0}, OUT eps, ASTID 0}
```

1. Return {FUNC [an empty map](abstract-operations.md#empty_map), CTX `context`, STACK `·`, GLOBAL [an empty map](abstract-operations.md#empty_map), HEAP {MAP [an empty map](abstract-operations.md#empty_map), SIZE `0`}, OUT `·`, ASTID `0`}.

### $name_of

<a id="name_of"></a>
`$name_of(bool funcKind id param* type inst)`:
The name of `bool funcKind id param* type inst`

```spectec
def $name_of(bool funcKind id param* type inst) = id
```

1. Return `id`.

### $funcs_of

<a id="funcs_of"></a>
`$funcs_of(func''*)`:
The function table of `func''*`

```spectec
def $funcs_of(eps) = $empty_map<id, func>

def $funcs_of(func_1 :: func*) = $add_map<id, func>($funcs_of(func*), $name_of(func_1), func_1)
```

1. If `func''*` matches pattern `[]`:
   1. Return [an empty map](abstract-operations.md#empty_map).
1. Else if let `func_1 :: func*` be `func''*`:
   1. Return [the map the function table of `func*` with the name of `func_1` bound to `func_1`](builtins.md#add_map).

### $main_funcs

<a id="main_funcs"></a>
`$main_funcs(func''*)`:
The functions of `func''*` carrying the main flag

```spectec
def $main_funcs(eps) = eps

def $main_funcs(func_1 :: func*) = func_1 :: $main_funcs(func*)
 -- if func_1 = (true funcKind id param* type inst)

def $main_funcs(func_1 :: func*) = $main_funcs(func*)
 -- otherwise
```

1. If `func''*` matches pattern `[]`:
   1. Return `·`.
1. Else if let `func_1 :: func*` be `func''*`:
   1. Let `bool funcKind id param* type inst` be `func_1`.
   1. Check that `bool` is equal to `true`.
   1. Return `func_1 :: $main_funcs(func*)`.

1. Otherwise:
   1. Otherwise:
      1. Check that `func''*` is a non-empty list.
      1. Let `func_1 :: func*` be `func''*`.
      1. Return [the functions of `func*` carrying the main flag](abstract-operations.md#main_funcs).

### $main_func

<a id="main_func"></a>
`$main_func(program)`:
The main function of `program`

```spectec
def $main_func(program) = func
 -- if $main_funcs(program) = func :: eps
```

1. Let `func''*` be [the functions of `program` carrying the main flag](abstract-operations.md#main_funcs).
1. [Let!<sub>type</sub>](index.md#check_let) `func :: func'*` be `func''*`.
1. Check that `func'*` is an empty list.
1. Return `func`.

### $body_of

<a id="body_of"></a>
`$body_of(b funcKind id param* type inst'')`:
The body of `b funcKind id param* type inst''`

```spectec
def $body_of(b funcKind id param* type (IBlock inst*)) = inst*

def $body_of(b funcKind id param* type inst) = inst :: eps
```

1. [Let!<sub>type</sub>](index.md#check_let) **`IBlock`** `inst*` be `inst''`.
1. Return `inst*`.
1. Return `inst'' :: ·`.

### $params_of

<a id="params_of"></a>
`$params_of(b funcKind id param* type inst)`:
The parameters of `b funcKind id param* type inst`

```spectec
def $params_of(b funcKind id param* type inst) = param*
```

1. Return `param*`.

### $init_context

<a id="init_context"></a>
`$init_context(func)`:
An initial context on `func`

```spectec
def $init_context(func) = {FUNC func, LOCAL $empty_map<local, value>}
```

1. Return {FUNC `func`, LOCAL [an empty map](abstract-operations.md#empty_map)}.

### $init_state

<a id="init_state"></a>
`$init_state(program)`:
The initial state for `program`

```spectec
def $init_state(program) = $empty_state($init_context($main_func(program)))[FUNC = $funcs_of(program)]
```

1. Return `$empty_state($init_context($main_func(program)))` with `FUNC` set to [the function table of `program`](abstract-operations.md#funcs_of).

### $init_insts

<a id="init_insts"></a>
`$init_insts(program)`:
The initial instruction sequence of `program`

```spectec
def $init_insts(program) = $body_of($main_func(program))
```

1. Return [the body of the main function of `program`](abstract-operations.md#body_of).

### $find_func

<a id="find_func"></a>
`$find_func(st, id)`:
The function named `id` in `st`

```spectec
def $find_func(st, id) = $find_map<id, func>(st.FUNC, id)
```

1. Return [the value of `id` in map `st.FUNC`](builtins.md#find_map).

### $bind_local

<a id="bind_local"></a>
`$bind_local(st, local, value)`:
`st` with local `local` bound to `value`

```spectec
def $bind_local(st, local, value) = st[CTX.LOCAL = $add_map<local, value>(st.CTX.LOCAL, local, value)]
```

1. Return `st` with `CTX.LOCAL` set to [the map `st.CTX.LOCAL` with `local` bound to `value`](builtins.md#add_map).

### $bind_global

<a id="bind_global"></a>
`$bind_global(st, global, value)`:
`st` with global `global` bound to `value`

```spectec
def $bind_global(st, global, value) = st[GLOBAL = $add_map<global, value>(st.GLOBAL, global, value)]
```

1. Return `st` with `GLOBAL` set to [the map `st.GLOBAL` with `global` bound to `value`](builtins.md#add_map).

### $print_out

<a id="print_out"></a>
`$print_out(st, value)`:
`st` with `value` appended to its output

```spectec
def $print_out(st, value) = st[OUT = st.OUT ++ [value]]
```

1. Return `st` with `OUT` set to `st.OUT` concatenated with `value`.

### $bind_heap

<a id="bind_heap"></a>
`$bind_heap(st, addr, obj)`:
`st` with `obj` stored at `addr`

```spectec
def $bind_heap(st, addr, obj) = st[HEAP.MAP = $add_map<addr, obj>(st.HEAP.MAP, addr, obj)]
```

1. Return `st` with `HEAP.MAP` set to [the map `st.HEAP.MAP` with `addr` bound to `obj`](builtins.md#add_map).

### $find_local

<a id="find_local"></a>
`$find_local(st, local)`:
The value of local `local` in `st`

```spectec
def $find_local(st, local) = $find_map<local, value>(st.CTX.LOCAL, local)
```

1. Return [the value of `local` in map `st.CTX.LOCAL`](builtins.md#find_map).

### $find_global

<a id="find_global"></a>
`$find_global(st, global)`:
The value of global `global` in `st`

```spectec
def $find_global(st, global) = $find_map<global, value>(st.GLOBAL, global)
```

1. Return [the value of `global` in map `st.GLOBAL`](builtins.md#find_map).

### $find_heap

<a id="find_heap"></a>
`$find_heap(st, addr)`:
The object at `addr` in the heap of `st`

```spectec
def $find_heap(st, addr) = $find_map<addr, obj>(st.HEAP.MAP, addr)
```

1. Return [the value of `addr` in map `st.HEAP.MAP`](builtins.md#find_map).

### $exists_local

<a id="exists_local"></a>
`$exists_local(st, local)`

```spectec
def $exists_local(st, local) = $is_some_<value>($find_local(st, local))
```

1. Return [the value of local `local` in `st` is defined](abstract-operations.md#is_some_).

### $exists_global

<a id="exists_global"></a>
`$exists_global(st, global)`

```spectec
def $exists_global(st, global) = $is_some_<value>($find_global(st, global))
```

1. Return [the value of global `global` in `st` is defined](abstract-operations.md#is_some_).

### $exists_heap

<a id="exists_heap"></a>
`$exists_heap(st, addr)`

```spectec
def $exists_heap(st, addr) = $is_some_<obj>($find_heap(st, addr))
```

1. Return [the object at `addr` in the heap of `st` is defined](abstract-operations.md#is_some_).

### $bind_params

<a id="bind_params"></a>
`$bind_params(param''*, value*)`:
The locals binding `param''*` to `value*`

```spectec
def $bind_params(eps, value*) = $empty_map<local, value>

def $bind_params((id type b) :: param*, eps) = $bind_params(param*, eps)

def $bind_params((id type b) :: param*, value_h :: value_t*) = $add_map<local, value>($bind_params(param*, value_t*), Name id, value_h)
```

1. If `param''*` matches pattern `[]`:
   1. Return [an empty map](abstract-operations.md#empty_map).
1. Else if let `id type b :: param*` be `param''*`:
   1. If `value*` matches pattern `[]`:
      1. Return [the locals binding `param*` to `·`](abstract-operations.md#bind_params).
   1. Else if let `value_h :: value_t*` be `value*`:
      1. Return [the map the locals binding `param*` to `value_t*` with **`Name`** `id` bound to `value_h`](builtins.md#add_map).

### $add_captured

<a id="add_captured"></a>
`$add_captured(mlv, set<pair<name, value>>')`:
`mlv` overlaid with the captured environment `set<pair<name, value>>'`

```spectec
def $add_captured(mlv, `{eps}) = mlv

def $add_captured(mlv, `{(name : value) :: (name_t : value_t)*}) = $add_captured($add_map<local, value>(mlv, name, value), `{(name_t : value_t)*})
```

1. Check that `set<pair<name, value>>'` is equal to `{·}`.
1. Return `mlv`.
1. Let `{pair<name, value>*}` be `set<pair<name, value>>'`.
1. [Let!<sub>type</sub>](index.md#check_let) `name : value :: ( name_t : value_t )*` be `pair<name, value>*`.
1. Return [the map `mlv` with `name` bound to `value` overlaid with the captured environment `{( name_t : value_t )*}`](abstract-operations.md#add_captured).

### $callee_context

<a id="callee_context"></a>
`$callee_context(func, value*, mnv)`:
The context a call to `func` with arguments `value*` and captured environment `mnv` enters

```spectec
def $callee_context(func, value*, mnv) = {FUNC func, LOCAL $add_captured($bind_params($params_of(func), value*), mnv)}
```

1. Return {FUNC `func`, LOCAL [the locals binding the parameters of `func` to `value*` overlaid with the captured environment `mnv`](abstract-operations.md#add_captured)}.

### $fresh_ast_id

<a id="fresh_ast_id"></a>
`$fresh_ast_id(st)`:
The next AST serial number of `st` and the state that follows it

```spectec
def $fresh_ast_id(st) = (st.ASTID, st[ASTID = st.ASTID + 1])
```

1. Return ( `st.ASTID`, `st` with `ASTID` set to `st.ASTID` + `1` ).

## 수치 변환

### $as_int

<a id="as_int"></a>
`$as_int(value)`:
The integer denoted by `value`

```spectec
def $as_int(MathV (BigDec int_u int_s)) = int_u / $pow10(nat_s)
 -- if int_s >= 0
 -- if int_s = nat_s
 -- if int_u \ $pow10(nat_s) = 0

def $as_int(MathV (BigDec int_u int_s)) = int_u * $pow10(nat_s)
 -- if int_s < 0
 -- if -int_s = nat_s
```

1. [Let!<sub>type</sub>](index.md#check_let) **`MathV`** `BigDec int_u int_s` be `value`.
1. Try:
   1. 
      1. Check that `int_s` is greater than or equal to `0`.
      1. [Let!<sub>type</sub>](index.md#check_let) `nat_s` be `int_s`.
      1. Check that `int_u` \ [10 raised to the power of `nat_s`](builtins.md#pow10) is equal to `0`.
      1. Return `int_u` / [10 raised to the power of `nat_s`](builtins.md#pow10).
   1. 
      1. Check that `int_s` is less than `0`.
      1. Let `int` be `-int_s`.
      1. [Let!<sub>type</sub>](index.md#check_let) `nat_s` be `int`.
      1. Return `int_u` * [10 raised to the power of `nat_s`](builtins.md#pow10).

### $to_bigdec

<a id="to_bigdec"></a>
`$to_bigdec(int)`:
`int` as a decimal

```spectec
def $to_bigdec(int) = BigDec int 0
```

1. Return **`BigDec`** `int` `0`.

## 객체 연산

힙에 놓인 그대로의 레코드와 사상, 리스트를 다룬다.

### $read_obj

<a id="read_obj"></a>
`$read_obj(obj', value'')`:
The value of field `value''` in `obj'`

```spectec
def $read_obj(RecordObj _ mtv, StringV text) = $find_map<text, value>(mtv, text)

def $read_obj(MapObj mvv, value) = $find_map<value, value>(mvv, value)

def $read_obj(ListObj value*, MathV bigDecimal) = value*[n]
 -- if n = $as_int(MathV bigDecimal)

def $read_obj(o, v) = eps
 -- otherwise
```

1. If let **`RecordObj`** `_` `mtv` be `obj'`:
   1. [Let!<sub>type</sub>](index.md#check_let) **`StringV`** `text` be `value''`.
   1. Return [the value of `text` in map `mtv`](builtins.md#find_map).
1. Else if let **`MapObj`** `mvv` be `obj'`:
   1. Return [the value of `value''` in map `mvv`](builtins.md#find_map).
1. Else if let **`ListObj`** `value*` be `obj'`:
   1. [Let!<sub>type</sub>](index.md#check_let) **`MathV`** `bigDecimal` be `value''`.
   1. Let `int` be [the integer denoted by **`MathV`** `bigDecimal`](abstract-operations.md#as_int).
   1. [Let!<sub>type</sub>](index.md#check_let) `n` be `int`.
   1. Return `value*[n]`.

1. Otherwise:
   1. Otherwise:
      1. Return `·`.

### $exists_obj

<a id="exists_obj"></a>
`$exists_obj(obj', value')`:
`obj'` has field `value'`

```spectec
def $exists_obj(RecordObj t_name mtv, StringV t) = $is_some_<value>($find_map<text, value>(mtv, t))

def $exists_obj(MapObj mvv, v) = $is_some_<value>($find_map<value, value>(mvv, v))

def $exists_obj(ListObj v*, MathV bigDecimal) = 0 <= i /\ i < |v*|
 -- if i = $as_int(MathV bigDecimal)

def $exists_obj(o, v) = false
 -- otherwise
```

1. If let **`RecordObj`** `t_name` `mtv` be `obj'`:
   1. [Let!<sub>type</sub>](index.md#check_let) **`StringV`** `t` be `value'`.
   1. Return [the value of `t` in map `mtv` is defined](abstract-operations.md#is_some_).
1. Else if let **`MapObj`** `mvv` be `obj'`:
   1. Return [the value of `value'` in map `mvv` is defined](abstract-operations.md#is_some_).
1. Else if let **`ListObj`** `v*` be `obj'`:
   1. [Let!<sub>type</sub>](index.md#check_let) **`MathV`** `bigDecimal` be `value'`.
   1. Let `i` be [the integer denoted by **`MathV`** `bigDecimal`](abstract-operations.md#as_int).
   1. Return `0` is less than or equal to `i` and `i` is less than `the length of v*`.

1. Otherwise:
   1. Otherwise:
      1. Return `false`.

### $size_of_obj

<a id="size_of_obj"></a>
`$size_of_obj(obj)`:
The number of elements in `obj`

```spectec
def $size_of_obj(ListObj v*) = |v*|
```

1. [Let!<sub>type</sub>](index.md#check_let) **`ListObj`** `v*` be `obj`.
1. Return `the length of v*`.

### $update_obj

<a id="update_obj"></a>
`$update_obj(obj, value', v_v')`:
`obj` with field `value'` set to `v_v'`

```spectec
def $update_obj(RecordObj t_name mtv, StringV t_f, v_v) = RecordObj t_name $add_map<text, value>(mtv, t_f, v_v)

def $update_obj(MapObj mvv, v_key, v_value) = MapObj $add_map<value, value>(mvv, v_key, v_value)

def $update_obj(ListObj v*, MathV bigDecimal, v_value) = ListObj (v*[[n_idx] = v_value])
 -- if i_idx = $as_int(MathV bigDecimal)
 -- if |v*| > i_idx /\ i_idx >= 0
 -- if n_idx = i_idx
```

1. If let **`RecordObj`** `t_name` `mtv` be `obj`:
   1. [Let!<sub>type</sub>](index.md#check_let) **`StringV`** `t_f` be `value'`.
   1. Return **`RecordObj`** `t_name` `$add_map<text, value>(mtv, t_f, v_v')`.
1. Else if let **`MapObj`** `mvv` be `obj`:
   1. Return **`MapObj`** `$add_map<value, value>(mvv, value', v_v')`.
1. Else if let **`ListObj`** `v*` be `obj`:
   1. [Let!<sub>type</sub>](index.md#check_let) **`MathV`** `bigDecimal` be `value'`.
   1. Let `i_idx` be [the integer denoted by **`MathV`** `bigDecimal`](abstract-operations.md#as_int).
   1. Check that `the length of v*` is greater than `i_idx` and `i_idx` is greater than or equal to `0`.
   1. [Let!<sub>type</sub>](index.md#check_let) `n_idx` be `i_idx`.
   1. Return **`ListObj`** `v*[[n_idx] = v_v']`.

### $int_like_keys

<a id="int_like_keys"></a>
`$int_like_keys(value''*)`:
The integer-like keys of `value''*`

```spectec
def $int_like_keys(eps) = eps

def $int_like_keys(StringV t :: v*) = (i, t) :: $int_like_keys(v*)
 -- if double = $str_to_number(t)
 -- if $number_to_text(double, 10) = t
 -- if i = $bigint_of_double(double)
 -- if $numeq_double(double, $double_of_int(i))

def $int_like_keys(v_h :: v_t*) = $int_like_keys(v_t*)
 -- otherwise
```

1. If `value''*` matches pattern `[]`:
   1. Return `·`.
1. Else if let `value :: v*` be `value''*`:
   1. [Let!<sub>type</sub>](index.md#check_let) **`StringV`** `t` be `value`.
   1. Let `double` be [the result of parsing `t` as a StringNumericLiteral](builtins.md#str_to_number).
   1. Check that [the string representation of `double` in radix `10`](builtins.md#number_to_text) is equal to `t`.
   1. Let `i` be [the integral part of `double`](builtins.md#bigint_of_double).
   1. Check that [`double` and `i` as a double are numerically equal](builtins.md#numeq_double).
   1. Return `( i, t ) :: $int_like_keys(v*)`.

1. Otherwise:
   1. Otherwise:
      1. Check that `value''*` is a non-empty list.
      1. Let `v_h :: v_t*` be `value''*`.
      1. Return [the integer-like keys of `v_t*`](abstract-operations.md#int_like_keys).

### $keys_obj

<a id="keys_obj"></a>
`$keys_obj(obj, _)`:
The keys of `obj` sorted numerically if `_`

```spectec
def $keys_obj(RecordObj t mtv, _) = (StringV t_key)*
 -- if `{t_key*} = $dom_map<text, value>(mtv)

def $keys_obj(MapObj mvv, true) = (StringV t)*
 -- if `{v_key*} = $dom_map<value, value>(mvv)
 -- if (i, t)* = $sort_<text>($int_like_keys(v_key*))

def $keys_obj(MapObj mvv, false) = v_key*
 -- if `{v_key*} = $dom_map<value, value>(mvv)
```

1. If let **`RecordObj`** `t` `mtv` be `obj`:
   1. Let `{t_key*}` be [the domain of the map `mtv`](abstract-operations.md#dom_map).
   1. Return `( StringV t_key )*`.
1. Else if let **`MapObj`** `mvv` be `obj`:
   1. If `_` is equal to `true`:
      1. Let `{v_key*}` be [the domain of the map `mvv`](abstract-operations.md#dom_map).
      1. Let `( i, t )*` be [the integer-like keys of `v_key*` ordered by its first components](abstract-operations.md#sort_).
      1. Return `( StringV t )*`.
   1. Else if `_` is equal to `false`:
      1. Let `{v_key*}` be [the domain of the map `mvv`](abstract-operations.md#dom_map).
      1. Return `v_key*`.

### $expand_obj

<a id="expand_obj"></a>
`$expand_obj(obj, value)`:
`obj` expanded with field `value`

```spectec
def $expand_obj(RecordObj t mtv, StringV t_field) = RecordObj t $add_map<text, value>(mtv, t_field, UndefV)
 -- if $is_some_<value>($find_map<text, value>(mtv, t_field))

def $expand_obj(RecordObj t mtv, StringV t_field) = RecordObj t mtv
 -- if ~$is_some_<value>($find_map<text, value>(mtv, t_field))
```

1. [Let!<sub>type</sub>](index.md#check_let) **`RecordObj`** `t` `mtv` be `obj`.
1. [Let!<sub>type</sub>](index.md#check_let) **`StringV`** `t_field` be `value`.
1. If [the value of `t_field` in map `mtv` is defined](abstract-operations.md#is_some_):
   1. Return **`RecordObj`** `t` `$add_map<text, value>(mtv, t_field, UndefV)`.
1. Else if [the value of `t_field` in map `mtv` is undefined](abstract-operations.md#is_some_):
   1. Return **`RecordObj`** `t` `mtv`.

### $delete_obj

<a id="delete_obj"></a>
`$delete_obj(obj, v_key)`:
`obj` with field `v_key` removed

```spectec
def $delete_obj(MapObj mvv, v_key) = MapObj $del_map<value, value>(mvv, v_key)
```

1. [Let!<sub>type</sub>](index.md#check_let) **`MapObj`** `mvv` be `obj`.
1. Return **`MapObj`** `$del_map<value, value>(mvv, v_key)`.

### $push_obj

<a id="push_obj"></a>
`$push_obj(obj, v_target, bool)`:
`obj` with `v_target` added at its front if `bool` or its end otherwise

```spectec
def $push_obj(ListObj v*, v_target, true) = ListObj (v_target :: v*)

def $push_obj(ListObj v*, v_target, false) = ListObj (v* ++ [v_target])
```

1. [Let!<sub>type</sub>](index.md#check_let) **`ListObj`** `v*` be `obj`.
1. If `bool` is equal to `true`:
   1. Return **`ListObj`** `v_target :: v*`.
1. Else if `bool` is equal to `false`:
   1. Return **`ListObj`** `v* ++ v_target`.

### $pop_obj

<a id="pop_obj"></a>
`$pop_obj(obj, bool)`:
`obj` less its first element if `bool` or its last otherwise, and that element

```spectec
def $pop_obj(ListObj v*, true) = (ListObj v_tail*, v_front)
 -- if |v*| > 0
 -- if v_front :: v_tail* = v*

def $pop_obj(ListObj v*, false) = (ListObj v_new*, v_popped)
 -- if i_lastIdx = |v*| - 1
 -- if i_lastIdx > 0
 -- if n_idx = i_lastIdx
 -- if v_popped = v*[n_idx]
 -- if v_new* = v*[0 : n_idx]
```

1. [Let!<sub>type</sub>](index.md#check_let) **`ListObj`** `v*` be `obj`.
1. If `bool` is equal to `true`:
   1. Check that the length of `v*` is greater than `0`.
   1. Let `value*` be `v*`.
   1. [Let!<sub>type</sub>](index.md#check_let) `v_front :: v_tail*` be `value*`.
   1. Return ( **`ListObj`** `v_tail*`, `v_front` ).
1. Else if `bool` is equal to `false`:
   1. Let `i_lastIdx` be `the length of v* - 1`.
   1. Check that `i_lastIdx` is greater than `0`.
   1. [Let!<sub>type</sub>](index.md#check_let) `n_idx` be `i_lastIdx`.
   1. Let `v_popped` be `v*[n_idx]`.
   1. Let `v_new*` be `v*[0 : n_idx]`.
   1. Return ( **`ListObj`** `v_new*`, `v_popped` ).

## 힙 연산

같은 연산을 주소로 찾아가서 한다.

### $alloc_heap

<a id="alloc_heap"></a>
`$alloc_heap(state, obj)`:
A fresh address for `obj` and the state that holds it

```spectec
def $alloc_heap(state, obj) = (a, $bind_heap(state, a, obj)[HEAP.SIZE = state.HEAP.SIZE + 1])
 -- if a = DynamicAddr state.HEAP.SIZE
```

1. Let `a` be **`DynamicAddr`** `state.HEAP.SIZE`.
1. Return ( `a`, `$bind_heap(state, a, obj)` with `HEAP.SIZE` set to `state.HEAP.SIZE` + `1` ).

### $alloc_record

<a id="alloc_record"></a>
`$alloc_record(state, text, mtv)`:
A fresh address for the `text` record with fields `mtv` and the state that holds it

```spectec
def $alloc_record(state, text, mtv) = $alloc_heap(state, (RecordObj text mtv))
```

1. Return [a fresh address for **`RecordObj`** `text` `mtv` and the state that holds it](abstract-operations.md#alloc_heap).

### $alloc_map

<a id="alloc_map"></a>
`$alloc_map(state, mvv)`:
A fresh address for the map `mvv` and the state that holds it

```spectec
def $alloc_map(state, mvv) = $alloc_heap(state, (MapObj mvv))
```

1. Return [a fresh address for **`MapObj`** `mvv` and the state that holds it](abstract-operations.md#alloc_heap).

### $alloc_list

<a id="alloc_list"></a>
`$alloc_list(state, value*)`:
A fresh address for the list `value*` and the state that holds it

```spectec
def $alloc_list(state, value*) = $alloc_heap(state, (ListObj value*))
```

1. Return [a fresh address for **`ListObj`** `value*` and the state that holds it](abstract-operations.md#alloc_heap).

### $copy_obj

<a id="copy_obj"></a>
`$copy_obj(state, obj)`:
A fresh address holding a copy of `obj` and the state that holds it

```spectec
def $copy_obj(state, (RecordObj t mtv)) = $alloc_heap(state, (RecordObj t mtv))

def $copy_obj(state, (MapObj mvv)) = $alloc_heap(state, (MapObj mvv))

def $copy_obj(state, (ListObj v*)) = $alloc_heap(state, (ListObj v*))

def $copy_obj(state, (YetObj t_n t_m)) = $alloc_heap(state, (YetObj t_n t_m))
```

1. If let **`RecordObj`** `t` `mtv` be `obj`:
   1. Return [a fresh address for **`RecordObj`** `t` `mtv` and the state that holds it](abstract-operations.md#alloc_heap).
1. Else if let **`MapObj`** `mvv` be `obj`:
   1. Return [a fresh address for **`MapObj`** `mvv` and the state that holds it](abstract-operations.md#alloc_heap).
1. Else if let **`ListObj`** `v*` be `obj`:
   1. Return [a fresh address for **`ListObj`** `v*` and the state that holds it](abstract-operations.md#alloc_heap).
1. Else if let **`YetObj`** `t_n` `t_m` be `obj`:
   1. Return [a fresh address for **`YetObj`** `t_n` `t_m` and the state that holds it](abstract-operations.md#alloc_heap).

### $copy_heap

<a id="copy_heap"></a>
`$copy_heap(state, addr)`:
A fresh address holding a copy of `addr` and the state that holds it

```spectec
def $copy_heap(state, addr) = $copy_obj(state, obj)
 -- if obj = $find_heap(state, addr)
```

1. Let `obj` be [**!**](index.md#option_get) [the object at `addr` in the heap of `state`](abstract-operations.md#find_heap).
1. Return [a fresh address holding a copy of `obj` and the state that holds it](abstract-operations.md#copy_obj).

### $keys_heap

<a id="keys_heap"></a>
`$keys_heap(state, addr, bool)`:
A fresh address holding the keys of `addr` sorted numerically if `bool` and the state that holds it

```spectec
def $keys_heap(state, addr, bool) = $alloc_list(state, v_keys*)
 -- if obj = $find_heap(state, addr)
 -- if v_keys* = $keys_obj(obj, bool)
```

1. Let `obj` be [**!**](index.md#option_get) [the object at `addr` in the heap of `state`](abstract-operations.md#find_heap).
1. Let `v_keys*` be [the keys of `obj` sorted numerically if `bool`](abstract-operations.md#keys_obj).
1. Return [a fresh address for the list `v_keys*` and the state that holds it](abstract-operations.md#alloc_list).

### $delete_heap

<a id="delete_heap"></a>
`$delete_heap(state, addr, value)`:
`state` with field `value` removed from the object at `addr`

```spectec
def $delete_heap(state, addr, value) = $bind_heap(state, addr, $delete_obj(obj, value))
 -- if obj = $find_heap(state, addr)
```

1. Let `obj` be [**!**](index.md#option_get) [the object at `addr` in the heap of `state`](abstract-operations.md#find_heap).
1. Return [`state` with `obj` with field `value` removed stored at `addr`](abstract-operations.md#bind_heap).

### $expand_heap

<a id="expand_heap"></a>
`$expand_heap(state, addr, value)`:
`state` with the object at `addr` expanded by field `value`

```spectec
def $expand_heap(state, addr, value) = $bind_heap(state, addr, $expand_obj(obj, value))
 -- if obj = $find_heap(state, addr)
```

1. Let `obj` be [**!**](index.md#option_get) [the object at `addr` in the heap of `state`](abstract-operations.md#find_heap).
1. Return [`state` with `obj` expanded with field `value` stored at `addr`](abstract-operations.md#bind_heap).

### $update_heap

<a id="update_heap"></a>
`$update_heap(state, addr, v_f, v_v)`:
`state` with field `v_f` of the object at `addr` set to `v_v`

```spectec
def $update_heap(state, addr, v_f, v_v) = $bind_heap(state, addr, $update_obj(obj, v_f, v_v))
 -- if obj = $find_heap(state, addr)
```

1. Let `obj` be [**!**](index.md#option_get) [the object at `addr` in the heap of `state`](abstract-operations.md#find_heap).
1. Return [`state` with `obj` with field `v_f` set to `v_v` stored at `addr`](abstract-operations.md#bind_heap).

### $push_heap

<a id="push_heap"></a>
`$push_heap(state, addr, v, bool)`:
`state` with `v` added to the object at `addr` at its front if `bool` or its end otherwise

```spectec
def $push_heap(state, addr, v, bool) = $bind_heap(state, addr, $push_obj(obj, v, bool))
 -- if obj = $find_heap(state, addr)
```

1. Let `obj` be [**!**](index.md#option_get) [the object at `addr` in the heap of `state`](abstract-operations.md#find_heap).
1. Return [`state` with `obj` with `v` added at its front if `bool` or its end otherwise stored at `addr`](abstract-operations.md#bind_heap).

### $pop_heap

<a id="pop_heap"></a>
`$pop_heap(state, addr, bool)`:
The state after removing an end element of `addr` and that element

```spectec
def $pop_heap(state, addr, bool) = ($bind_heap(state, addr, obj_new), value)
 -- if obj = $find_heap(state, addr)
 -- if (obj_new, value) = $pop_obj(obj, bool)
```

1. Let `obj` be [**!**](index.md#option_get) [the object at `addr` in the heap of `state`](abstract-operations.md#find_heap).
1. Let `( obj_new, value )` be [`obj` less its first element if `bool` or its last otherwise, and that element](abstract-operations.md#pop_obj).
1. Return ( [`state` with `obj_new` stored at `addr`](abstract-operations.md#bind_heap), `value` ).

## 타입 이름

### $type_of

<a id="type_of"></a>
`$type_of(state, value)`:
The type name of `value`

```spectec
def $type_of(state, NumberV double) = "Number"

def $type_of(state, BigIntV int) = "BigInt"

def $type_of(state, StringV t) = "String"

def $type_of(state, BoolV b) = "Boolean"

def $type_of(state, UndefV) = "Undefined"

def $type_of(state, NullV) = "Null"

def $type_of(state, AddrV a) = "Object"
 -- if $find_heap(state, a) = RecordObj t_name mtv
 -- if $is_subty_record(t_name, "Object")

def $type_of(state, AddrV a) = "Symbol"
 -- if $find_heap(state, a) = RecordObj t_name mtv
 -- if $is_subty_record(t_name, "Symbol")

def $type_of(state, AddrV a) = "SpecType"
 -- if $find_heap(state, a) = RecordObj t_name mtv

def $type_of(state, AddrV a) = "SpecType"
 -- if $find_heap(state, a) = MapObj mvv

def $type_of(state, AddrV a) = "SpecType"
 -- if $find_heap(state, a) = ListObj v*

def $type_of(state, MathV bigDecimal) = "SpecType"

def $type_of(state, InfinityV b) = "SpecType"

def $type_of(state, CodeUnitV int) = "SpecType"

def $type_of(state, EnumV t) = "SpecType"

def $type_of(state, AstV ast) = "SpecType"

def $type_of(state, GrammarSymbolV t b*) = "SpecType"

def $type_of(state, CloV callable) = "SpecType"

def $type_of(state, IntV int) = "SpecType"
```

1. If let **`NumberV`** `double` be `value`:
   1. Return `"Number"`.
1. Else if let **`BigIntV`** `int` be `value`:
   1. Return `"BigInt"`.
1. Else if let **`StringV`** `t` be `value`:
   1. Return `"String"`.
1. Else if let **`BoolV`** `b` be `value`:
   1. Return `"Boolean"`.
1. Else if `value` matches pattern **`UndefV`**:
   1. Return `"Undefined"`.
1. Else if `value` matches pattern **`NullV`**:
   1. Return `"Null"`.
1. Else if let **`AddrV`** `a` be `value`:
   1. Let `obj` be [**!**](index.md#option_get) [the object at `a` in the heap of `state`](abstract-operations.md#find_heap).
   1. If let **`RecordObj`** `t_name` `mtv` be `obj`:
      1. Try:
         1. 
            1. Check that [the record type `t_name` is `"Object"` or a subtype of it](builtins.md#is_subty_record).
            1. Return `"Object"`.
         1. 
            1. Check that [the record type `t_name` is `"Symbol"` or a subtype of it](builtins.md#is_subty_record).
            1. Return `"Symbol"`.
      1. Return `"SpecType"`.
   1. Else if let **`MapObj`** `mvv` be `obj`:
      1. Return `"SpecType"`.
   1. Else if let **`ListObj`** `v*` be `obj`:
      1. Return `"SpecType"`.
1. Else if let **`MathV`** `bigDecimal` be `value`:
   1. Return `"SpecType"`.
1. Else if let **`InfinityV`** `b` be `value`:
   1. Return `"SpecType"`.
1. Else if let **`CodeUnitV`** `int` be `value`:
   1. Return `"SpecType"`.
1. Else if let **`EnumV`** `t` be `value`:
   1. Return `"SpecType"`.
1. Else if let **`AstV`** `ast` be `value`:
   1. Return `"SpecType"`.
1. Else if let **`GrammarSymbolV`** `t` `b*` be `value`:
   1. Return `"SpecType"`.
1. Else if let **`CloV`** `callable` be `value`:
   1. Return `"SpecType"`.
1. Else if let **`IntV`** `int` be `value`:
   1. Return `"SpecType"`.

## 공백 다듬기

### $white_space_cps

<a id="white_space_cps"></a>
`$white_space_cps`:
The white space code points

```spectec
def $white_space_cps = [0x9, 0xB, 0xC, 0xFEFF, 0x20, 0xA0, 0x1680, 0x2000, 0x2001, 0x2002, 0x2003, 0x2004, 0x2005, 0x2006, 0x2007, 0x2008, 0x2009, 0x200A, 0x202F, 0x205F, 0x3000]
```

1. Return `[ 9, 11, 12, 65279, 32, 160, 5760, 8192, 8193, 8194, 8195, 8196, 8197, 8198, 8199, 8200, 8201, 8202, 8239, 8287, 12288 ]`.

### $line_terminator_cps

<a id="line_terminator_cps"></a>
`$line_terminator_cps`:
The line terminator code points

```spectec
def $line_terminator_cps = [0xA, 0xD, 0x2028, 0x2029]
```

1. Return `[ 10, 13, 8232, 8233 ]`.

### $is_trimmable

<a id="is_trimmable"></a>
`$is_trimmable(nat)`:
`nat` is white space or a line terminator

```spectec
def $is_trimmable(nat) = (nat <- $white_space_cps) \/ (nat <- $line_terminator_cps)
```

1. Return `nat` is in [the white space code points](abstract-operations.md#white_space_cps) or `nat` is in [the line terminator code points](abstract-operations.md#line_terminator_cps).

### $trim_start_cps

<a id="trim_start_cps"></a>
`$trim_start_cps(nat*)`:
`nat*` without its longest trimmable prefix

```spectec
def $trim_start_cps(eps) = eps

def $trim_start_cps(nat_h :: nat_t*) = $trim_start_cps(nat_t*)
 -- if $is_trimmable(nat_h)

def $trim_start_cps(nat_h :: nat_t*) = nat_h :: nat_t*
 -- otherwise
```

1. If `nat*` matches pattern `[]`:
   1. Return `·`.
1. Else if let `nat_h :: nat_t*` be `nat*`:
   1. Check that [`nat_h` is white space or a line terminator](abstract-operations.md#is_trimmable).
   1. Return [`nat_t*` without its longest trimmable prefix](abstract-operations.md#trim_start_cps).

1. Otherwise:
   1. Otherwise:
      1. Check that `nat*` is a non-empty list.
      1. Let `nat_h :: nat_t*` be `nat*`.
      1. Return `nat_h :: nat_t*`.

### $trim_end_cps

<a id="trim_end_cps"></a>
`$trim_end_cps(nat*)`:
`nat*` without its longest trimmable suffix

```spectec
def $trim_end_cps(nat*) = $rev_<nat>($trim_start_cps($rev_<nat>(nat*)))
```

1. Return [the reversal of the reversal of `nat*` without its longest trimmable prefix](builtins.md#rev_).

### $trim_string

<a id="trim_string"></a>
`$trim_string(text, bool)`:
`text` with its leading run trimmed if `bool` or its trailing run otherwise

```spectec
def $trim_string(text, true) = $text_of_cps($trim_start_cps($cps_of_text(text)))

def $trim_string(text, false) = $text_of_cps($trim_end_cps($cps_of_text(text)))
```

1. If `bool` is equal to `true`:
   1. Return [the string formed from the code points the code points of `text` without its longest trimmable prefix](builtins.md#text_of_cps).
1. Else if `bool` is equal to `false`:
   1. Return [the string formed from the code points the code points of `text` without its longest trimmable suffix](builtins.md#text_of_cps).

## AST 연산

### $read_ast

<a id="read_ast"></a>
`$read_ast(ast''', value)`:
The child of `ast'''` at `value`

```spectec
def $read_ast(AST _ _ ast _, StringV t) = ast

def $read_ast(AST _ _ _ (SYN astSyntactic), StringV t) = astSyntactic.CHILDREN[n_rhsidx]
 -- if n_rhsidx = $ast_production_idx(astSyntactic.NAME, astSyntactic.RHSIDX, t)

def $read_ast(AST _ _ _ (SYN astSyntactic), MathV bigDecimal) = astSyntactic.CHILDREN[n_rhsidx]
 -- if n_rhsidx = $as_int(MathV bigDecimal)

def $read_ast(ast, value) = eps
 -- otherwise
```

1. Let **`AST`** `_` `_` `ast'?` `_` be `ast'''`.
1. Try:
   1. 
      1. [Let!<sub>type</sub>](index.md#check_let) `ast` be `ast'?`.
      1. [Let!<sub>type</sub>](index.md#check_let) **`StringV`** `t` be `value`.
      1. Return `ast`.
   1. 
      1. [Let!<sub>type</sub>](index.md#check_let) **`SYN`** `astSyntactic` be `_`.
      1. If let **`StringV`** `t` be `value`:
         1. Let `n_rhsidx` be [**!**](index.md#option_get) [the index of the child named `t` in production `astSyntactic.NAME` alternative `astSyntactic.RHSIDX`](builtins.md#ast_production_idx).
         1. Return `astSyntactic.CHILDREN[n_rhsidx]`.
      1. Else if let **`MathV`** `bigDecimal` be `value`:
         1. Let `int` be [the integer denoted by **`MathV`** `bigDecimal`](abstract-operations.md#as_int).
         1. [Let!<sub>type</sub>](index.md#check_let) `n_rhsidx` be `int`.
         1. Return `astSyntactic.CHILDREN[n_rhsidx]`.

1. Otherwise:
   1. Otherwise:
      1. Return `·`.

### $ast_name

<a id="ast_name"></a>
`$ast_name(AST _ _ _? astValue)`:
The production name of **`AST`** `_` `_` `_?` `astValue`

```spectec
def $ast_name(AST _ _ _ (SYN astSyntactic)) = astSyntactic.NAME

def $ast_name(AST _ _ _ (LEX astLexical)) = astLexical.NAME
```

1. If let **`SYN`** `astSyntactic` be `astValue`:
   1. Return `astSyntactic.NAME`.
1. Else if let **`LEX`** `astLexical` be `astValue`:
   1. Return `astLexical.NAME`.

### $ast_idx

<a id="ast_idx"></a>
`$ast_idx(AST _ _ _? astValue)`:
The alternative index of **`AST`** `_` `_` `_?` `astValue`

```spectec
def $ast_idx(AST _ _ _ (SYN astSyntactic)) = astSyntactic.RHSIDX

def $ast_idx(AST _ _ _ (LEX astLexical)) = 0
```

1. If let **`SYN`** `astSyntactic` be `astValue`:
   1. Return `astSyntactic.RHSIDX`.
1. Else if let **`LEX`** `astLexical` be `astValue`:
   1. Return `0`.

### $ast_subidx

<a id="ast_subidx"></a>
`$ast_subidx(AST _ _ _? astValue)`:
The sub-alternative index of **`AST`** `_` `_` `_?` `astValue`

```spectec
def $ast_subidx(AST _ _ _ (SYN astSyntactic)) = astSyntactic.SUBIDX

def $ast_subidx(AST _ _ _ (LEX astLexical)) = 0
```

1. If let **`SYN`** `astSyntactic` be `astValue`:
   1. Return `astSyntactic.SUBIDX`.
1. Else if let **`LEX`** `astLexical` be `astValue`:
   1. Return `0`.

### $only_child

<a id="only_child"></a>
`$only_child(ast'')`:
The sole present child of `ast''`

```spectec
def $only_child(AST _ _ _ (SYN astSyntactic)) = ast_1
 -- if $filter_some_<ast>(astSyntactic.CHILDREN) = ast_1 :: eps

def $only_child(ast) = eps
 -- otherwise
```

1. Let **`AST`** `_` `_` `_?` `astValue` be `ast''`.
1. [Let!<sub>type</sub>](index.md#check_let) **`SYN`** `astSyntactic` be `astValue`.
1. Let `ast'*` be [the defined elements of `astSyntactic.CHILDREN`](abstract-operations.md#filter_some_).
1. [Let!<sub>type</sub>](index.md#check_let) `ast_1 :: ast*` be `ast'*`.
1. Check that `ast*` is an empty list.
1. Return `ast_1`.

1. Otherwise:
   1. Otherwise:
      1. Return `·`.

### $ast_chain

<a id="ast_chain"></a>
`$ast_chain(ast)`:
The chain of sole children descending from `ast`

```spectec
def $ast_chain(ast) = ast :: $ast_chain(ast_1)
 -- if $only_child(ast) = ast_1

def $ast_chain(ast) = ast :: eps
 -- otherwise
```

1. Let `ast_1` be [**!**](index.md#option_get) [the sole present child of `ast`](abstract-operations.md#only_child).
1. Return `ast :: $ast_chain(ast_1)`.

1. Otherwise:
   1. Otherwise:
      1. Return `ast :: ·`.

### $sdo_name

<a id="sdo_name"></a>
`$sdo_name(ast, t)`:
The SDO name of `t` on `ast`

```spectec
def $sdo_name(ast, t) = $ast_name(ast) ++ "[" ++ $int_to_text($ast_idx(ast)) ++ "," ++ $int_to_text($ast_subidx(ast)) ++ "]." ++ t
```

1. Return [the production name of `ast`](abstract-operations.md#ast_name) concatenated with `"["` concatenated with [the decimal notation of the alternative index of `ast`](builtins.md#int_to_text) concatenated with `","` concatenated with [the decimal notation of the sub-alternative index of `ast`](builtins.md#int_to_text) concatenated with `"]."` concatenated with `t`.

### $find_sdo

<a id="find_sdo"></a>
`$find_sdo(state, ast''*, t)`:
The node in `ast''*` carrying operation `t` and that operation

```spectec
def $find_sdo(state, eps, t) = eps

def $find_sdo(state, ast_1 :: ast*, t) = (ast_1, func)
 -- if $find_func(state, $sdo_name(ast_1, t)) = func

def $find_sdo(state, ast_1 :: ast*, t) = (ast_1, func)
 -- if $find_func(state, "DEFAULT:" ++ t) = func

def $find_sdo(state, ast_1 :: ast*, t) = $find_sdo(state, ast*, t)
 -- otherwise
```

1. If `ast''*` matches pattern `[]`:
   1. Return `·`.
1. Else if let `ast_1 :: ast*` be `ast''*`:
   1. Let `func` be [**!**](index.md#option_get) [the function named the SDO name of `t` on `ast_1` in `state`](abstract-operations.md#find_func).
   1. Return ( `ast_1`, `func` ).
   1. Let `func` be [**!**](index.md#option_get) [the function named `"DEFAULT:"` concatenated with `t` in `state`](abstract-operations.md#find_func).
   1. Return ( `ast_1`, `func` ).

1. Otherwise:
   1. Otherwise:
      1. Check that `ast''*` is a non-empty list.
      1. Let `ast_1 :: ast*` be `ast''*`.
      1. Return [the node in `ast*` carrying operation `t` and that operation](abstract-operations.md#find_sdo).

### $instance_of

<a id="instance_of"></a>
`$instance_of(value'', value''')`:
`value''` is an instance of `value'''`

```spectec
def $instance_of(AstV (AST _ _ _ (SYN astSyntactic)), GrammarSymbolV "" b*) = true

def $instance_of(AstV ast, GrammarSymbolV t b*) = t = $ast_name(ast)

def $instance_of(v_1, v_2) = false
 -- otherwise
```

1. [Let!<sub>type</sub>](index.md#check_let) **`AstV`** `AST _ _ _? astValue` be `value''`.
1. [Let!<sub>type</sub>](index.md#check_let) **`SYN`** `astSyntactic` be `astValue`.
1. [Let!<sub>type</sub>](index.md#check_let) **`GrammarSymbolV`** `text` `b*` be `value'''`.
1. Check that `text` is equal to `""`. return `true`.
1. Let **`AstV`** `ast` be `value''`.
1. [Let!<sub>type</sub>](index.md#check_let) **`GrammarSymbolV`** `t` `b*` be `value'''`.
1. Return `t` is equal to [the production name of `ast`](abstract-operations.md#ast_name).

1. Otherwise:
   1. Otherwise:
      1. Return `false`.

### $size_of_ast

<a id="size_of_ast"></a>
`$size_of_ast(AST _ _ _? astValue)`:
The number of children of **`AST`** `_` `_` `_?` `astValue`

```spectec
def $size_of_ast(AST _ _ _ (SYN astSyntactic)) = |astSyntactic.CHILDREN|

def $size_of_ast(AST _ _ _ (LEX astLexical)) = |astLexical.CHILDREN|
```

1. If let **`SYN`** `astSyntactic` be `astValue`:
   1. Return `the length of astSyntactic.CHILDREN`.
1. Else if let **`LEX`** `astLexical` be `astValue`:
   1. Return `the length of astLexical.CHILDREN`.

### $opt_presence

<a id="opt_presence"></a>
`$opt_presence(bool''*, ast'''?*)`:
Which of the optional positions `bool''*` are filled in `ast'''?*`

```spectec
def $opt_presence(eps, (ast?)*) = eps

def $opt_presence(b_h :: b_t*, eps) = eps

def $opt_presence(true :: b_t*, eps :: (ast?)*) = false :: $opt_presence(b_t*, (ast?)*)

def $opt_presence(true :: b_t*, ast_1 :: (ast?)*) = true :: $opt_presence(b_t*, (ast?)*)

def $opt_presence(false :: b_t*, eps :: (ast?)*) = $opt_presence(b_t*, (ast?)*)

def $opt_presence(false :: b_t*, ast_1 :: (ast?)*) = $opt_presence(b_t*, (ast?)*)
```

1. Let `ast?` be `ast'''?`, for all `ast'''?` in `ast'''?*` and `ast?` in `ast?*`.
1. If `bool''*` matches pattern `[]`:
   1. Return `·`.
1. Else if let `b_h :: b_t*` be `bool''*`:
   1. Try:
      1. 
         1. Check that `ast?*` is an empty list.
         1. Return `·`.
      1. 
         1. If `b_h` is equal to `true`:
            1. [Let!<sub>type</sub>](index.md#check_let) `ast'? :: ast?*` be `ast?*`.
            1. If `ast'?` matches pattern `()`:
               1. Return `false :: $opt_presence(b_t*, ast?*)`.
            1. Else if let `ast_1` be `ast'?`:
               1. Return `true :: $opt_presence(b_t*, ast?*)`.
         1. Else if `b_h` is equal to `false`:
            1. [Let!<sub>type</sub>](index.md#check_let) `ast'? :: ast?*` be `ast?*`.
            1. If `ast'?` matches pattern `()`:
               1. Return [which of the optional positions `b_t*` are filled in `ast?*`](abstract-operations.md#opt_presence).
            1. Else if let `ast_1` be `ast'?`:
               1. Return [which of the optional positions `b_t*` are filled in `ast?*`](abstract-operations.md#opt_presence).

### $pow2

<a id="pow2"></a>
`$pow2(nat')`:
Two raised to the power of `nat'`

```spectec
def $pow2(0) = 1

def $pow2(n) = 2 * $pow2(n')
 -- if n =/= 0
 -- if n' = n - 1
```

1. If `nat'` is equal to `0`:
   1. Return `1`.
1. Else if `nat'` is not equal to `0`:
   1. Let `n'` be `nat'` - `1`.
   1. Return `2` * [two raised to the power of `n'`](abstract-operations.md#pow2).

### $pack_bits

<a id="pack_bits"></a>
`$pack_bits(bool''*)`:
The bits `bool''*` packed into a number, most significant first

```spectec
def $pack_bits(eps) = 0

def $pack_bits(true :: b_t*) = $pow2(|b_t*|) + $pack_bits(b_t*)

def $pack_bits(false :: b_t*) = $pack_bits(b_t*)
```

1. If `bool''*` matches pattern `[]`:
   1. Return `0`.
1. Else if let `bool :: b_t*` be `bool''*`:
   1. If `bool` is equal to `true`:
      1. Return [two raised to the power of the length of `b_t*`](abstract-operations.md#pow2) + [the bits `b_t*` packed into a number, most significant first](abstract-operations.md#pack_bits).
   1. Else if `bool` is equal to `false`:
      1. Return [the bits `b_t*` packed into a number, most significant first](abstract-operations.md#pack_bits).

### $sub_idx

<a id="sub_idx"></a>
`$sub_idx(id, n, ast?*)`:
The sub-alternative index of production `id` alternative `n` over children `ast?*`

```spectec
def $sub_idx(id, n, (ast?)*) = $pack_bits($opt_presence($ast_optionals(id, n), (ast?)*))
```

1. Return [the bits which of the optional positions which nonterminals of production `id` alternative `n` are optional are filled in `ast?*` packed into a number, most significant first](abstract-operations.md#pack_bits).

### $init_syntactic

<a id="init_syntactic"></a>
`$init_syntactic(id, bool*, nat, ast?*)`:
A syntactic node for production `id` over children `ast?*`

```spectec
def $init_syntactic(id, bool*, nat, (ast?)*) = SYN {NAME id, ARGS bool*, RHSIDX nat, SUBIDX $sub_idx(id, nat, (ast?)*), CHILDREN (ast?)*, PARENT eps}
```

1. Return **`SYN`** `{NAME id, ARGS bool*, RHSIDX nat, SUBIDX $sub_idx(id, nat, ast?*), CHILDREN ast?*, PARENT ·}`.

### $init_lexical

<a id="init_lexical"></a>
`$init_lexical(id, text)`:
A lexical node for production `id` over `text`

```spectec
def $init_lexical(id, text) = LEX {NAME id, STR text, CHILDREN eps, PARENT eps}
```

1. Return **`LEX`** `{NAME id, STR text, CHILDREN ·, PARENT ·}`.

### $reinsert_holes

<a id="reinsert_holes"></a>
`$reinsert_holes(expr'''?*, value'''*)`:
The values `value'''*` put back into the holes of `expr'''?*`

```spectec
def $reinsert_holes(eps, value*) = eps

def $reinsert_holes(eps :: (expr?)*, value*) = eps :: $reinsert_holes((expr?)*, value*)

def $reinsert_holes(expr_1 :: (expr?)*, (AstV ast) :: value*) = ast :: $reinsert_holes((expr?)*, value*)
```

1. Let `expr?` be `expr'''?`, for all `expr'''?` in `expr'''?*` and `expr?` in `expr?*`.
1. If `expr?*` matches pattern `[]`:
   1. Return `·`.
1. Else if let `expr'? :: expr?*` be `expr?*`:
   1. If `expr'?` matches pattern `()`:
      1. Return `· :: $reinsert_holes(expr?*, value'''*)`.
   1. Else if let `expr_1` be `expr'?`:
      1. [Let!<sub>type</sub>](index.md#check_let) `value' :: value*` be `value'''*`.
      1. [Let!<sub>type</sub>](index.md#check_let) **`AstV`** `ast` be `value'`.
      1. Return `ast :: $reinsert_holes(expr?*, value*)`.

## 타입 검사

### $has_fields

<a id="has_fields"></a>
`$has_fields(mtv, text'*)`:
`mtv` has every field in `text'*`

```spectec
def $has_fields(mtv, eps) = true

def $has_fields(mtv, t_h :: t_t*) = $has_fields(mtv, t_t*)
 -- if $is_some_<value>($find_map<text, value>(mtv, t_h))

def $has_fields(mtv, t*) = false
```

1. If `text'*` matches pattern `[]`: return `true`.
1. Else if let `t_h :: t_t*` be `text'*`:
   1. Check that [the value of `t_h` in map `mtv` is defined](abstract-operations.md#is_some_).
   1. Return [`mtv` has every field in `t_t*`](abstract-operations.md#has_fields).
1. Return `false`.

### $is_math_int

<a id="is_math_int"></a>
`$is_math_int(bigDecimal')`:
`bigDecimal'` denotes an integer

```spectec
def $is_math_int(BigDec int_u int_s) = true
 -- if int_s <= 0

def $is_math_int(BigDec int_u int_s) = true
 -- if int_s = nat_s
 -- if int_u \ $pow10(nat_s) = 0

def $is_math_int(bigDecimal) = false
```

1. Let **`BigDec`** `int_u` `int_s` be `bigDecimal'`.
1. Try:
   1. 
      1. Check that `int_s` is less than or equal to `0`. return `true`.
   1. 
      1. [Let!<sub>type</sub>](index.md#check_let) `nat_s` be `int_s`.
      1. Check that `int_u` \ [10 raised to the power of `nat_s`](builtins.md#pow10) is equal to `0`. return `true`.
1. Return `false`.

### $type_contains

<a id="type_contains"></a>
`$type_contains(state, type'', value)`:
`value` is of type `type''`

```spectec
def $type_contains(state, RecordT "" t_field*, AddrV a) = $has_fields(mtv, t_field*)
 -- if $find_heap(state, a) = RecordObj t_obj mtv

def $type_contains(state, RecordT t_name t_field*, AddrV a) = $has_fields(mtv, t_field*)
 -- if $find_heap(state, a) = RecordObj t_obj mtv
 -- if $is_subty_record(t_obj, t_name)

def $type_contains(state, RecordT t_name t_field*, value) = false

def $type_contains(state, CompletionT id, AddrV a) = true
 -- if $find_heap(state, a) = RecordObj t_obj mtv
 -- if $is_subty_record(t_obj, $completion_record(id))

def $type_contains(state, CompletionT id, value) = false

def $type_contains(state, AstT eps, AstV ast) = true

def $type_contains(state, AstT t*, AstV ast) = $ast_name(ast) <- t*

def $type_contains(state, AstT t*, value) = false

def $type_contains(state, ListT type, AddrV a) = $all_type_contains(state, type, v*)
 -- if $find_heap(state, a) = ListObj v*

def $type_contains(state, ListT type, value) = false

def $type_contains(state, EnumT id*, EnumV id_v) = id_v <- id*

def $type_contains(state, EnumT id*, value) = false

def $type_contains(state, PrimT StringT, StringV t) = true

def $type_contains(state, PrimT NumberT, NumberV double) = true

def $type_contains(state, PrimT NumberIntT, NumberV (FIN sign nat int)) = $is_math_int($bigdec_of_double(FIN sign nat int))

def $type_contains(state, PrimT BigIntT, BigIntV int) = true

def $type_contains(state, PrimT BoolT, BoolV b) = true

def $type_contains(state, PrimT MathT, MathV bigDecimal) = true

def $type_contains(state, PrimT IntT, MathV bigDecimal) = $is_math_int(bigDecimal)

def $type_contains(state, PrimT CodeUnitT, CodeUnitV int) = true

def $type_contains(state, PrimT UndefinedT, UndefV) = true

def $type_contains(state, PrimT NullT, NullV) = true

def $type_contains(state, PrimT primType, value) = false

def $type_contains(state, UnionT type_1 type_2, value) = $type_contains(state, type_1, value) \/ $type_contains(state, type_2, value)
```

1. Try:
   1. 
      1. If let **`RecordT`** `text` `t_field*` be `type''`:
         1. Try:
            1. 
               1. Check that `text` is equal to `""`.
               1. [Let!<sub>type</sub>](index.md#check_let) **`AddrV`** `a` be `value`.
               1. Let `obj` be [**!**](index.md#option_get) [the object at `a` in the heap of `state`](abstract-operations.md#find_heap).
               1. [Let!<sub>type</sub>](index.md#check_let) **`RecordObj`** `t_obj` `mtv` be `obj`.
               1. Return [`mtv` has every field in `t_field*`](abstract-operations.md#has_fields).
            1. 
               1. [Let!<sub>type</sub>](index.md#check_let) **`AddrV`** `a` be `value`.
               1. Let `obj` be [**!**](index.md#option_get) [the object at `a` in the heap of `state`](abstract-operations.md#find_heap).
               1. [Let!<sub>type</sub>](index.md#check_let) **`RecordObj`** `t_obj` `mtv` be `obj`.
               1. Check that [the record type `t_obj` is `text` or a subtype of it](builtins.md#is_subty_record).
               1. Return [`mtv` has every field in `t_field*`](abstract-operations.md#has_fields).
         1. Return `false`.
      1. Else if let **`CompletionT`** `id` be `type''`:
         1. [Let!<sub>type</sub>](index.md#check_let) **`AddrV`** `a` be `value`.
         1. Let `obj` be [**!**](index.md#option_get) [the object at `a` in the heap of `state`](abstract-operations.md#find_heap).
         1. [Let!<sub>type</sub>](index.md#check_let) **`RecordObj`** `t_obj` `mtv` be `obj`.
         1. Check that [the record type `t_obj` is the record type named by completion type `id` or a subtype of it](builtins.md#is_subty_record). return `true`.
         1. Return `false`.
      1. Else if let **`AstT`** `t*` be `type''`:
         1. [Let!<sub>type</sub>](index.md#check_let) **`AstV`** `ast` be `value`.
         1. Return [the production name of `ast`](abstract-operations.md#ast_name) is in `t*`.
         1. Return `false`.
      1. Else if let **`ListT`** `type` be `type''`:
         1. [Let!<sub>type</sub>](index.md#check_let) **`AddrV`** `a` be `value`.
         1. Let `obj` be [**!**](index.md#option_get) [the object at `a` in the heap of `state`](abstract-operations.md#find_heap).
         1. [Let!<sub>type</sub>](index.md#check_let) **`ListObj`** `v*` be `obj`.
         1. Return [every value in `v*` is of type `type`](abstract-operations.md#all_type_contains).
         1. Return `false`.
      1. Else if let **`EnumT`** `id*` be `type''`:
         1. [Let!<sub>type</sub>](index.md#check_let) **`EnumV`** `id_v` be `value`.
         1. Return `id_v` is in `id*`.
         1. Return `false`.
      1. Else if let **`PrimT`** `primType` be `type''`: return `false`.
      1. Else if let **`UnionT`** `type_1` `type_2` be `type''`:
         1. Return [`value` is of type `type_1`](abstract-operations.md#type_contains) or [`value` is of type `type_2`](abstract-operations.md#type_contains).
   1. 
      1. Check that `type''` is equal to **`AstT`** `·`.
      1. [Let!<sub>type</sub>](index.md#check_let) **`AstV`** `ast` be `value`. return `true`.
   1. 
      1. Check that `type''` is equal to **`PrimT`** `StringT`.
      1. [Let!<sub>type</sub>](index.md#check_let) **`StringV`** `t` be `value`. return `true`.
   1. 
      1. Check that `type''` is equal to **`PrimT`** `NumberT`.
      1. [Let!<sub>type</sub>](index.md#check_let) **`NumberV`** `double` be `value`. return `true`.
   1. 
      1. Check that `type''` is equal to **`PrimT`** `NumberIntT`.
      1. [Let!<sub>type</sub>](index.md#check_let) **`NumberV`** `double` be `value`.
      1. [Let!<sub>type</sub>](index.md#check_let) **`FIN`** `sign` `nat` `int` be `double`.
      1. Return [**`FIN`** `sign` `nat` `int` as a mathematical value denotes an integer](abstract-operations.md#is_math_int).
   1. 
      1. Check that `type''` is equal to **`PrimT`** `BigIntT`.
      1. [Let!<sub>type</sub>](index.md#check_let) **`BigIntV`** `int` be `value`. return `true`.
   1. 
      1. Check that `type''` is equal to **`PrimT`** `BoolT`.
      1. [Let!<sub>type</sub>](index.md#check_let) **`BoolV`** `b` be `value`. return `true`.
   1. 
      1. Check that `type''` is equal to **`PrimT`** `MathT`.
      1. [Let!<sub>type</sub>](index.md#check_let) **`MathV`** `bigDecimal` be `value`. return `true`.
   1. 
      1. Check that `type''` is equal to **`PrimT`** `IntT`.
      1. [Let!<sub>type</sub>](index.md#check_let) **`MathV`** `bigDecimal` be `value`.
      1. Return [`bigDecimal` denotes an integer](abstract-operations.md#is_math_int).
   1. 
      1. Check that `type''` is equal to **`PrimT`** `CodeUnitT`.
      1. [Let!<sub>type</sub>](index.md#check_let) **`CodeUnitV`** `int` be `value`. return `true`.
   1. 
      1. Check that `type''` is equal to **`PrimT`** `UndefinedT`.
      1. Check that `value` matches pattern **`UndefV`**. return `true`.
   1. 
      1. Check that `type''` is equal to **`PrimT`** `NullT`.
      1. Check that `value` matches pattern **`NullV`**. return `true`.

### $all_type_contains

<a id="all_type_contains"></a>
`$all_type_contains(state, type, value*)`:
Every value in `value*` is of type `type`

```spectec
def $all_type_contains(state, type, eps) = true

def $all_type_contains(state, type, v_h :: v_t*) = $all_type_contains(state, type, v_t*)
 -- if $type_contains(state, type, v_h) = true

def $all_type_contains(state, type, value*) = false
```

1. If `value*` matches pattern `[]`: return `true`.
1. Else if let `v_h :: v_t*` be `value*`:
   1. Check that [`v_h` is of type `type`](abstract-operations.md#type_contains) is equal to `true`.
   1. Return [every value in `v_t*` is of type `type`](abstract-operations.md#all_type_contains).
1. Return `false`.

### $completion_record

<a id="completion_record"></a>
`$completion_record(text)`:
The record type named by completion type `text`

```spectec
def $completion_record("Completion") = "CompletionRecord"

def $completion_record("Normal") = "NormalCompletion"

def $completion_record("Abrupt") = "AbruptCompletion"

def $completion_record("Break") = "BreakCompletion"

def $completion_record("Continue") = "ContinueCompletion"

def $completion_record("Return") = "ReturnCompletion"

def $completion_record("Throw") = "ThrowCompletion"
```

1. If `text` is equal to `"Completion"`:
   1. Return `"CompletionRecord"`.
1. Else if `text` is equal to `"Normal"`:
   1. Return `"NormalCompletion"`.
1. Else if `text` is equal to `"Abrupt"`:
   1. Return `"AbruptCompletion"`.
1. Else if `text` is equal to `"Break"`:
   1. Return `"BreakCompletion"`.
1. Else if `text` is equal to `"Continue"`:
   1. Return `"ContinueCompletion"`.
1. Else if `text` is equal to `"Return"`:
   1. Return `"ReturnCompletion"`.
1. Else if `text` is equal to `"Throw"`:
   1. Return `"ThrowCompletion"`.

## 참조 연산

### $find_var

<a id="find_var"></a>
`$find_var(state, variable)`:
The value of `variable` in `state`

```spectec
def $find_var(state, global) = $find_global(state, global)

def $find_var(state, local) = $find_local(state, local)
```

1. If let `global` be `variable`:
   1. Return [the value of global `global` in `state`](abstract-operations.md#find_global).
1. Else if let `local` be `variable`:
   1. Return [the value of local `local` in `state`](abstract-operations.md#find_local).

### $read_ref

<a id="read_ref"></a>
`$read_ref(state, refTarget)`:
The value `refTarget` reads to in state `state`

```spectec
def $read_ref(state, VarTarget variable) = v
 -- if $find_var(state, variable) = v

def $read_ref(state, FieldTarget (AddrV a) v_field) = v
 -- if $find_heap(state, a) = o
 -- if $read_obj(o, v_field) = v

def $read_ref(state, FieldTarget (AstV ast) v_field) = AstV ast_1
 -- if $read_ast(ast, v_field) = ast_1

def $read_ref(state, FieldTarget (StringV t) (MathV bigDecimal)) = CodeUnitV n_cp*[n]
 -- if n_cp* = $code_units_of_text(t)
 -- if n = $as_int(MathV bigDecimal)
```

1. If let **`VarTarget`** `variable` be `refTarget`:
   1. Let `v` be [**!**](index.md#option_get) [the value of `variable` in `state`](abstract-operations.md#find_var).
   1. Return `v`.
1. Else if let **`FieldTarget`** `value` `v_field` be `refTarget`:
   1. If let **`AddrV`** `a` be `value`:
      1. Let `o` be [**!**](index.md#option_get) [the object at `a` in the heap of `state`](abstract-operations.md#find_heap).
      1. Let `value'?` be [the value of field `v_field` in `o`](abstract-operations.md#read_obj).
      1. [Let!<sub>type</sub>](index.md#check_let) `v` be `value'?`.
      1. Return `v`.
   1. Else if let **`AstV`** `ast` be `value`:
      1. Let `ast_1` be [**!**](index.md#option_get) [the child of `ast` at `v_field`](abstract-operations.md#read_ast).
      1. Return **`AstV`** `ast_1`.
   1. Else if let **`StringV`** `t` be `value`:
      1. [Let!<sub>type</sub>](index.md#check_let) **`MathV`** `bigDecimal` be `v_field`.
      1. Let `n_cp*` be [the UTF-16 code units of `t`](builtins.md#code_units_of_text).
      1. Let `int` be [the integer denoted by **`MathV`** `bigDecimal`](abstract-operations.md#as_int).
      1. [Let!<sub>type</sub>](index.md#check_let) `n` be `int`.
      1. Return **`CodeUnitV`** `n_cp*[n]`.

### $exists_ref

<a id="exists_ref"></a>
`$exists_ref(state, refTarget)`:
`refTarget` resolves in state `state`

```spectec
def $exists_ref(state, VarTarget variable) = $is_some_<value>($find_var(state, variable))

def $exists_ref(state, FieldTarget (AddrV addr) v_field) = $exists_obj(o, v_field)
 -- if $find_heap(state, addr) = o

def $exists_ref(state, FieldTarget (AstV ast) v_field) = $is_some_<ast>($read_ast(ast, v_field))
```

1. If let **`VarTarget`** `variable` be `refTarget`:
   1. Return [the value of `variable` in `state` is defined](abstract-operations.md#is_some_).
1. Else if let **`FieldTarget`** `value` `v_field` be `refTarget`:
   1. If let **`AddrV`** `addr` be `value`:
      1. Let `o` be [**!**](index.md#option_get) [the object at `addr` in the heap of `state`](abstract-operations.md#find_heap).
      1. Return [`o` has field `v_field`](abstract-operations.md#exists_obj).
   1. Else if let **`AstV`** `ast` be `value`:
      1. Return [the child of `ast` at `v_field` is defined](abstract-operations.md#is_some_).

## 단항 연산자

### $uop

<a id="uop"></a>
`$uop(uop, value)`:
The result of applying `uop` to `value`

```spectec
def $uop(Abs, MathV bigDecimal) = MathV $abs_bigdec(bigDecimal)

def $uop(Floor, MathV bigDecimal) = MathV $floor_bigdec(bigDecimal)

def $uop(Neg, value) = $uop_neg(value)

def $uop(Not, BoolV b) = BoolV (~b)

def $uop(BNot, value) = $uop_bnot(value)
```

1. If `uop` matches pattern **`Abs`**:
   1. [Let!<sub>type</sub>](index.md#check_let) **`MathV`** `bigDecimal` be `value`.
   1. Return **`MathV`** `$abs_bigdec(bigDecimal)`.
1. Else if `uop` matches pattern **`Floor`**:
   1. [Let!<sub>type</sub>](index.md#check_let) **`MathV`** `bigDecimal` be `value`.
   1. Return **`MathV`** `$floor_bigdec(bigDecimal)`.
1. Else if `uop` matches pattern **`Neg`**:
   1. Return [the negation of `value`](abstract-operations.md#uop_neg).
1. Else if `uop` matches pattern **`Not`**:
   1. [Let!<sub>type</sub>](index.md#check_let) **`BoolV`** `b` be `value`.
   1. Return **`BoolV`** `~b`.
1. Else if `uop` matches pattern **`BNot`**:
   1. Return [the bitwise complement of `value`](abstract-operations.md#uop_bnot).

### $abs_bigdec

<a id="abs_bigdec"></a>
`$abs_bigdec(BigDec int_u int_s)`:
The absolute value of **`BigDec`** `int_u` `int_s`

```spectec
def $abs_bigdec(BigDec int_u int_s) = BigDec int_u int_s
 -- if int_u >= 0

def $abs_bigdec(BigDec int_u int_s) = BigDec 0 - int_u int_s
 -- if int_u < 0
```

1. Try:
   1. 
      1. Check that `int_u` is greater than or equal to `0`.
      1. Return **`BigDec`** `int_u` `int_s`.
   1. 
      1. Check that `int_u` is less than `0`.
      1. Return **`BigDec`** `0 - int_u` `int_s`.

### $neg_sign

<a id="neg_sign"></a>
`$neg_sign(sign)`:
The opposite of `sign`

```spectec
def $neg_sign(POS) = NEG

def $neg_sign(NEG) = POS
```

1. If `sign` matches pattern **`POS`**:
   1. Return **`NEG`**.
1. Else if `sign` matches pattern **`NEG`**:
   1. Return **`POS`**.

### $neg_double

<a id="neg_double"></a>
`$neg_double(double)`:
The negation of `double`

```spectec
def $neg_double(FIN sign nat int) = FIN $neg_sign(sign) nat int

def $neg_double(INF sign) = INF $neg_sign(sign)

def $neg_double(NAN) = NAN
```

1. If let **`FIN`** `sign` `nat` `int` be `double`:
   1. Return **`FIN`** `$neg_sign(sign)` `nat` `int`.
1. Else if let **`INF`** `sign` be `double`:
   1. Return **`INF`** `$neg_sign(sign)`.
1. Else if `double` matches pattern **`NAN`**:
   1. Return **`NAN`**.

### $neg_bigdec

<a id="neg_bigdec"></a>
`$neg_bigdec(BigDec int_u int_s)`:
The negation of **`BigDec`** `int_u` `int_s`

```spectec
def $neg_bigdec(BigDec int_u int_s) = BigDec 0 - int_u int_s
```

1. Return **`BigDec`** `0 - int_u` `int_s`.

### $uop_neg

<a id="uop_neg"></a>
`$uop_neg(value)`:
The negation of `value`

```spectec
def $uop_neg(NumberV double) = NumberV $neg_double(double)

def $uop_neg(MathV bigDecimal) = MathV $neg_bigdec(bigDecimal)

def $uop_neg(InfinityV b) = InfinityV (~b)

def $uop_neg(BigIntV i) = BigIntV 0 - i
```

1. If let **`NumberV`** `double` be `value`:
   1. Return **`NumberV`** `$neg_double(double)`.
1. Else if let **`MathV`** `bigDecimal` be `value`:
   1. Return **`MathV`** `$neg_bigdec(bigDecimal)`.
1. Else if let **`InfinityV`** `b` be `value`:
   1. Return **`InfinityV`** `~b`.
1. Else if let **`BigIntV`** `i` be `value`:
   1. Return **`BigIntV`** `0 - i`.

### $bnot_int

<a id="bnot_int"></a>
`$bnot_int(int)`:
The bitwise complement of `int`

```spectec
def $bnot_int(int) = 0 - $as_int32(int) - 1
```

1. Return `0` - [the low 32 bits of `int` as a signed integer](builtins.md#as_int32) - `1`.

### $uop_bnot

<a id="uop_bnot"></a>
`$uop_bnot(value)`:
The bitwise complement of `value`

```spectec
def $uop_bnot(BigIntV i) = BigIntV 0 - i - 1

def $uop_bnot(MathV bigDecimal) = MathV (BigDec $bnot_int($as_int(MathV bigDecimal)) 0)

def $uop_bnot(NumberV double) = NumberV $double_of_int($bnot_int($as_int_of_double(double)))
```

1. If let **`BigIntV`** `i` be `value`:
   1. Return **`BigIntV`** `0 - i - 1`.
1. Else if let **`MathV`** `bigDecimal` be `value`:
   1. Return **`MathV`** `BigDec $bnot_int($as_int(MathV bigDecimal)) 0`.
1. Else if let **`NumberV`** `double` be `value`:
   1. Return **`NumberV`** `$double_of_int($bnot_int($as_int_of_double(double)))`.

## 이항 연산자

### $bop

<a id="bop"></a>
`$bop(bop, value'', value''')`:
The result of applying `bop` to `value''` and `value'''`

```spectec
def $bop(Add, NumberV double_l, NumberV double_r) = NumberV $add_double(double_l, double_r)

def $bop(Sub, NumberV double_l, NumberV double_r) = NumberV $sub_double(double_l, double_r)

def $bop(Mul, NumberV double_l, NumberV double_r) = NumberV $mul_double(double_l, double_r)

def $bop(Pow, NumberV double_l, NumberV double_r) = NumberV $pow_double(double_l, double_r)

def $bop(Div, NumberV double_l, NumberV double_r) = NumberV $div_double(double_l, double_r)

def $bop(Mod, NumberV double_l, NumberV double_r) = NumberV $mod_double(double_l, double_r)

def $bop(Lt, NumberV (FIN NEG 0 0), NumberV (FIN POS 0 0)) = BoolV true

def $bop(Lt, NumberV double_l, NumberV double_r) = BoolV $lt_double(double_l, double_r)

def $bop(Add, MathV bigDecimal_l, MathV bigDecimal_r) = MathV $add_bigdec(bigDecimal_l, bigDecimal_r)

def $bop(Sub, MathV bigDecimal_l, MathV bigDecimal_r) = MathV $sub_bigdec(bigDecimal_l, bigDecimal_r)

def $bop(Mul, MathV bigDecimal_l, MathV bigDecimal_r) = MathV $mul_bigdec(bigDecimal_l, bigDecimal_r)

def $bop(Div, MathV bigDecimal_l, MathV bigDecimal_r) = MathV $div_bigdec(bigDecimal_l, bigDecimal_r)

def $bop(Mod, MathV bigDecimal_l, MathV bigDecimal_r) = MathV $mod_bigdec(bigDecimal_l, bigDecimal_r)

def $bop(Pow, MathV bigDecimal_l, MathV bigDecimal_r) = MathV $pow_bigdec_nat(bigDecimal_l, nat_n)
 -- if $as_valid_nat(bigDecimal_r) = nat_n

def $bop(Pow, MathV bigDecimal_l, MathV bigDecimal_r) = MathV $pow_bigdec(bigDecimal_l, bigDecimal_r)

def $bop(BAnd, MathV bigDecimal_l, MathV bigDecimal_r) = MathV (BigDec $band($trunc_bigdec(bigDecimal_l), $trunc_bigdec(bigDecimal_r)) 0)

def $bop(BOr, MathV bigDecimal_l, MathV bigDecimal_r) = MathV (BigDec $bor($trunc_bigdec(bigDecimal_l), $trunc_bigdec(bigDecimal_r)) 0)

def $bop(BXOr, MathV bigDecimal_l, MathV bigDecimal_r) = MathV (BigDec $bxor($trunc_bigdec(bigDecimal_l), $trunc_bigdec(bigDecimal_r)) 0)

def $bop(LShift, MathV bigDecimal_l, MathV bigDecimal_r) = MathV (BigDec $shl($trunc_bigdec(bigDecimal_l), nat_n) 0)
 -- if nat_n = $trunc_bigdec(bigDecimal_r)

def $bop(RShift, MathV bigDecimal_l, MathV bigDecimal_r) = MathV (BigDec $shr($trunc_bigdec(bigDecimal_l), nat_n) 0)
 -- if nat_n = $trunc_bigdec(bigDecimal_r)

def $bop(Lt, MathV bigDecimal_l, MathV bigDecimal_r) = BoolV $lt_bigdec(bigDecimal_l, bigDecimal_r)

def $bop(Add, InfinityV b, MathV bigDecimal) = InfinityV b

def $bop(Add, MathV bigDecimal, InfinityV b) = InfinityV b

def $bop(Add, InfinityV b_l, InfinityV b_r) = InfinityV b_l
 -- if b_l = b_r

def $bop(Sub, InfinityV b, MathV bigDecimal) = InfinityV b

def $bop(Sub, MathV bigDecimal, InfinityV b) = InfinityV (~b)

def $bop(Sub, InfinityV b_l, InfinityV b_r) = InfinityV b_l
 -- if b_l =/= b_r

def $bop(Mul, InfinityV b, MathV bigDecimal) = InfinityV b
 -- if $lt_bigdec(BigDec 0 0, bigDecimal)

def $bop(Mul, InfinityV b, MathV bigDecimal) = InfinityV (~b)
 -- if $lt_bigdec(bigDecimal, BigDec 0 0)

def $bop(Mul, MathV bigDecimal, InfinityV b) = InfinityV b
 -- if $lt_bigdec(BigDec 0 0, bigDecimal)

def $bop(Mul, MathV bigDecimal, InfinityV b) = InfinityV (~b)
 -- if $lt_bigdec(bigDecimal, BigDec 0 0)

def $bop(Mul, InfinityV b_l, InfinityV b_r) = InfinityV (b_l = b_r)

def $bop(Lt, InfinityV b, MathV bigDecimal) = BoolV (~b)

def $bop(Lt, MathV bigDecimal, InfinityV b) = BoolV b

def $bop(Lt, InfinityV b_l, InfinityV b_r) = BoolV (~b_l)
 -- if b_l =/= b_r

def $bop(And, BoolV b_l, BoolV b_r) = BoolV (b_l /\ b_r)

def $bop(Or, BoolV b_l, BoolV b_r) = BoolV (b_l \/ b_r)

def $bop(Xor, BoolV b_l, BoolV b_r) = BoolV (b_l =/= b_r)

def $bop(Eq, MathV bigDecimal_l, MathV bigDecimal_r) = BoolV $eq_bigdec(bigDecimal_l, bigDecimal_r)

def $bop(Eq, NumberV double_l, NumberV double_r) = BoolV (double_l = double_r)

def $bop(Eq, AstV ast_l, AstV ast_r) = BoolV (ast_l = ast_r)

def $bop(Eq, v_l, v_r) = BoolV (v_l = v_r)

def $bop(Equal, MathV bigDecimal_l, MathV bigDecimal_r) = BoolV $eq_bigdec(bigDecimal_l, bigDecimal_r)

def $bop(Equal, InfinityV b_l, InfinityV b_r) = BoolV (b_l = b_r)

def $bop(Equal, NumberV double_l, NumberV double_r) = BoolV $numeq_double(double_l, double_r)

def $bop(Equal, BigIntV int_l, BigIntV int_r) = BoolV (int_l = int_r)

def $bop(Equal, InfinityV b, MathV bigDecimal) = BoolV false

def $bop(Equal, MathV bigDecimal, InfinityV b) = BoolV false

def $bop(Add, BigIntV int_l, BigIntV int_r) = BigIntV int_l + int_r

def $bop(LShift, BigIntV int_l, BigIntV int_r) = BigIntV $shl(int_l, nat_n)
 -- if nat_n = int_r

def $bop(RShift, BigIntV int_l, BigIntV int_r) = BigIntV $shr(int_l, nat_n)
 -- if nat_n = int_r

def $bop(Sub, BigIntV int_l, BigIntV int_r) = BigIntV int_l - int_r

def $bop(Mul, BigIntV int_l, BigIntV int_r) = BigIntV int_l * int_r

def $bop(Div, BigIntV int_l, BigIntV int_r) = BigIntV $quot_int(int_l, int_r)

def $bop(Mod, BigIntV int_l, BigIntV int_r) = BigIntV $mod_int(int_l, int_r)

def $bop(Lt, BigIntV int_l, BigIntV int_r) = BoolV int_l < int_r

def $bop(BAnd, BigIntV int_l, BigIntV int_r) = BigIntV $band(int_l, int_r)

def $bop(BOr, BigIntV int_l, BigIntV int_r) = BigIntV $bor(int_l, int_r)

def $bop(BXOr, BigIntV int_l, BigIntV int_r) = BigIntV $bxor(int_l, int_r)

def $bop(Pow, BigIntV int_l, BigIntV int_r) = BigIntV $ipow(int_l, nat_n)
 -- if nat_n = int_r
```

1. If `bop` matches pattern **`Add`**:
   1. If let **`NumberV`** `double_l` be `value''`:
      1. [Let!<sub>type</sub>](index.md#check_let) **`NumberV`** `double_r` be `value'''`.
      1. Return **`NumberV`** `$add_double(double_l, double_r)`.
   1. Else if let **`MathV`** `bigDecimal_l` be `value''`:
      1. If let **`MathV`** `bigDecimal_r` be `value'''`:
         1. Return **`MathV`** `$add_bigdec(bigDecimal_l, bigDecimal_r)`.
      1. Else if let **`InfinityV`** `b` be `value'''`:
         1. Return **`InfinityV`** `b`.
   1. Else if let **`InfinityV`** `b` be `value''`:
      1. If let **`MathV`** `bigDecimal` be `value'''`:
         1. Return **`InfinityV`** `b`.
      1. Else if let **`InfinityV`** `b_r` be `value'''`:
         1. Check that `b` is equal to `b_r`.
         1. Return **`InfinityV`** `b`.
   1. Else if let **`BigIntV`** `int_l` be `value''`:
      1. [Let!<sub>type</sub>](index.md#check_let) **`BigIntV`** `int_r` be `value'''`.
      1. Return **`BigIntV`** `int_l + int_r`.
1. Else if `bop` matches pattern **`Sub`**:
   1. If let **`NumberV`** `double_l` be `value''`:
      1. [Let!<sub>type</sub>](index.md#check_let) **`NumberV`** `double_r` be `value'''`.
      1. Return **`NumberV`** `$sub_double(double_l, double_r)`.
   1. Else if let **`MathV`** `bigDecimal_l` be `value''`:
      1. If let **`MathV`** `bigDecimal_r` be `value'''`:
         1. Return **`MathV`** `$sub_bigdec(bigDecimal_l, bigDecimal_r)`.
      1. Else if let **`InfinityV`** `b` be `value'''`:
         1. Return **`InfinityV`** `~b`.
   1. Else if let **`InfinityV`** `b` be `value''`:
      1. If let **`MathV`** `bigDecimal` be `value'''`:
         1. Return **`InfinityV`** `b`.
      1. Else if let **`InfinityV`** `b_r` be `value'''`:
         1. Check that `b` is not equal to `b_r`.
         1. Return **`InfinityV`** `b`.
   1. Else if let **`BigIntV`** `int_l` be `value''`:
      1. [Let!<sub>type</sub>](index.md#check_let) **`BigIntV`** `int_r` be `value'''`.
      1. Return **`BigIntV`** `int_l - int_r`.
1. Else if `bop` matches pattern **`Mul`**:
   1. If let **`NumberV`** `double_l` be `value''`:
      1. [Let!<sub>type</sub>](index.md#check_let) **`NumberV`** `double_r` be `value'''`.
      1. Return **`NumberV`** `$mul_double(double_l, double_r)`.
   1. Else if let **`MathV`** `bigDecimal_l` be `value''`:
      1. If let **`MathV`** `bigDecimal_r` be `value'''`:
         1. Return **`MathV`** `$mul_bigdec(bigDecimal_l, bigDecimal_r)`.
      1. Else if let **`InfinityV`** `b` be `value'''`:
         1. Try:
            1. 
               1. Check that [**`BigDec`** `0` `0` is less than `bigDecimal_l`](abstract-operations.md#lt_bigdec).
               1. Return **`InfinityV`** `b`.
            1. 
               1. Check that [`bigDecimal_l` is less than **`BigDec`** `0` `0`](abstract-operations.md#lt_bigdec).
               1. Return **`InfinityV`** `~b`.
   1. Else if let **`InfinityV`** `b` be `value''`:
      1. If let **`MathV`** `bigDecimal` be `value'''`:
         1. Try:
            1. 
               1. Check that [**`BigDec`** `0` `0` is less than `bigDecimal`](abstract-operations.md#lt_bigdec).
               1. Return **`InfinityV`** `b`.
            1. 
               1. Check that [`bigDecimal` is less than **`BigDec`** `0` `0`](abstract-operations.md#lt_bigdec).
               1. Return **`InfinityV`** `~b`.
      1. Else if let **`InfinityV`** `b_r` be `value'''`:
         1. Return **`InfinityV`** `b = b_r`.
   1. Else if let **`BigIntV`** `int_l` be `value''`:
      1. [Let!<sub>type</sub>](index.md#check_let) **`BigIntV`** `int_r` be `value'''`.
      1. Return **`BigIntV`** `int_l * int_r`.
1. Else if `bop` matches pattern **`Pow`**:
   1. If let **`NumberV`** `double_l` be `value''`:
      1. [Let!<sub>type</sub>](index.md#check_let) **`NumberV`** `double_r` be `value'''`.
      1. Return **`NumberV`** `$pow_double(double_l, double_r)`.
   1. Else if let **`MathV`** `bigDecimal_l` be `value''`:
      1. [Let!<sub>type</sub>](index.md#check_let) **`MathV`** `bigDecimal_r` be `value'''`.
      1. Let `nat_n` be [**!**](index.md#option_get) [`bigDecimal_r` as a natural number, if it is integral, non-negative, and within 32 bits](builtins.md#as_valid_nat).
      1. Return **`MathV`** `$pow_bigdec_nat(bigDecimal_l, nat_n)`.
      1. Return **`MathV`** `$pow_bigdec(bigDecimal_l, bigDecimal_r)`.
   1. Else if let **`BigIntV`** `int_l` be `value''`:
      1. [Let!<sub>type</sub>](index.md#check_let) **`BigIntV`** `int_r` be `value'''`.
      1. [Let!<sub>type</sub>](index.md#check_let) `nat_n` be `int_r`.
      1. Return **`BigIntV`** `$ipow(int_l, nat_n)`.
1. Else if `bop` matches pattern **`Div`**:
   1. If let **`NumberV`** `double_l` be `value''`:
      1. [Let!<sub>type</sub>](index.md#check_let) **`NumberV`** `double_r` be `value'''`.
      1. Return **`NumberV`** `$div_double(double_l, double_r)`.
   1. Else if let **`MathV`** `bigDecimal_l` be `value''`:
      1. [Let!<sub>type</sub>](index.md#check_let) **`MathV`** `bigDecimal_r` be `value'''`.
      1. Return **`MathV`** `$div_bigdec(bigDecimal_l, bigDecimal_r)`.
   1. Else if let **`BigIntV`** `int_l` be `value''`:
      1. [Let!<sub>type</sub>](index.md#check_let) **`BigIntV`** `int_r` be `value'''`.
      1. Return **`BigIntV`** `$quot_int(int_l, int_r)`.
1. Else if `bop` matches pattern **`Mod`**:
   1. If let **`NumberV`** `double_l` be `value''`:
      1. [Let!<sub>type</sub>](index.md#check_let) **`NumberV`** `double_r` be `value'''`.
      1. Return **`NumberV`** `$mod_double(double_l, double_r)`.
   1. Else if let **`MathV`** `bigDecimal_l` be `value''`:
      1. [Let!<sub>type</sub>](index.md#check_let) **`MathV`** `bigDecimal_r` be `value'''`.
      1. Return **`MathV`** `$mod_bigdec(bigDecimal_l, bigDecimal_r)`.
   1. Else if let **`BigIntV`** `int_l` be `value''`:
      1. [Let!<sub>type</sub>](index.md#check_let) **`BigIntV`** `int_r` be `value'''`.
      1. Return **`BigIntV`** `$mod_int(int_l, int_r)`.
1. Else if `bop` matches pattern **`Lt`**:
   1. Try:
      1. 
         1. Check that `value''` is equal to **`NumberV`** `FIN NEG 0 0`.
         1. Check that `value'''` is equal to **`NumberV`** `FIN POS 0 0`.
         1. Return **`BoolV`** `true`.
      1. 
         1. If let **`NumberV`** `double_l` be `value''`:
            1. [Let!<sub>type</sub>](index.md#check_let) **`NumberV`** `double_r` be `value'''`.
            1. Return **`BoolV`** `$lt_double(double_l, double_r)`.
         1. Else if let **`MathV`** `bigDecimal_l` be `value''`:
            1. If let **`MathV`** `bigDecimal_r` be `value'''`:
               1. Return **`BoolV`** `$lt_bigdec(bigDecimal_l, bigDecimal_r)`.
            1. Else if let **`InfinityV`** `b` be `value'''`:
               1. Return **`BoolV`** `b`.
         1. Else if let **`InfinityV`** `b` be `value''`:
            1. If let **`MathV`** `bigDecimal` be `value'''`:
               1. Return **`BoolV`** `~b`.
            1. Else if let **`InfinityV`** `b_r` be `value'''`:
               1. Check that `b` is not equal to `b_r`.
               1. Return **`BoolV`** `~b`.
         1. Else if let **`BigIntV`** `int_l` be `value''`:
            1. [Let!<sub>type</sub>](index.md#check_let) **`BigIntV`** `int_r` be `value'''`.
            1. Return **`BoolV`** `int_l < int_r`.
1. Else if `bop` matches pattern **`BAnd`**:
   1. If let **`MathV`** `bigDecimal_l` be `value''`:
      1. [Let!<sub>type</sub>](index.md#check_let) **`MathV`** `bigDecimal_r` be `value'''`.
      1. Return **`MathV`** `BigDec $band($trunc_bigdec(bigDecimal_l), $trunc_bigdec(bigDecimal_r)) 0`.
   1. Else if let **`BigIntV`** `int_l` be `value''`:
      1. [Let!<sub>type</sub>](index.md#check_let) **`BigIntV`** `int_r` be `value'''`.
      1. Return **`BigIntV`** `$band(int_l, int_r)`.
1. Else if `bop` matches pattern **`BOr`**:
   1. If let **`MathV`** `bigDecimal_l` be `value''`:
      1. [Let!<sub>type</sub>](index.md#check_let) **`MathV`** `bigDecimal_r` be `value'''`.
      1. Return **`MathV`** `BigDec $bor($trunc_bigdec(bigDecimal_l), $trunc_bigdec(bigDecimal_r)) 0`.
   1. Else if let **`BigIntV`** `int_l` be `value''`:
      1. [Let!<sub>type</sub>](index.md#check_let) **`BigIntV`** `int_r` be `value'''`.
      1. Return **`BigIntV`** `$bor(int_l, int_r)`.
1. Else if `bop` matches pattern **`BXOr`**:
   1. If let **`MathV`** `bigDecimal_l` be `value''`:
      1. [Let!<sub>type</sub>](index.md#check_let) **`MathV`** `bigDecimal_r` be `value'''`.
      1. Return **`MathV`** `BigDec $bxor($trunc_bigdec(bigDecimal_l), $trunc_bigdec(bigDecimal_r)) 0`.
   1. Else if let **`BigIntV`** `int_l` be `value''`:
      1. [Let!<sub>type</sub>](index.md#check_let) **`BigIntV`** `int_r` be `value'''`.
      1. Return **`BigIntV`** `$bxor(int_l, int_r)`.
1. Else if `bop` matches pattern **`LShift`**:
   1. If let **`MathV`** `bigDecimal_l` be `value''`:
      1. [Let!<sub>type</sub>](index.md#check_let) **`MathV`** `bigDecimal_r` be `value'''`.
      1. Let `int` be [`bigDecimal_r` truncated toward zero](abstract-operations.md#trunc_bigdec).
      1. [Let!<sub>type</sub>](index.md#check_let) `nat_n` be `int`.
      1. Return **`MathV`** `BigDec $shl($trunc_bigdec(bigDecimal_l), nat_n) 0`.
   1. Else if let **`BigIntV`** `int_l` be `value''`:
      1. [Let!<sub>type</sub>](index.md#check_let) **`BigIntV`** `int_r` be `value'''`.
      1. [Let!<sub>type</sub>](index.md#check_let) `nat_n` be `int_r`.
      1. Return **`BigIntV`** `$shl(int_l, nat_n)`.
1. Else if `bop` matches pattern **`RShift`**:
   1. If let **`MathV`** `bigDecimal_l` be `value''`:
      1. [Let!<sub>type</sub>](index.md#check_let) **`MathV`** `bigDecimal_r` be `value'''`.
      1. Let `int` be [`bigDecimal_r` truncated toward zero](abstract-operations.md#trunc_bigdec).
      1. [Let!<sub>type</sub>](index.md#check_let) `nat_n` be `int`.
      1. Return **`MathV`** `BigDec $shr($trunc_bigdec(bigDecimal_l), nat_n) 0`.
   1. Else if let **`BigIntV`** `int_l` be `value''`:
      1. [Let!<sub>type</sub>](index.md#check_let) **`BigIntV`** `int_r` be `value'''`.
      1. [Let!<sub>type</sub>](index.md#check_let) `nat_n` be `int_r`.
      1. Return **`BigIntV`** `$shr(int_l, nat_n)`.
1. Else if `bop` matches pattern **`And`**:
   1. [Let!<sub>type</sub>](index.md#check_let) **`BoolV`** `b_l` be `value''`.
   1. [Let!<sub>type</sub>](index.md#check_let) **`BoolV`** `b_r` be `value'''`.
   1. Return **`BoolV`** `b_l /\ b_r`.
1. Else if `bop` matches pattern **`Or`**:
   1. [Let!<sub>type</sub>](index.md#check_let) **`BoolV`** `b_l` be `value''`.
   1. [Let!<sub>type</sub>](index.md#check_let) **`BoolV`** `b_r` be `value'''`.
   1. Return **`BoolV`** `b_l \/ b_r`.
1. Else if `bop` matches pattern **`Xor`**:
   1. [Let!<sub>type</sub>](index.md#check_let) **`BoolV`** `b_l` be `value''`.
   1. [Let!<sub>type</sub>](index.md#check_let) **`BoolV`** `b_r` be `value'''`.
   1. Return **`BoolV`** `b_l =/= b_r`.
1. Else if `bop` matches pattern **`Eq`**:
   1. If let **`MathV`** `bigDecimal_l` be `value''`:
      1. [Let!<sub>type</sub>](index.md#check_let) **`MathV`** `bigDecimal_r` be `value'''`.
      1. Return **`BoolV`** `$eq_bigdec(bigDecimal_l, bigDecimal_r)`.
   1. Else if let **`NumberV`** `double_l` be `value''`:
      1. [Let!<sub>type</sub>](index.md#check_let) **`NumberV`** `double_r` be `value'''`.
      1. Return **`BoolV`** `double_l = double_r`.
   1. Else if let **`AstV`** `ast_l` be `value''`:
      1. [Let!<sub>type</sub>](index.md#check_let) **`AstV`** `ast_r` be `value'''`.
      1. Return **`BoolV`** `ast_l = ast_r`.
   1. Return **`BoolV`** `value'' = value'''`.
1. Else if `bop` matches pattern **`Equal`**:
   1. If let **`MathV`** `bigDecimal_l` be `value''`:
      1. If let **`MathV`** `bigDecimal_r` be `value'''`:
         1. Return **`BoolV`** `$eq_bigdec(bigDecimal_l, bigDecimal_r)`.
      1. Else if let **`InfinityV`** `b` be `value'''`:
         1. Return **`BoolV`** `false`.
   1. Else if let **`InfinityV`** `b_l` be `value''`:
      1. If let **`InfinityV`** `b_r` be `value'''`:
         1. Return **`BoolV`** `b_l = b_r`.
      1. Else if let **`MathV`** `bigDecimal` be `value'''`:
         1. Return **`BoolV`** `false`.
   1. Else if let **`NumberV`** `double_l` be `value''`:
      1. [Let!<sub>type</sub>](index.md#check_let) **`NumberV`** `double_r` be `value'''`.
      1. Return **`BoolV`** `$numeq_double(double_l, double_r)`.
   1. Else if let **`BigIntV`** `int_l` be `value''`:
      1. [Let!<sub>type</sub>](index.md#check_let) **`BigIntV`** `int_r` be `value'''`.
      1. Return **`BoolV`** `int_l = int_r`.

### $mod_int

<a id="mod_int"></a>
`$mod_int(int_l, int_r)`:
`int_l` modulo `int_r` taking the sign of the divisor

```spectec
def $mod_int(int_l, int_r) = (int_l \ int_r) + int_r
 -- if (int_l \ int_r) * int_r < 0

def $mod_int(int_l, int_r) = int_l \ int_r
```

1. Check that `int_l` \ `int_r` * `int_r` is less than `0`.
1. Return `int_l` \ `int_r` + `int_r`.
1. Return `int_l` \ `int_r`.

### $quot_int

<a id="quot_int"></a>
`$quot_int(int_l, int_r)`:
`int_l` divided by `int_r` truncated toward zero

```spectec
def $quot_int(int_l, int_r) = (int_l - (int_l \ int_r)) / int_r
```

1. Return `int_l` - `int_l` \ `int_r` / `int_r`.

### $scale_up

<a id="scale_up"></a>
`$scale_up(int_u, nat_k)`:
`int_u` scaled up by `nat_k` powers of ten

```spectec
def $scale_up(int_u, nat_k) = int_u * $pow10(nat_k)
```

1. Return `int_u` * [10 raised to the power of `nat_k`](builtins.md#pow10).

### $add_bigdec

<a id="add_bigdec"></a>
`$add_bigdec(BigDec int_ul int_sl, BigDec int_ur int_sr)`:
The sum of **`BigDec`** `int_ul` `int_sl` and **`BigDec`** `int_ur` `int_sr`

```spectec
def $add_bigdec(BigDec int_ul int_sl, BigDec int_ur int_sr) = BigDec int_ul + $scale_up(int_ur, nat_k) int_sl
 -- if nat_k = int_sl - int_sr

def $add_bigdec(BigDec int_ul int_sl, BigDec int_ur int_sr) = BigDec $scale_up(int_ul, nat_k) + int_ur int_sr
 -- if nat_k = int_sr - int_sl
```

1. Let `int` be `int_sl` - `int_sr`.
1. [Let!<sub>type</sub>](index.md#check_let) `nat_k` be `int`.
1. Return **`BigDec`** `int_ul + $scale_up(int_ur, nat_k)` `int_sl`.
1. Let `int` be `int_sr` - `int_sl`.
1. [Let!<sub>type</sub>](index.md#check_let) `nat_k` be `int`.
1. Return **`BigDec`** `$scale_up(int_ul, nat_k) + int_ur` `int_sr`.

### $sub_bigdec

<a id="sub_bigdec"></a>
`$sub_bigdec(BigDec int_ul int_sl, BigDec int_ur int_sr)`:
The difference of **`BigDec`** `int_ul` `int_sl` and **`BigDec`** `int_ur` `int_sr`

```spectec
def $sub_bigdec(BigDec int_ul int_sl, BigDec int_ur int_sr) = BigDec int_ul - $scale_up(int_ur, nat_k) int_sl
 -- if nat_k = int_sl - int_sr

def $sub_bigdec(BigDec int_ul int_sl, BigDec int_ur int_sr) = BigDec $scale_up(int_ul, nat_k) - int_ur int_sr
 -- if nat_k = int_sr - int_sl
```

1. Let `int` be `int_sl` - `int_sr`.
1. [Let!<sub>type</sub>](index.md#check_let) `nat_k` be `int`.
1. Return **`BigDec`** `int_ul - $scale_up(int_ur, nat_k)` `int_sl`.
1. Let `int` be `int_sr` - `int_sl`.
1. [Let!<sub>type</sub>](index.md#check_let) `nat_k` be `int`.
1. Return **`BigDec`** `$scale_up(int_ul, nat_k) - int_ur` `int_sr`.

### $mul_bigdec

<a id="mul_bigdec"></a>
`$mul_bigdec(BigDec int_ul int_sl, BigDec int_ur int_sr)`:
The product of **`BigDec`** `int_ul` `int_sl` and **`BigDec`** `int_ur` `int_sr`

```spectec
def $mul_bigdec(BigDec int_ul int_sl, BigDec int_ur int_sr) = BigDec int_ul * int_ur int_sl + int_sr
```

1. Return **`BigDec`** `int_ul * int_ur` `int_sl + int_sr`.

### $mod_bigdec

<a id="mod_bigdec"></a>
`$mod_bigdec(BigDec int_ul int_sl, BigDec int_ur int_sr)`:
**`BigDec`** `int_ul` `int_sl` modulo **`BigDec`** `int_ur` `int_sr`

```spectec
def $mod_bigdec(BigDec int_ul int_sl, BigDec int_ur int_sr) = BigDec $mod_int(int_ul, $scale_up(int_ur, nat_k)) int_sl
 -- if nat_k = int_sl - int_sr

def $mod_bigdec(BigDec int_ul int_sl, BigDec int_ur int_sr) = BigDec $mod_int($scale_up(int_ul, nat_k), int_ur) int_sr
 -- if nat_k = int_sr - int_sl
```

1. Let `int` be `int_sl` - `int_sr`.
1. [Let!<sub>type</sub>](index.md#check_let) `nat_k` be `int`.
1. Return **`BigDec`** `$mod_int(int_ul, $scale_up(int_ur, nat_k))` `int_sl`.
1. Let `int` be `int_sr` - `int_sl`.
1. [Let!<sub>type</sub>](index.md#check_let) `nat_k` be `int`.
1. Return **`BigDec`** `$mod_int($scale_up(int_ul, nat_k), int_ur)` `int_sr`.

### $lt_bigdec

<a id="lt_bigdec"></a>
`$lt_bigdec(BigDec int_ul int_sl, BigDec int_ur int_sr)`:
**`BigDec`** `int_ul` `int_sl` is less than **`BigDec`** `int_ur` `int_sr`

```spectec
def $lt_bigdec(BigDec int_ul int_sl, BigDec int_ur int_sr) = int_ul < $scale_up(int_ur, nat_k)
 -- if nat_k = int_sl - int_sr

def $lt_bigdec(BigDec int_ul int_sl, BigDec int_ur int_sr) = $scale_up(int_ul, nat_k) < int_ur
 -- if nat_k = int_sr - int_sl
```

1. Let `int` be `int_sl` - `int_sr`.
1. [Let!<sub>type</sub>](index.md#check_let) `nat_k` be `int`.
1. Return `int_ul` is less than [`int_ur` scaled up by `nat_k` powers of ten](abstract-operations.md#scale_up).
1. Let `int` be `int_sr` - `int_sl`.
1. [Let!<sub>type</sub>](index.md#check_let) `nat_k` be `int`.
1. Return [`int_ul` scaled up by `nat_k` powers of ten](abstract-operations.md#scale_up) is less than `int_ur`.

### $eq_bigdec

<a id="eq_bigdec"></a>
`$eq_bigdec(BigDec int_ul int_sl, BigDec int_ur int_sr)`:
**`BigDec`** `int_ul` `int_sl` is numerically equal to **`BigDec`** `int_ur` `int_sr`

```spectec
def $eq_bigdec(BigDec int_ul int_sl, BigDec int_ur int_sr) = int_ul = $scale_up(int_ur, nat_k)
 -- if int_sl > int_sr
 -- if nat_k = int_sl - int_sr

def $eq_bigdec(BigDec int_ul int_sl, BigDec int_ur int_sr) = $scale_up(int_ul, nat_k) = int_ur
 -- if int_sl <= int_sr
 -- if nat_k = int_sr - int_sl
```

1. Try:
   1. 
      1. Check that `int_sl` is greater than `int_sr`.
      1. Let `int` be `int_sl` - `int_sr`.
      1. [Let!<sub>type</sub>](index.md#check_let) `nat_k` be `int`.
      1. Return `int_ul` is equal to [`int_ur` scaled up by `nat_k` powers of ten](abstract-operations.md#scale_up).
   1. 
      1. Check that `int_sl` is less than or equal to `int_sr`.
      1. Let `int` be `int_sr` - `int_sl`.
      1. [Let!<sub>type</sub>](index.md#check_let) `nat_k` be `int`.
      1. Return [`int_ul` scaled up by `nat_k` powers of ten](abstract-operations.md#scale_up) is equal to `int_ur`.

### $trunc_bigdec

<a id="trunc_bigdec"></a>
`$trunc_bigdec(BigDec int_u int_s)`:
**`BigDec`** `int_u` `int_s` truncated toward zero

```spectec
def $trunc_bigdec(BigDec int_u int_s) = int_u * $pow10(nat_k)
 -- if nat_k = 0 - int_s

def $trunc_bigdec(BigDec int_u int_s) = $quot_int(int_u, $pow10(nat_k))
 -- if nat_k = int_s
```

1. Let `int` be `0` - `int_s`.
1. Try:
   1. 
      1. [Let!<sub>type</sub>](index.md#check_let) `nat_k` be `int`.
      1. Return `int_u` * [10 raised to the power of `nat_k`](builtins.md#pow10).
   1. 
      1. [Let!<sub>type</sub>](index.md#check_let) `nat_k` be `int_s`.
      1. Return [`int_u` divided by 10 raised to the power of `nat_k` truncated toward zero](abstract-operations.md#quot_int).

### $pow_bigdec_nat

<a id="pow_bigdec_nat"></a>
`$pow_bigdec_nat(BigDec int_u int_s, nat_n)`:
**`BigDec`** `int_u` `int_s` raised to the power of `nat_n`

```spectec
def $pow_bigdec_nat(BigDec int_u int_s, nat_n) = BigDec $ipow(int_u, nat_n) int_s * nat_n
```

1. Return **`BigDec`** `$ipow(int_u, nat_n)` `int_s * nat_n`.

## 변환 연산자

### $cop

<a id="cop"></a>
`$cop(cop, value)`:
The result of converting `value` by `cop`

```spectec
def $cop(ToMath, CodeUnitV int) = MathV (BigDec int 0)

def $cop(ToCodeUnit, MathV bigDecimal) = CodeUnitV $as_uint16($trunc_bigdec(bigDecimal))

def $cop(ToNumber, InfinityV true) = NumberV (INF POS)

def $cop(ToNumber, InfinityV false) = NumberV (INF NEG)

def $cop(ToApproxNumber, MathV bigDecimal) = NumberV $double_of_bigdec(bigDecimal)

def $cop(ToNumber, MathV bigDecimal) = NumberV $double_of_bigdec(bigDecimal)

def $cop(ToBigInt, MathV bigDecimal) = BigIntV $trunc_bigdec(bigDecimal)

def $cop(ToMath, MathV bigDecimal) = MathV bigDecimal

def $cop(ToNumber, StringV t) = NumberV $str_to_number(t)

def $cop(ToBigInt, StringV t) = $str_to_bigint(t)

def $cop(ToStr e?, StringV t) = StringV t

def $cop(ToMath, NumberV double) = MathV $bigdec_of_double(double)

def $cop(ToNumber, NumberV double) = NumberV double

def $cop(ToBigInt, NumberV double) = BigIntV $bigint_of_double(double)

def $cop(ToMath, BigIntV int) = MathV (BigDec int 0)

def $cop(ToBigInt, BigIntV int) = BigIntV int
```

1. If `cop` matches pattern **`ToMath`**:
   1. If let **`CodeUnitV`** `int` be `value`:
      1. Return **`MathV`** `BigDec int 0`.
   1. Else if let **`MathV`** `bigDecimal` be `value`:
      1. Return **`MathV`** `bigDecimal`.
   1. Else if let **`NumberV`** `double` be `value`:
      1. Return **`MathV`** `$bigdec_of_double(double)`.
   1. Else if let **`BigIntV`** `int` be `value`:
      1. Return **`MathV`** `BigDec int 0`.
1. Else if `cop` matches pattern **`ToCodeUnit`**:
   1. [Let!<sub>type</sub>](index.md#check_let) **`MathV`** `bigDecimal` be `value`.
   1. Return **`CodeUnitV`** `$as_uint16($trunc_bigdec(bigDecimal))`.
1. Else if `cop` matches pattern **`ToNumber`**:
   1. Try:
      1. 
         1. Check that `value` is equal to **`InfinityV`** `true`.
         1. Return **`NumberV`** `INF POS`.
      1. 
         1. Check that `value` is equal to **`InfinityV`** `false`.
         1. Return **`NumberV`** `INF NEG`.
      1. 
         1. If let **`MathV`** `bigDecimal` be `value`:
            1. Return **`NumberV`** `$double_of_bigdec(bigDecimal)`.
         1. Else if let **`StringV`** `t` be `value`:
            1. Return **`NumberV`** `$str_to_number(t)`.
         1. Else if let **`NumberV`** `double` be `value`:
            1. Return **`NumberV`** `double`.
1. Else if `cop` matches pattern **`ToApproxNumber`**:
   1. [Let!<sub>type</sub>](index.md#check_let) **`MathV`** `bigDecimal` be `value`.
   1. Return **`NumberV`** `$double_of_bigdec(bigDecimal)`.
1. Else if `cop` matches pattern **`ToBigInt`**:
   1. If let **`MathV`** `bigDecimal` be `value`:
      1. Return **`BigIntV`** `$trunc_bigdec(bigDecimal)`.
   1. Else if let **`StringV`** `t` be `value`:
      1. Return [the result of parsing `t` as a StringIntegerLiteral](builtins.md#str_to_bigint).
   1. Else if let **`NumberV`** `double` be `value`:
      1. Return **`BigIntV`** `$bigint_of_double(double)`.
   1. Else if let **`BigIntV`** `int` be `value`:
      1. Return **`BigIntV`** `int`.
1. Else if let **`ToStr`** `e?` be `cop`:
   1. [Let!<sub>type</sub>](index.md#check_let) **`StringV`** `t` be `value`.
   1. Return **`StringV`** `t`.

### $to_str_radix

<a id="to_str_radix"></a>
`$to_str_radix(value, nat)`:
The text of `value` in radix `nat`

```spectec
def $to_str_radix(NumberV double, nat) = StringV $number_to_text(double, nat)

def $to_str_radix(BigIntV int, nat) = StringV $bigint_to_text(int, nat)
```

1. If let **`NumberV`** `double` be `value`:
   1. Return **`StringV`** `$number_to_text(double, nat)`.
1. Else if let **`BigIntV`** `int` be `value`:
   1. Return **`StringV`** `$bigint_to_text(int, nat)`.

## 가변 인자 연산자와 수학 연산자

### $vop

<a id="vop"></a>
`$vop(vop, value*)`:
The result of applying `vop` to `value*`

```spectec
def $vop(Min, v_1 :: v*) = InfinityV false
 -- if InfinityV false <- v_1 :: v*

def $vop(Min, v_1 :: v*) = InfinityV true
 -- if $remove_val(v_1 :: v*, InfinityV true) = eps

def $vop(Min, v_1 :: v*) = MathV $min_bigdecs(bigDecimal_h, bigDecimal_t*)
 -- if $remove_val(v_1 :: v*, InfinityV true) = (MathV bigDecimal_h) :: (MathV bigDecimal_t)*

def $vop(Max, v_1 :: v*) = InfinityV true
 -- if InfinityV true <- v_1 :: v*

def $vop(Max, v_1 :: v*) = InfinityV false
 -- if $remove_val(v_1 :: v*, InfinityV false) = eps

def $vop(Max, v_1 :: v*) = MathV $max_bigdecs(bigDecimal_h, bigDecimal_t*)
 -- if $remove_val(v_1 :: v*, InfinityV false) = (MathV bigDecimal_h) :: (MathV bigDecimal_t)*

def $vop(Concat, v_1 :: v*) = StringV $concat_texts($to_text(v_1), $to_text(v)*)
```

1. If `vop` matches pattern **`Min`**:
   1. [Let!<sub>type</sub>](index.md#check_let) `v_1 :: v*` be `value*`.
   1. Try:
      1. 
         1. Check that **`InfinityV`** `false` is in `v_1 :: v*`.
         1. Return **`InfinityV`** `false`.
      1. 
         1. Check that [`v_1 :: v*` with every occurrence of **`InfinityV`** `true` removed](abstract-operations.md#remove_val) is equal to `·`.
         1. Return **`InfinityV`** `true`.
   1. Let `value'''*` be [`v_1 :: v*` with every occurrence of **`InfinityV`** `true` removed](abstract-operations.md#remove_val).
   1. [Let!<sub>type</sub>](index.md#check_let) `value' :: value''*` be `value'''*`.
   1. Check that `value''` matches pattern **`MathV`** `_`, for all `value''` in `value''*`.
   1. Let **`MathV`** `bigDecimal_t` be `value''`, for all `bigDecimal_t` in `bigDecimal_t*` and `value''` in `value''*`.
   1. [Let!<sub>type</sub>](index.md#check_let) **`MathV`** `bigDecimal_h` be `value'`.
   1. Return **`MathV`** `$min_bigdecs(bigDecimal_h, bigDecimal_t*)`.
1. Else if `vop` matches pattern **`Max`**:
   1. [Let!<sub>type</sub>](index.md#check_let) `v_1 :: v*` be `value*`.
   1. Try:
      1. 
         1. Check that **`InfinityV`** `true` is in `v_1 :: v*`.
         1. Return **`InfinityV`** `true`.
      1. 
         1. Check that [`v_1 :: v*` with every occurrence of **`InfinityV`** `false` removed](abstract-operations.md#remove_val) is equal to `·`.
         1. Return **`InfinityV`** `false`.
   1. Let `value'''*` be [`v_1 :: v*` with every occurrence of **`InfinityV`** `false` removed](abstract-operations.md#remove_val).
   1. [Let!<sub>type</sub>](index.md#check_let) `value' :: value''*` be `value'''*`.
   1. Check that `value''` matches pattern **`MathV`** `_`, for all `value''` in `value''*`.
   1. Let **`MathV`** `bigDecimal_t` be `value''`, for all `bigDecimal_t` in `bigDecimal_t*` and `value''` in `value''*`.
   1. [Let!<sub>type</sub>](index.md#check_let) **`MathV`** `bigDecimal_h` be `value'`.
   1. Return **`MathV`** `$max_bigdecs(bigDecimal_h, bigDecimal_t*)`.
1. Else if `vop` matches pattern **`Concat`**:
   1. [Let!<sub>type</sub>](index.md#check_let) `v_1 :: v*` be `value*`.
   1. Return **`StringV`** `$concat_texts($to_text(v_1), $to_text(v)*)`.

### $remove_val

<a id="remove_val"></a>
`$remove_val(value*, v_x)`:
`value*` with every occurrence of `v_x` removed

```spectec
def $remove_val(eps, v_x) = eps

def $remove_val(v_1 :: v*, v_x) = $remove_val(v*, v_x)
 -- if v_1 = v_x

def $remove_val(v_1 :: v*, v_x) = v_1 :: $remove_val(v*, v_x)
 -- otherwise
```

1. If `value*` matches pattern `[]`:
   1. Return `·`.
1. Else if let `v_1 :: v*` be `value*`:
   1. Check that `v_1` is equal to `v_x`.
   1. Return [`v*` with every occurrence of `v_x` removed](abstract-operations.md#remove_val).

1. Otherwise:
   1. Otherwise:
      1. Check that `value*` is a non-empty list.
      1. Let `v_1 :: v*` be `value*`.
      1. Return `v_1 :: $remove_val(v*, v_x)`.

### $min_bigdecs

<a id="min_bigdecs"></a>
`$min_bigdecs(bigDecimal'', bigDecimal'*)`:
The least of `bigDecimal''` and `bigDecimal'*`

```spectec
def $min_bigdecs(bigDecimal, eps) = bigDecimal

def $min_bigdecs(bigDecimal_1, bigDecimal_2 :: bigDecimal*) = $min_bigdecs(bigDecimal_1, bigDecimal*)
 -- if ~$lt_bigdec(bigDecimal_2, bigDecimal_1)

def $min_bigdecs(bigDecimal_1, bigDecimal_2 :: bigDecimal*) = $min_bigdecs(bigDecimal_2, bigDecimal*)
```

1. If `bigDecimal'*` matches pattern `[]`:
   1. Return `bigDecimal''`.
1. Else if let `bigDecimal_2 :: bigDecimal*` be `bigDecimal'*`:
   1. Check that [`bigDecimal_2` is not less than `bigDecimal''`](abstract-operations.md#lt_bigdec).
   1. Return [the least of `bigDecimal''` and `bigDecimal*`](abstract-operations.md#min_bigdecs).
   1. Return [the least of `bigDecimal_2` and `bigDecimal*`](abstract-operations.md#min_bigdecs).

### $max_bigdecs

<a id="max_bigdecs"></a>
`$max_bigdecs(bigDecimal'', bigDecimal'*)`:
The greatest of `bigDecimal''` and `bigDecimal'*`

```spectec
def $max_bigdecs(bigDecimal, eps) = bigDecimal

def $max_bigdecs(bigDecimal_1, bigDecimal_2 :: bigDecimal*) = $max_bigdecs(bigDecimal_1, bigDecimal*)
 -- if ~$lt_bigdec(bigDecimal_1, bigDecimal_2)

def $max_bigdecs(bigDecimal_1, bigDecimal_2 :: bigDecimal*) = $max_bigdecs(bigDecimal_2, bigDecimal*)
```

1. If `bigDecimal'*` matches pattern `[]`:
   1. Return `bigDecimal''`.
1. Else if let `bigDecimal_2 :: bigDecimal*` be `bigDecimal'*`:
   1. Check that [`bigDecimal''` is not less than `bigDecimal_2`](abstract-operations.md#lt_bigdec).
   1. Return [the greatest of `bigDecimal''` and `bigDecimal*`](abstract-operations.md#max_bigdecs).
   1. Return [the greatest of `bigDecimal_2` and `bigDecimal*`](abstract-operations.md#max_bigdecs).

### $to_text

<a id="to_text"></a>
`$to_text(value)`:
The text of `value`

```spectec
def $to_text(StringV t) = t

def $to_text(CodeUnitV int) = $text_of_code_units(nat)
 -- if nat = int
```

1. If let **`StringV`** `t` be `value`:
   1. Return `t`.
1. Else if let **`CodeUnitV`** `int` be `value`:
   1. [Let!<sub>type</sub>](index.md#check_let) `nat` be `int`.
   1. Return [the string formed from the UTF-16 code units `nat`](builtins.md#text_of_code_units).

### $concat_texts

<a id="concat_texts"></a>
`$concat_texts(t', text*)`:
`t'` followed by `text*`

```spectec
def $concat_texts(t, eps) = t

def $concat_texts(t_1, t_2 :: t*) = $concat_texts(t_1 ++ t_2, t*)
```

1. If `text*` matches pattern `[]`:
   1. Return `t'`.
1. Else if let `t_2 :: t*` be `text*`:
   1. Return [`t'` concatenated with `t_2` followed by `t*`](abstract-operations.md#concat_texts).

### $mop

<a id="mop"></a>
`$mop(mop, value'''*)`:
The result of applying `mop` to `value'''*`

```spectec
def $mop(mop, [MathV bigDecimal]) = MathV $math_un(mop, bigDecimal)
 -- if mop <- [Expm1, Log10, Log2, Cos, Cbrt, Exp, Cosh, Sinh, Tanh, Acos, Asin, Atan, Log1p, Log, Sin, Sqrt, Tan]

def $mop(Atan2, [MathV bigDecimal_x, MathV bigDecimal_y]) = MathV $math_atan2(bigDecimal_x, bigDecimal_y)
```

1. Try:
   1. 
      1. [Let!<sub>type</sub>](index.md#check_let) `value` be `value'''*`.
      1. [Let!<sub>type</sub>](index.md#check_let) **`MathV`** `bigDecimal` be `value`.
      1. Check that `mop` is in `[ Expm1, Log10, Log2, Cos, Cbrt, Exp, Cosh, Sinh, Tanh, Acos, Asin, Atan, Log1p, Log, Sin, Sqrt, Tan ]`.
      1. Return **`MathV`** `$math_un(mop, bigDecimal)`.
   1. 
      1. Check that `mop` matches pattern **`Atan2`**.
      1. [Let!<sub>type</sub>](index.md#check_let) `[ value, value' ]` be `value'''*`.
      1. [Let!<sub>type</sub>](index.md#check_let) **`MathV`** `bigDecimal_x` be `value`.
      1. [Let!<sub>type</sub>](index.md#check_let) **`MathV`** `bigDecimal_y` be `value'`.
      1. Return **`MathV`** `$math_atan2(bigDecimal_x, bigDecimal_y)`.

## 식 보조 연산

### $size_of

<a id="size_of"></a>
`$size_of(state, value)`:
The size of `value`

```spectec
def $size_of(state, StringV t) = MathV $to_bigdec(|t|)

def $size_of(state, AddrV a) = MathV $to_bigdec($size_of_obj(o))
 -- if $find_heap(state, a) = o

def $size_of(state, AstV ast) = MathV $to_bigdec($size_of_ast(ast))
```

1. If let **`StringV`** `t` be `value`:
   1. Return **`MathV`** `$to_bigdec(the length of t)`.
1. Else if let **`AddrV`** `a` be `value`:
   1. Let `o` be [**!**](index.md#option_get) [the object at `a` in the heap of `state`](abstract-operations.md#find_heap).
   1. Return **`MathV`** `$to_bigdec($size_of_obj(o))`.
1. Else if let **`AstV`** `ast` be `value`:
   1. Return **`MathV`** `$to_bigdec($size_of_ast(ast))`.

### $capture

<a id="capture"></a>
`$capture(state, name''*)`:
The values of `name''*` captured from `state`

```spectec
def $capture(state, eps) = $empty_map<name, value>

def $capture(state, name_1 :: name*) = $add_map<name, value>($capture(state, name*), name_1, v)
 -- if $find_local(state, name_1) = v
```

1. If `name''*` matches pattern `[]`:
   1. Return [an empty map](abstract-operations.md#empty_map).
1. Else if let `name_1 :: name*` be `name''*`:
   1. Let `v` be [**!**](index.md#option_get) [the value of local `name_1` in `state`](abstract-operations.md#find_local).
   1. Return [the map the values of `name*` captured from `state` with `name_1` bound to `v`](builtins.md#add_map).

### $names_only

<a id="names_only"></a>
`$names_only(set<pair<local, value>>')`:
The named locals of `set<pair<local, value>>'`

```spectec
def $names_only(`{eps}) = $empty_map<name, value>

def $names_only(`{(name : value) :: (local_t : value_t)*}) = $add_map<name, value>($names_only(`{(local_t : value_t)*}), name, value)

def $names_only(`{(local : value) :: (local_t : value_t)*}) = $names_only(`{(local_t : value_t)*})
 -- otherwise
```

1. Check that `set<pair<local, value>>'` is equal to `{·}`.
1. Return [an empty map](abstract-operations.md#empty_map).
1. Let `{pair<local, value>*}` be `set<pair<local, value>>'`.
1. [Let!<sub>type</sub>](index.md#check_let) `local : value :: ( local_t : value_t )*` be `pair<local, value>*`.
1. [Let!<sub>type</sub>](index.md#check_let) `name` be `local`.
1. Return [the map the named locals of `{( local_t : value_t )*}` with `name` bound to `value`](builtins.md#add_map).

1. Otherwise:
   1. Otherwise:
      1. Try:
         1. 
            1. Let `{pair<local, value>*}` be `set<pair<local, value>>'`.
            1. [Let!<sub>type</sub>](index.md#check_let) `local : value :: ( local_t : value_t )*` be `pair<local, value>*`.
            1. Return [the named locals of `{( local_t : value_t )*}`](abstract-operations.md#names_only).

### $capture_all

<a id="capture_all"></a>
`$capture_all(st)`:
Every named local of `st`

```spectec
def $capture_all(st) = $names_only(st.CTX.LOCAL)
```

1. Return [the named locals of `st.CTX.LOCAL`](abstract-operations.md#names_only).

### $is_callable

<a id="is_callable"></a>
`$is_callable(value')`:
`value'` is callable

```spectec
def $is_callable(CloV callable) = true

def $is_callable(v) = false
 -- otherwise
```

1. [Let!<sub>type</sub>](index.md#check_let) **`CloV`** `callable` be `value'`. return `true`.

1. Otherwise:
   1. Otherwise:
      1. Return `false`.
