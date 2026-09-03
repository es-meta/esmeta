# 명령

[← 식](expressions.md) · [프로그램 실행 →](running.md)

명령 실행은 전부 small-step이고, ESMeta의 `Interpreter.step`을 따른다. 구성은 상태
*와* 앞으로 실행할 명령 열이다.

<a id="Step"></a>
`s ⊢ inst''''* ⟹ % ; %`:
Stepping `inst''''*` in state `s`

명령 열을 상태에서 읽지 않고 인자로 받기 때문에, 규칙이 자기가 다루는 명령에 바로
패턴을 맞출 수 있다. 커서에 대한 조건을 쓸 일도, 커서를 옮길 일도 없다.

## 묶음 (`IBlock`)

묶음은 자기 본문을 앞으로 실행할 명령들 앞에 그대로 이어 붙인다. 그래서 묶는 데
드는 비용이 없고, 따로 프레임을 잡을 일도 없다.

```spectec
rule Step/IBlock:
  s |- ((IBlock inst'*) :: inst*) ==> s ; (inst'* ++ inst*)
```

<a id="Step/IBlock"></a>
Stepping `inst'''*` in state `s` yields `inst'*` concatenated with `inst*` in state `s`, provided:

1. `inst'''*` is a non-empty list.
1. Let `inst'' :: inst*` be `inst'''*`.
1. `inst''` matches pattern **`IBlock`** `_`.
1. Let `IBlock inst'*` be `inst''`.

## 묶기와 대입 (`ILet`, `IAssign`)

```spectec
rule Step/ILet:
  s |- (ILet name e :: inst*) ==> $bind_local(s_1, name, v) ; inst*
 -- Eval_expr: s |- e ==> v ; s_1
```

<a id="Step/ILet"></a>
Stepping `inst''*` in state `s` yields `inst*` in state [`s_1` with local `name` bound to `v`](abstract-operations.md#bind_local), provided:

1. `inst''*` is a non-empty list.
1. Let `inst' :: inst*` be `inst''*`.
1. `inst'` matches pattern **`ILet`** `_` `_`.
1. Let `ILet name e` be `inst'`.
1. [Evaluating `e` in state `s`](expressions.md#Eval_expr) yields `v` in state `s_1`.

대입은 [참조 규칙](expressions.md#references)을 거친다. 그래서 규칙이 구문
종류마다가 아니라 대상 종류마다 하나씩 있다.

```spectec
rule Step/IAssign-local:
  s |- ((IAssign ref e) :: inst*) ==> $bind_local(s_2, local, value) ; inst*
 -- Eval_ref: s |- ref ==> VarTarget local ; s_1
 -- Eval_expr: s_1 |- e ==> value ; s_2

rule Step/IAssign-global:
  s |- ((IAssign ref e) :: inst*) ==> $bind_global(s_2, global, value) ; inst*
 -- Eval_ref: s |- ref ==> VarTarget global ; s_1
 -- Eval_expr: s_1 |- e ==> value ; s_2

rule Step/IAssign-fieldtarget:
  s |- ((IAssign ref e) :: inst*) ==> $update_heap(s_2, a, v_f, value) ; inst*
 -- Eval_ref: s |- ref ==> FieldTarget (AddrV a) v_f ; s_1
 -- Eval_expr: s_1 |- e ==> value ; s_2
```

<a id="Step/IAssign-local"></a>
Stepping `inst''*` in state `s` yields `inst*` in state [`s_2` with local `local` bound to `value`](abstract-operations.md#bind_local), provided:

1. `inst''*` is a non-empty list.
1. Let `inst' :: inst*` be `inst''*`.
1. `inst'` matches pattern **`IAssign`** `_` `_`.
1. Let `IAssign ref e` be `inst'`.
1. [Resolving `ref` in state `s`](expressions.md#Eval_ref) yields `refTarget` in state `s_1`.
1. `refTarget` matches pattern **`VarTarget`** `_`.
1. Let `VarTarget variable` be `refTarget`.
1. `variable` has type `local`.
1. Let `local` be `variable`.
1. [Evaluating `e` in state `s_1`](expressions.md#Eval_expr) yields `value` in state `s_2`.

<a id="Step/IAssign-global"></a>
<a id="Step/IAssign-fieldtarget"></a>
Stepping `inst''*` in state `s` yields `inst*` in state [`s_2` with global `global` bound to `value`](abstract-operations.md#bind_global), provided:

1. `inst''*` is a non-empty list.
1. Let `inst' :: inst*` be `inst''*`.
1. `inst'` matches pattern **`IAssign`** `_` `_`.
1. Let `IAssign ref e` be `inst'`.
1. [Resolving `ref` in state `s`](expressions.md#Eval_ref) yields `refTarget` in state `s_1`.
1. `refTarget` matches pattern **`VarTarget`** `_`.
1. Let `VarTarget variable` be `refTarget`.
1. `variable` has type `global`.
1. Let `global` be `variable`.
1. [Evaluating `e` in state `s_1`](expressions.md#Eval_expr) yields `value` in state `s_2`.

Stepping `inst''*` in state `s` yields `inst*` in state [`s_2` with field `v_f` of the object at `a` set to `value`](abstract-operations.md#update_heap), provided:

1. `inst''*` is a non-empty list.
1. Let `inst' :: inst*` be `inst''*`.
1. `inst'` matches pattern **`IAssign`** `_` `_`.
1. Let `IAssign ref e` be `inst'`.
1. [Resolving `ref` in state `s`](expressions.md#Eval_ref) yields `refTarget` in state `s_1`.
1. `refTarget` matches pattern **`FieldTarget`** `_` `_`.
1. Let `FieldTarget value' v_f` be `refTarget`.
1. `value'` matches pattern **`AddrV`** `_`.
1. Let `AddrV a` be `value'`.
1. [Evaluating `e` in state `s_1`](expressions.md#Eval_expr) yields `value` in state `s_2`.

## 분기 (`IIf`, `IWhile`)

`IIf`는 조건을 평가한 뒤 둘 중 한 갈래로 이어 간다. 조건이 불 값이 아니면 명령 쪽
실패이므로, 멈춰 서지 않고 트랩에 빠진다.

else 갈래가 없는 `IIf`는 조건이 거짓일 때 뒤따르는 명령으로 그냥 이어 간다.
그래서 거짓인 경우가 빈 갈래를 둔 규칙 하나가 아니라 규칙 둘로 나뉜다.

```spectec
rule Step/IIf-true:
  s |- ((IIf e inst_t inst_f?) :: inst*) ==> s_1 ; (inst_t :: inst*)
 -- Eval_expr: s |- e ==> BoolV true ; s_1

rule Step/IIf-false:
  s |- ((IIf e inst_t inst_f) :: inst*) ==> s_1 ; (inst_f :: inst*)
 -- Eval_expr: s |- e ==> BoolV false ; s_1

rule Step/IIf-false-noelse:
  s |- ((IIf e inst_t eps) :: inst*) ==> s_1 ; inst*
 -- Eval_expr: s |- e ==> BoolV false ; s_1

rule Step/IIf-trap:
  s |- ((IIf e inst_t inst_f?) :: inst*) ==> s_1 ; (ITrap (NoBool v) :: eps)
 -- Eval_expr: s |- e ==> v ; s_1
 -- if v =/= BoolV true
 -- if v =/= BoolV false
```

<a id="Step/IIf-true"></a>
<a id="Step/IIf-false"></a>
Stepping `inst''*` in state `s` yields `inst_t :: inst*` in state `s_1`, provided:

1. `inst''*` is a non-empty list.
1. Let `inst' :: inst*` be `inst''*`.
1. `inst'` matches pattern **`IIf`** `_` `_` `_`.
1. Let `IIf e inst_t inst_f?` be `inst'`.
1. [Evaluating `e` in state `s`](expressions.md#Eval_expr) yields `value` in state `s_1`.
1. `value` is equal to **`BoolV`** `true`.

Stepping `inst'''*` in state `s` yields `inst_f :: inst*` in state `s_1`, provided:

1. `inst'''*` is a non-empty list.
1. Let `inst'' :: inst*` be `inst'''*`.
1. `inst''` matches pattern **`IIf`** `_` `_` `_`.
1. Let `IIf e inst_t inst'?` be `inst''`.
1. `inst'?` is defined.
1. Let `inst_f` be `inst'?`.
1. [Evaluating `e` in state `s`](expressions.md#Eval_expr) yields `value` in state `s_1`.
1. `value` is equal to **`BoolV`** `false`.

<a id="Step/IIf-false-noelse"></a>
<a id="Step/IIf-trap"></a>
Stepping `inst'''*` in state `s` yields `inst*` in state `s_1`, provided:

1. `inst'''*` is a non-empty list.
1. Let `inst'' :: inst*` be `inst'''*`.
1. `inst''` matches pattern **`IIf`** `_` `_` `_`.
1. Let `IIf e inst_t inst'?` be `inst''`.
1. `inst'?` is none.
1. [Evaluating `e` in state `s`](expressions.md#Eval_expr) yields `value` in state `s_1`.
1. `value` is equal to **`BoolV`** `false`.

Stepping `inst''*` in state `s` yields `ITrap NoBool v :: ·` in state `s_1`, provided:

1. `inst''*` is a non-empty list.
1. Let `inst' :: inst*` be `inst''*`.
1. `inst'` matches pattern **`IIf`** `_` `_` `_`.
1. Let `IIf e inst_t inst_f?` be `inst'`.
1. [Evaluating `e` in state `s`](expressions.md#Eval_expr) yields `v` in state `s_1`.
1. `v` is not equal to **`BoolV`** `true`.
1. `v` is not equal to **`BoolV`** `false`.

`IWhile`은 펼쳐진다. 조건이 참이면 본문 뒤에 반복문 자신이 오는 모습으로 다시
쓰이고, 그래서 반복문에 따로 보조 관계가 필요 없다.

```spectec
rule Step/IWhile-true:
  s |- ((IWhile e inst_b) :: inst*) ==> s_1 ; (inst_b :: (IWhile e inst_b) :: inst*)
 -- Eval_expr: s |- e ==> BoolV true ; s_1

rule Step/IWhile-false:
  s |- ((IWhile e inst_b) :: inst*) ==> s_1 ; inst*
 -- Eval_expr: s |- e ==> BoolV false ; s_1

rule Step/IWhile-trap:
  s |- ((IWhile e inst_b) :: inst*) ==> s_1 ; (ITrap (NoBool v) :: eps)
 -- Eval_expr: s |- e ==> v ; s_1
 -- if v =/= BoolV true
 -- if v =/= BoolV false
```

<a id="Step/IWhile-true"></a>
<a id="Step/IWhile-false"></a>
Stepping `inst''*` in state `s` yields `inst_b :: IWhile e inst_b :: inst*` in state `s_1`, provided:

1. `inst''*` is a non-empty list.
1. Let `inst' :: inst*` be `inst''*`.
1. `inst'` matches pattern **`IWhile`** `_` `_`.
1. Let `IWhile e inst_b` be `inst'`.
1. [Evaluating `e` in state `s`](expressions.md#Eval_expr) yields `value` in state `s_1`.
1. `value` is equal to **`BoolV`** `true`.

Stepping `inst''*` in state `s` yields `inst*` in state `s_1`, provided:

1. `inst''*` is a non-empty list.
1. Let `inst' :: inst*` be `inst''*`.
1. `inst'` matches pattern **`IWhile`** `_` `_`.
1. Let `IWhile e inst_b` be `inst'`.
1. [Evaluating `e` in state `s`](expressions.md#Eval_expr) yields `value` in state `s_1`.
1. `value` is equal to **`BoolV`** `false`.

반복문의 조건도 불 값이 아니면 분기 조건과 똑같이 트랩에 빠진다.

<a id="Step/IWhile-trap"></a>
Stepping `inst''*` in state `s` yields `ITrap NoBool v :: ·` in state `s_1`, provided:

1. `inst''*` is a non-empty list.
1. Let `inst' :: inst*` be `inst''*`.
1. `inst'` matches pattern **`IWhile`** `_` `_`.
1. Let `IWhile e inst_b` be `inst'`.
1. [Evaluating `e` in state `s`](expressions.md#Eval_expr) yields `v` in state `s_1`.
1. `v` is not equal to **`BoolV`** `true`.
1. `v` is not equal to **`BoolV`** `false`.

## 호출과 반환 (`ICall`, `IReturn`)

호출은 호출자를 잠시 멈춰 둔다. 호출자의 문맥과 남은 명령들, 결과를 받을 지역
변수를 `callctx`로 쌓아 두고, 피호출자의 본문이 실행할 명령 열이 된다.

```spectec
rule Step/ICall-clo:
  s |- ((ICall local e_f e_a*) :: inst*) ==> s_2[STACK = (callctx :: s_2.STACK)][CTX = $callee_context(func, v*, mnv)] ; $body_of(func)
 -- Eval_expr: s |- e_f ==> CloV (CallV func mnv) ; s_1
 -- Eval_exprs: s_1 |- e_a* ==> v* ; s_2
 -- if callctx = {CTX s_2.CTX, INST inst*, RETID local}
```

<a id="Step/ICall-clo"></a>
Stepping `inst''*` in state `s` yields [the body of `func`](abstract-operations.md#body_of) in state `s_2[STACK = callctx :: s_2.STACK]` with `CTX` set to [the context a call to `func` with arguments `v*` and captured environment `mnv` enters](abstract-operations.md#callee_context), provided:

1. `inst''*` is a non-empty list.
1. Let `inst' :: inst*` be `inst''*`.
1. `inst'` matches pattern **`ICall`** `_` `_` `_`.
1. Let `ICall local e_f e_a*` be `inst'`.
1. [Evaluating `e_f` in state `s`](expressions.md#Eval_expr) yields `value` in state `s_1`.
1. `value` matches pattern **`CloV`** `_`.
1. Let `CloV callable` be `value`.
1. `callable` matches pattern **`CallV`** `_` `_`.
1. Let `CallV func mnv` be `callable`.
1. [Evaluating `e_a*` in state `s_1`](expressions.md#Eval_exprs) yields `v*` in state `s_2`.
1. Let `callctx` be {CTX `s_2.CTX`, INST `inst*`, RETID `local`}.

연속을 부를 때는 스택에 쌓지 않고 스택을 통째로 바꾼다. 그래서 연속이 자기가
만들어진 자리로 돌아간다. 부를 수 없는 것을 부르면 트랩에 빠진다.

```spectec
rule Step/ICall-cont:
  s |- ((ICall local e_f e_a*) :: inst*) ==> s_2[STACK = callctx*][CTX = $callee_context(func, v*, mnv)] ; $body_of(func)
 -- Eval_expr: s |- e_f ==> CloV (ContV func mnv callctx*) ; s_1
 -- Eval_exprs: s_1 |- e_a* ==> v* ; s_2

rule Step/ICall-trap:
  s |- ((ICall local e_f e_a*) :: inst*) ==> s_1 ; (ITrap (NoCallable v) :: eps)
 -- Eval_expr: s |- e_f ==> v ; s_1
 -- if $is_callable(v) = false
```

<a id="Step/ICall-cont"></a>
<a id="Step/ICall-trap"></a>
Stepping `inst''*` in state `s` yields [the body of `func`](abstract-operations.md#body_of) in state `s_2[STACK = callctx*]` with `CTX` set to [the context a call to `func` with arguments `v*` and captured environment `mnv` enters](abstract-operations.md#callee_context), provided:

1. `inst''*` is a non-empty list.
1. Let `inst' :: inst*` be `inst''*`.
1. `inst'` matches pattern **`ICall`** `_` `_` `_`.
1. Let `ICall local e_f e_a*` be `inst'`.
1. [Evaluating `e_f` in state `s`](expressions.md#Eval_expr) yields `value` in state `s_1`.
1. `value` matches pattern **`CloV`** `_`.
1. Let `CloV callable` be `value`.
1. `callable` matches pattern **`ContV`** `_` `_` `_`.
1. Let `ContV func mnv callctx*` be `callable`.
1. [Evaluating `e_a*` in state `s_1`](expressions.md#Eval_exprs) yields `v*` in state `s_2`.

Stepping `inst''*` in state `s` yields `ITrap NoCallable v :: ·` in state `s_1`, provided:

1. `inst''*` is a non-empty list.
1. Let `inst' :: inst*` be `inst''*`.
1. `inst'` matches pattern **`ICall`** `_` `_` `_`.
1. Let `ICall local e_f e_a*` be `inst'`.
1. [Evaluating `e_f` in state `s`](expressions.md#Eval_expr) yields `v` in state `s_1`.
1. [`v` is callable](abstract-operations.md#is_callable) is equal to `false`.

구문 지향 연산 호출은 먼저 AST의 생성 규칙 사슬을 따라 연산을 찾고, 그 AST를 첫
인자로 삼아 들어간다.

```spectec
rule Step/ISdoCall:
  s |- ((ISdoCall local e_b t_op e_a*) :: inst*) ==> s_2[STACK = (callctx :: s_2.STACK)][CTX = $callee_context(func, (AstV ast_0) :: v*, $empty_map<name, value>)] ; $body_of(func)
 -- Eval_expr: s |- e_b ==> AstV ast ; s_1
 -- if (ast_0, func) = $find_sdo(s_1, $ast_chain(ast), t_op)
 -- Eval_exprs: s_1 |- e_a* ==> v* ; s_2
 -- if callctx = {CTX s_2.CTX, INST inst*, RETID local}
```

<a id="Step/ISdoCall"></a>
Stepping `inst''*` in state `s` yields [the body of `func`](abstract-operations.md#body_of) in state `s_2[STACK = callctx :: s_2.STACK]` with `CTX` set to [the context a call to `func` with arguments `AstV ast_0 :: v*` and captured environment an empty map enters](abstract-operations.md#callee_context), provided:

1. `inst''*` is a non-empty list.
1. Let `inst' :: inst*` be `inst''*`.
1. `inst'` matches pattern **`ISdoCall`** `_` `_` `_` `_`.
1. Let `ISdoCall local e_b t_op e_a*` be `inst'`.
1. [Evaluating `e_b` in state `s`](expressions.md#Eval_expr) yields `value` in state `s_1`.
1. `value` matches pattern **`AstV`** `_`.
1. Let `AstV ast` be `value`.
1. Let `(ast, func)?` be [the node in the chain of sole children descending from `ast` carrying operation `t_op` and that operation](abstract-operations.md#find_sdo).
1. `(ast, func)?` is defined.
1. Let `( ast_0, func )` be `(ast, func)?`.
1. [Evaluating `e_a*` in state `s_1`](expressions.md#Eval_exprs) yields `v*` in state `s_2`.
1. Let `callctx` be {CTX `s_2.CTX`, INST `inst*`, RETID `local`}.

반환은 스택에서 하나를 꺼내고, 호출자가 비워 둔 지역 변수에 결과를 묶고, 적어 둔
커서에서 호출자를 이어 간다.

```spectec
rule Step/IReturn:
  s |- ((IReturn e) :: inst*) ==> $bind_local(s_1[CTX = callctx.CTX][STACK = callctx'*], callctx.RETID, v) ; callctx.INST
 -- Eval_expr: s |- e ==> v ; s_1
 -- if s.STACK = (callctx :: callctx'*)
```

<a id="Step/IReturn"></a>
Stepping `inst''*` in state `s` yields `callctx.INST` in state [`s_1\[CTX = callctx.CTX\]` with `STACK` set to `callctx'*` with local `callctx.RETID` bound to `v`](abstract-operations.md#bind_local), provided:

1. `inst''*` is a non-empty list.
1. Let `inst' :: inst*` be `inst''*`.
1. `inst'` matches pattern **`IReturn`** `_`.
1. Let `IReturn e` be `inst'`.
1. [Evaluating `e` in state `s`](expressions.md#Eval_expr) yields `v` in state `s_1`.
1. Let `callctx''*` be `s.STACK`.
1. `callctx''*` is a non-empty list.
1. Let `callctx :: callctx'*` be `callctx''*`.

가장 바깥 프레임에서 돌아올 때는 다르다. 이어 갈 호출자가 없으니 프로그램이 그냥
끝난다.

```spectec
rule Step/IReturn-top:
  s |- ((IReturn e) :: inst*) ==> $bind_global(s_1, Global "RESULT", v) ; eps
 -- Eval_expr: s |- e ==> v ; s_1
 -- if s.STACK = eps
```

<a id="Step/IReturn-top"></a>
Stepping `inst''*` in state `s` yields `·` in state [`s_1` with global **`Global`** `"RESULT"` bound to `v`](abstract-operations.md#bind_global), provided:

1. `inst''*` is a non-empty list.
1. Let `inst' :: inst*` be `inst''*`.
1. `inst'` matches pattern **`IReturn`** `_`.
1. Let `IReturn e` be `inst'`.
1. [Evaluating `e` in state `s`](expressions.md#Eval_expr) yields `v` in state `s_1`.
1. `s.STACK` is equal to `·`.

피호출자가 반환하지 않고 끝까지 가 버리는 것은 조용한 완료가 아니라 실패다.

```spectec
rule Step/exit-noreturn:
  s |- eps ==> s ; (ITrap NoReturn :: eps)
 -- if s.STACK =/= eps
```

<a id="Step/exit-noreturn"></a>
Stepping `inst*` in state `s` yields `ITrap NoReturn :: ·` in state `s`, provided:

1. `inst*` is an empty list.
1. `s.STACK` is not equal to `·`.

## 힙 갱신 (`IPush`, `IPop`, `IDelete`, `IExpand`)

이 넷은 새로 할당하는 대신 객체를 그 자리에서 바꾼다. 그래서 각각 피연산자를
평가한 뒤 주소를 힙 연산에 넘긴다. 넣기와 빼기는 리스트의 어느 쪽 끝인지를 표시로
받는다.

```spectec
rule Step/IPush:
  s |- ((IPush e_v e_a b) :: inst*) ==> $push_heap(s_2, a, v, b) ; inst*
 -- Eval_expr: s |- e_v ==> v ; s_1
 -- Eval_expr: s_1 |- e_a ==> AddrV a ; s_2

rule Step/IPop:
  s |- ((IPop local e_l b) :: inst*) ==> $bind_local(s_2, local, value) ; inst*
 -- Eval_expr: s |- e_l ==> AddrV a ; s_1
 -- if (s_2, value) = $pop_heap(s_1, a, b)
```

<a id="Step/IPush"></a>
<a id="Step/IPop"></a>
Stepping `inst''*` in state `s` yields `inst*` in state [`s_2` with `v` added to the object at `a` at its front if `b` or its end otherwise](abstract-operations.md#push_heap), provided:

1. `inst''*` is a non-empty list.
1. Let `inst' :: inst*` be `inst''*`.
1. `inst'` matches pattern **`IPush`** `_` `_` `_`.
1. Let `IPush e_v e_a b` be `inst'`.
1. [Evaluating `e_v` in state `s`](expressions.md#Eval_expr) yields `v` in state `s_1`.
1. [Evaluating `e_a` in state `s_1`](expressions.md#Eval_expr) yields `value` in state `s_2`.
1. `value` matches pattern **`AddrV`** `_`.
1. Let `AddrV a` be `value`.

Stepping `inst''*` in state `s` yields `inst*` in state [`s_2` with local `local` bound to `value`](abstract-operations.md#bind_local), provided:

1. `inst''*` is a non-empty list.
1. Let `inst' :: inst*` be `inst''*`.
1. `inst'` matches pattern **`IPop`** `_` `_` `_`.
1. Let `IPop local e_l b` be `inst'`.
1. [Evaluating `e_l` in state `s`](expressions.md#Eval_expr) yields `value'` in state `s_1`.
1. `value'` matches pattern **`AddrV`** `_`.
1. Let `AddrV a` be `value'`.
1. Let `( s_2, value )` be [the state after removing an end element of `a` and that element](abstract-operations.md#pop_heap).

지우기는 필드나 키를 없애고, 넓히기는 하나를 더한다.

```spectec
rule Step/IDelete:
  s |- ((IDelete ref e) :: inst*) ==> $delete_heap(s_2, a, v) ; inst*
 -- Eval_ref: s |- ref ==> refTarget ; s_1
 -- if AddrV a = $read_ref(s_1, refTarget)
 -- Eval_expr: s_1 |- e ==> v ; s_2

rule Step/IExpand:
  s |- ((IExpand ref e) :: inst*) ==> $expand_heap(s_2, a, v) ; inst*
 -- Eval_ref: s |- ref ==> refTarget ; s_1
 -- if AddrV a = $read_ref(s, refTarget)
 -- Eval_expr: s_1 |- e ==> v ; s_2
```

<a id="Step/IDelete"></a>
<a id="Step/IExpand"></a>
Stepping `inst''*` in state `s` yields `inst*` in state [`s_2` with field `v` removed from the object at `a`](abstract-operations.md#delete_heap), provided:

1. `inst''*` is a non-empty list.
1. Let `inst' :: inst*` be `inst''*`.
1. `inst'` matches pattern **`IDelete`** `_` `_`.
1. Let `IDelete ref e` be `inst'`.
1. [Resolving `ref` in state `s`](expressions.md#Eval_ref) yields `refTarget` in state `s_1`.
1. Let `value` be [the value `refTarget` reads to in state `s_1`](abstract-operations.md#read_ref).
1. `value` matches pattern **`AddrV`** `_`.
1. Let `AddrV a` be `value`.
1. [Evaluating `e` in state `s_1`](expressions.md#Eval_expr) yields `v` in state `s_2`.

Stepping `inst''*` in state `s` yields `inst*` in state [`s_2` with the object at `a` expanded by field `v`](abstract-operations.md#expand_heap), provided:

1. `inst''*` is a non-empty list.
1. Let `inst' :: inst*` be `inst''*`.
1. `inst'` matches pattern **`IExpand`** `_` `_`.
1. Let `IExpand ref e` be `inst'`.
1. [Resolving `ref` in state `s`](expressions.md#Eval_ref) yields `refTarget` in state `s_1`.
1. Let `value` be [the value `refTarget` reads to in state `s`](abstract-operations.md#read_ref).
1. `value` matches pattern **`AddrV`** `_`.
1. Let `AddrV a` be `value`.
1. [Evaluating `e` in state `s_1`](expressions.md#Eval_expr) yields `v` in state `s_2`.

## 단언 (`IAssert`)

```spectec
rule Step/IAssert:
  s |- (IAssert e :: inst*) ==> s_1 ; inst*
 -- Eval_expr: s |- e ==> BoolV true ; s_1

rule Step/IAssert-trap:
  s |- (IAssert e :: inst*) ==> s_1 ; (ITrap (AssertFail e) :: eps)
 -- Eval_expr: s |- e ==> v ; s_1
 -- if v =/= BoolV true
```

<a id="Step/IAssert"></a>
Stepping `inst''*` in state `s` yields `inst*` in state `s_1`, provided:

1. `inst''*` is a non-empty list.
1. Let `inst' :: inst*` be `inst''*`.
1. `inst'` matches pattern **`IAssert`** `_`.
1. Let `IAssert e` be `inst'`.
1. [Evaluating `e` in state `s`](expressions.md#Eval_expr) yields `value` in state `s_1`.
1. `value` is equal to **`BoolV`** `true`.

<a id="Step/IAssert-trap"></a>
Stepping `inst''*` in state `s` yields `ITrap AssertFail e :: ·` in state `s_1`, provided:

1. `inst''*` is a non-empty list.
1. Let `inst' :: inst*` be `inst''*`.
1. `inst'` matches pattern **`IAssert`** `_`.
1. Let `IAssert e` be `inst'`.
1. [Evaluating `e` in state `s`](expressions.md#Eval_expr) yields `v` in state `s_1`.
1. `v` is not equal to **`BoolV`** `true`.

## 효과

```spectec
rule Step/IPrint:
  s |- (IPrint e :: inst*) ==> $print_out(s_1, v) ; inst*
 -- Eval_expr: s |- e ==> v ; s_1

rule Step/INop:
  s |- (INop :: inst*) ==> s ; inst*

rule Step/IExpr:
  s |- (IExpr e :: inst*) ==> s_1 ; inst*
 -- Eval_expr: s |- e ==> _ ; s_1
```

<a id="Step/IPrint"></a>
Stepping `inst''*` in state `s` yields `inst*` in state [`s_1` with `v` appended to its output](abstract-operations.md#print_out), provided:

1. `inst''*` is a non-empty list.
1. Let `inst' :: inst*` be `inst''*`.
1. `inst'` matches pattern **`IPrint`** `_`.
1. Let `IPrint e` be `inst'`.
1. [Evaluating `e` in state `s`](expressions.md#Eval_expr) yields `v` in state `s_1`.

아무 일도 하지 않는 명령은 자기를 버리고, 식 명령은 식을 부수 효과 때문에 평가한
뒤 값을 버린다.

<a id="Step/INop"></a>
<a id="Step/IExpr"></a>
Stepping `inst''*` in state `s` yields `inst*` in state `s`, provided:

1. `inst''*` is a non-empty list.
1. Let `inst' :: inst*` be `inst''*`.
1. `inst'` matches pattern **`INop`**.

Stepping `inst''*` in state `s` yields `inst*` in state `s_1`, provided:

1. `inst''*` is a non-empty list.
1. Let `inst' :: inst*` be `inst''*`.
1. `inst'` matches pattern **`IExpr`** `_`.
1. Let `IExpr e` be `inst'`.
1. [Evaluating `e` in state `s`](expressions.md#Eval_expr) yields `_` in state `s_1`.

## 규칙이 상태에 닿는 곳

위 규칙들이 상태에 접근할 때는 모두 [추상 연산](abstract-operations.md)을 거친다.
호출이 들어갈 본문은 [`$body_of`](abstract-operations.md#body_of)이고, 필드를 읽는
것은 [`$read_obj`](abstract-operations.md#read_obj)이고, 리스트의 양 끝은
[`$push_obj`](abstract-operations.md#push_obj)와
[`$pop_obj`](abstract-operations.md#pop_obj)다.
