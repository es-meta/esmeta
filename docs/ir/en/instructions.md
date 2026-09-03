# Instructions

[← Expressions](expressions.md) · [Running a program →](running.md)

Instruction execution is fully small-step, and it follows ESMeta's
`Interpreter.step`. The configuration is a state *and* the sequence left to
run.

<a id="Step"></a>
`s ⊢ inst''''* ⟹ % ; %`:
Stepping `inst''''*` in state `s`

Taking the sequence as an argument rather than reading it off the state is what
lets a rule pattern-match the instruction it is about. No rule has to phrase
itself as a condition on a cursor, and none has to move one.

## Blocks (`IBlock`)

A block splices its body in front of what is left to run, so grouping costs
nothing at run time and needs no frame of its own.

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

## Bindings and assignment (`ILet`, `IAssign`)

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

Assignment goes through the [reference rules](expressions.md#references), so
there is one rule per kind of target rather than one per kind of syntax.

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

## Branching (`IIf`, `IWhile`)

`IIf` evaluates its condition and continues with one branch or the other. A
condition that is not a boolean is an instruction-level failure, so it traps
rather than sticking.

An `IIf` with no else branch continues with what follows when the condition is
false, so the two false cases are separate rules rather than one rule with an
empty branch.

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

`IWhile` unrolls, so a true condition rewrites to the body followed by the loop
itself and the loop needs no auxiliary relation.

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

A loop condition that is neither boolean traps, exactly as a branch condition
does.

<a id="Step/IWhile-trap"></a>
Stepping `inst''*` in state `s` yields `ITrap NoBool v :: ·` in state `s_1`, provided:

1. `inst''*` is a non-empty list.
1. Let `inst' :: inst*` be `inst''*`.
1. `inst'` matches pattern **`IWhile`** `_` `_`.
1. Let `IWhile e inst_b` be `inst'`.
1. [Evaluating `e` in state `s`](expressions.md#Eval_expr) yields `v` in state `s_1`.
1. `v` is not equal to **`BoolV`** `true`.
1. `v` is not equal to **`BoolV`** `false`.

## Calls and returns (`ICall`, `IReturn`)

A call suspends the caller. Its context, the instructions it has left, and the
local its result lands in are pushed as a `callctx`, and the callee's body
becomes the sequence to run.

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

Calling a continuation replaces the stack outright rather than pushing onto it,
which is what makes it return to where it was captured. Calling something that
is not callable traps.

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

A syntax-directed call resolves the operation against the AST's production
chain first, then enters it with the AST as its leading argument.

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

A return pops the stack, binds the result into the local the caller reserved,
and resumes the caller at the cursor it recorded.

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

Returning from the outermost frame is different, because there is no caller to
resume and the program is simply finished.

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

Running off the end of a callee without returning is a failure, not a silent
completion.

```spectec
rule Step/exit-noreturn:
  s |- eps ==> s ; (ITrap NoReturn :: eps)
 -- if s.STACK =/= eps
```

<a id="Step/exit-noreturn"></a>
Stepping `inst*` in state `s` yields `ITrap NoReturn :: ·` in state `s`, provided:

1. `inst*` is an empty list.
1. `s.STACK` is not equal to `·`.

## Heap updates (`IPush`, `IPop`, `IDelete`, `IExpand`)

These four change an object in place rather than allocating, so each evaluates
its operands and hands the address to a heap operation. Pushing and popping
take a flag saying which end of the list to work on.

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

Deleting removes a field or key, and expanding adds one.

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

## Assertions (`IAssert`)

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

## Effects

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

A no-op discards itself, and an expression statement evaluates its expression
for effect and discards the value.

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

## Where the rules reach the state

Every state access in the rules above goes through an operation in
[Abstract Operations](abstract-operations.md). The body a call enters is
[`$body_of`](abstract-operations.md#body_of), a field read is
[`$read_obj`](abstract-operations.md#read_obj), and the list ends are
[`$push_obj`](abstract-operations.md#push_obj) and
[`$pop_obj`](abstract-operations.md#pop_obj).
