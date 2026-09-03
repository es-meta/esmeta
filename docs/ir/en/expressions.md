# Expressions

[← State](state.md) · [Instructions →](instructions.md)

Expression evaluation is big-step, so the relation takes a state and an
expression to a value and a state. It threads the state because an expression
can allocate, and because a call reached through an expression can print.

<a id="Eval_expr"></a>
`s ⊢ expr'' ⟹ % ; %`:
Evaluating `expr''` in state `s`

A sequence of expressions is evaluated left to right, threading the state
through, so an earlier allocation is visible to a later expression. Operand
lists get their own relation for that reason.

<a id="Eval_exprs"></a>
`s ⊢ expr* ⟹ % ; %`:
Evaluating `expr*` in state `s`

```spectec
rule Eval_exprs/nil:
  s |- eps ==> eps ; s

rule Eval_exprs/cons:
  s |- (e_h :: e_t*) ==> (v_h :: v_t*) ; s_2
 -- Eval_expr: s |- e_h ==> v_h ; s_1
 -- Eval_exprs: s_1 |- e_t* ==> v_t* ; s_2
```

<a id="Eval_exprs/nil"></a>
<a id="Eval_exprs/cons"></a>
Evaluating `expr*` in state `s` yields `·` in state `s`, provided:

1. `expr*` is an empty list.

Evaluating `expr*` in state `s` yields `v_h :: v_t*` in state `s_2`, provided:

1. `expr*` is a non-empty list.
1. Let `e_h :: e_t*` be `expr*`.
1. [Evaluating `e_h` in state `s`](expressions.md#Eval_expr) yields `v_h` in state `s_1`.
1. [Evaluating `e_t*` in state `s_1`](expressions.md#Eval_exprs) yields `v_t*` in state `s_2`.

Every form of `expr` appears below. Four of them have no rule at all, and those
are gathered at the end under [unmodelled forms](#unmodelled-forms).

## Literals

Literals are the base case, with no premises and no change to the state. Each
carries its payload straight into the value domain.

```spectec
rule Eval_expr/EBool:
  s |- EBool b ==> BoolV b ; s

rule Eval_expr/EStr:
  s |- EStr t ==> StringV t ; s

rule Eval_expr/EUndef:
  s |- EUndef ==> UndefV ; s

rule Eval_expr/ENull:
  s |- ENull ==> NullV ; s

rule Eval_expr/EEnum:
  s |- EEnum id ==> EnumV id ; s

rule Eval_expr/ECodeUnit:
  s |- ECodeUnit i ==> CodeUnitV i ; s

rule Eval_expr/EBigInt:
  s |- EBigInt i ==> BigIntV i ; s

rule Eval_expr/ENumber:
  s |- ENumber double ==> NumberV double ; s

rule Eval_expr/EMath:
  s |- EMath bigDecimal ==> MathV bigDecimal ; s

rule Eval_expr/EInfinity:
  s |- EInfinity b ==> InfinityV b ; s
```

<a id="Eval_expr/EBool"></a>
<a id="Eval_expr/EStr"></a>
<a id="Eval_expr/EUndef"></a>
<a id="Eval_expr/ENull"></a>
<a id="Eval_expr/EEnum"></a>
<a id="Eval_expr/ECodeUnit"></a>
<a id="Eval_expr/EBigInt"></a>
<a id="Eval_expr/ENumber"></a>
<a id="Eval_expr/EMath"></a>
<a id="Eval_expr/EInfinity"></a>
Evaluating `expr` in state `s` yields **`BoolV`** `b` in state `s`, provided:

1. `expr` matches pattern **`EBool`** `_`.
1. Let `EBool b` be `expr`.

Evaluating `expr` in state `s` yields **`StringV`** `t` in state `s`, provided:

1. `expr` matches pattern **`EStr`** `_`.
1. Let `EStr t` be `expr`.

Evaluating `expr` in state `s` yields **`UndefV`** in state `s`, provided:

1. `expr` matches pattern **`EUndef`**.

Evaluating `expr` in state `s` yields **`NullV`** in state `s`, provided:

1. `expr` matches pattern **`ENull`**.

Evaluating `expr` in state `s` yields **`EnumV`** `id` in state `s`, provided:

1. `expr` matches pattern **`EEnum`** `_`.
1. Let `EEnum id` be `expr`.

Evaluating `expr` in state `s` yields **`CodeUnitV`** `i` in state `s`, provided:

1. `expr` matches pattern **`ECodeUnit`** `_`.
1. Let `ECodeUnit i` be `expr`.

Evaluating `expr` in state `s` yields **`BigIntV`** `i` in state `s`, provided:

1. `expr` matches pattern **`EBigInt`** `_`.
1. Let `EBigInt i` be `expr`.

Evaluating `expr` in state `s` yields **`NumberV`** `double` in state `s`, provided:

1. `expr` matches pattern **`ENumber`** `_`.
1. Let `ENumber double` be `expr`.

Evaluating `expr` in state `s` yields **`MathV`** `bigDecimal` in state `s`, provided:

1. `expr` matches pattern **`EMath`** `_`.
1. Let `EMath bigDecimal` be `expr`.

Evaluating `expr` in state `s` yields **`InfinityV`** `b` in state `s`, provided:

1. `expr` matches pattern **`EInfinity`** `_`.
1. Let `EInfinity b` be `expr`.

`ENumber` needs no separate case for NaN. ESMeta canonicalises the payload
because a Scala `Double` has many NaN bit patterns, but
[`double`](syntax.md#values) has a single `NAN` case, so every NaN is already
that one value.

## Operators

A unary expression evaluates its operand and hands it to `$uop`. A binary
expression evaluates both operands, then hands the pair to `$bop`.

```spectec
rule Eval_expr/EUnary:
  s |- EUnary uop e ==> $uop(uop, v) ; s_1
 -- Eval_expr: s |- e ==> v ; s_1

rule Eval_expr/EBinary:
  s |- EBinary bop e_l e_r ==> $bop(bop, v_l, v_r) ; s_2
 -- if bop =/= And
 -- if bop =/= Or
 -- Eval_expr: s |- e_l ==> v_l ; s_1
 -- Eval_expr: s_1 |- e_r ==> v_r ; s_2
```

<a id="Eval_expr/EUnary"></a>
<a id="Eval_expr/EBinary"></a>
Evaluating `expr` in state `s` yields [the result of applying `uop` to `v`](abstract-operations.md#uop) in state `s_1`, provided:

1. `expr` matches pattern **`EUnary`** `_` `_`.
1. Let `EUnary uop e` be `expr`.
1. [Evaluating `e` in state `s`](expressions.md#Eval_expr) yields `v` in state `s_1`.

Evaluating `expr` in state `s` yields [the result of applying `bop` to `v_l` and `v_r`](abstract-operations.md#bop) in state `s_2`, provided:

1. `expr` matches pattern **`EBinary`** `_` `_` `_`.
1. Let `EBinary bop e_l e_r` be `expr`.
1. `bop` is not equal to **`And`**.
1. `bop` is not equal to **`Or`**.
1. [Evaluating `e_l` in state `s`](expressions.md#Eval_expr) yields `v_l` in state `s_1`.
1. [Evaluating `e_r` in state `s_1`](expressions.md#Eval_expr) yields `v_r` in state `s_2`.

`And` and `Or` are the exceptions, because they short-circuit, so they get
their own rules rather than going through `$bop`. Each has a short form that
never evaluates the right operand and a full form that does.

The full forms guard with `=/=` rather than matching the deciding boolean. That
is what makes a non-boolean left operand still evaluate the right one, effects
included, before `$bop` gets stuck on the pair.

```spectec
rule Eval_expr/EBinary-and-short:
  s |- EBinary And e_l e_r ==> BoolV false ; s_1
 -- Eval_expr: s |- e_l ==> BoolV false ; s_1

rule Eval_expr/EBinary-and-full:
  s |- EBinary And e_l e_r ==> $bop(And, v_l, v_r) ; s_2
 -- Eval_expr: s |- e_l ==> v_l ; s_1
 -- if v_l =/= BoolV false
 -- Eval_expr: s_1 |- e_r ==> v_r ; s_2

rule Eval_expr/EBinary-or-short:
  s |- EBinary Or e_l e_r ==> BoolV true ; s_1
 -- Eval_expr: s |- e_l ==> BoolV true ; s_1

rule Eval_expr/EBinary-or-full:
  s |- EBinary Or e_l e_r ==> $bop(Or, v_l, v_r) ; s_2
 -- Eval_expr: s |- e_l ==> v_l ; s_1
 -- if v_l =/= BoolV true
 -- Eval_expr: s_1 |- e_r ==> v_r ; s_2
```

<a id="Eval_expr/EBinary-and-short"></a>
<a id="Eval_expr/EBinary-and-full"></a>
Evaluating `expr` in state `s` yields **`BoolV`** `false` in state `s_1`, provided:

1. `expr` matches pattern **`EBinary`** `_` `_` `_`.
1. Let `EBinary bop e_l e_r` be `expr`.
1. `bop` matches pattern **`And`**.
1. [Evaluating `e_l` in state `s`](expressions.md#Eval_expr) yields `value` in state `s_1`.
1. `value` is equal to **`BoolV`** `false`.

Evaluating `expr` in state `s` yields [the result of applying **`And`** to `v_l` and `v_r`](abstract-operations.md#bop) in state `s_2`, provided:

1. `expr` matches pattern **`EBinary`** `_` `_` `_`.
1. Let `EBinary bop e_l e_r` be `expr`.
1. `bop` matches pattern **`And`**.
1. [Evaluating `e_l` in state `s`](expressions.md#Eval_expr) yields `v_l` in state `s_1`.
1. `v_l` is not equal to **`BoolV`** `false`.
1. [Evaluating `e_r` in state `s_1`](expressions.md#Eval_expr) yields `v_r` in state `s_2`.

<a id="Eval_expr/EBinary-or-short"></a>
<a id="Eval_expr/EBinary-or-full"></a>
Evaluating `expr` in state `s` yields **`BoolV`** `true` in state `s_1`, provided:

1. `expr` matches pattern **`EBinary`** `_` `_` `_`.
1. Let `EBinary bop e_l e_r` be `expr`.
1. `bop` matches pattern **`Or`**.
1. [Evaluating `e_l` in state `s`](expressions.md#Eval_expr) yields `value` in state `s_1`.
1. `value` is equal to **`BoolV`** `true`.

Evaluating `expr` in state `s` yields [the result of applying **`Or`** to `v_l` and `v_r`](abstract-operations.md#bop) in state `s_2`, provided:

1. `expr` matches pattern **`EBinary`** `_` `_` `_`.
1. Let `EBinary bop e_l e_r` be `expr`.
1. `bop` matches pattern **`Or`**.
1. [Evaluating `e_l` in state `s`](expressions.md#Eval_expr) yields `v_l` in state `s_1`.
1. `v_l` is not equal to **`BoolV`** `true`.
1. [Evaluating `e_r` in state `s_1`](expressions.md#Eval_expr) yields `v_r` in state `s_2`.

Variadic and mathematical operators take a whole operand list, so both reach it
through `Eval_exprs`.

```spectec
rule Eval_expr/EVariadic:
  s |- EVariadic vop e* ==> $vop(vop, v*) ; s_1
 -- Eval_exprs: s |- e* ==> v* ; s_1

rule Eval_expr/EMathOp:
  s |- EMathOp mop e* ==> $mop(mop, v*) ; s_1
 -- Eval_exprs: s |- e* ==> v* ; s_1
```

<a id="Eval_expr/EVariadic"></a>
<a id="Eval_expr/EMathOp"></a>
Evaluating `expr` in state `s` yields [the result of applying `vop` to `v*`](abstract-operations.md#vop) in state `s_1`, provided:

1. `expr` matches pattern **`EVariadic`** `_` `_`.
1. Let `EVariadic vop e*` be `expr`.
1. [Evaluating `e*` in state `s`](expressions.md#Eval_exprs) yields `v*` in state `s_1`.

Evaluating `expr` in state `s` yields [the result of applying `mop` to `v*`](abstract-operations.md#mop) in state `s_1`, provided:

1. `expr` matches pattern **`EMathOp`** `_` `_`.
1. Let `EMathOp mop e*` be `expr`.
1. [Evaluating `e*` in state `s`](expressions.md#Eval_exprs) yields `v*` in state `s_1`.

## Conversions

A conversion evaluates its operand and dispatches on the operator. Converting a
number or a big integer to a string also evaluates a radix, defaulting to ten
when none is written, so those two cases cannot go through `$cop` alone.

The three rules stay disjoint because
[`$to_str_radix`](abstract-operations.md#to_str_radix) is undefined for every
other operand, and `$cop` has no clause for those two.

```spectec
rule Eval_expr/EConvert:
  s |- EConvert cop e ==> $cop(cop, v) ; s_1
 -- Eval_expr: s |- e ==> v ; s_1

rule Eval_expr/EConvert-toStr-default:
  s |- EConvert (ToStr eps) e ==> $to_str_radix(v, 10) ; s_1
 -- Eval_expr: s |- e ==> v ; s_1

rule Eval_expr/EConvert-toStr-radix:
  s |- EConvert (ToStr e_r) e ==> $to_str_radix(v, n) ; s_2
 -- Eval_expr: s |- e ==> v ; s_1
 -- Eval_expr: s_1 |- e_r ==> v_r ; s_2
 -- if n = $as_int(v_r)
```

<a id="Eval_expr/EConvert"></a>
Evaluating `expr` in state `s` yields [the result of converting `v` by `cop`](abstract-operations.md#cop) in state `s_1`, provided:

1. `expr` matches pattern **`EConvert`** `_` `_`.
1. Let `EConvert cop e` be `expr`.
1. [Evaluating `e` in state `s`](expressions.md#Eval_expr) yields `v` in state `s_1`.

<a id="Eval_expr/EConvert-toStr-default"></a>
<a id="Eval_expr/EConvert-toStr-radix"></a>
Evaluating `expr` in state `s` yields [the text of `v` in radix `10`](abstract-operations.md#to_str_radix) in state `s_1`, provided:

1. `expr` matches pattern **`EConvert`** `_` `_`.
1. Let `EConvert cop e` be `expr`.
1. `cop` is equal to **`ToStr`** `·`.
1. [Evaluating `e` in state `s`](expressions.md#Eval_expr) yields `v` in state `s_1`.

Evaluating `expr'` in state `s` yields [the text of `v` in radix `n`](abstract-operations.md#to_str_radix) in state `s_2`, provided:

1. `expr'` matches pattern **`EConvert`** `_` `_`.
1. Let `EConvert cop e` be `expr'`.
1. `cop` matches pattern **`ToStr`** `_`.
1. Let `ToStr expr?` be `cop`.
1. `expr?` is defined.
1. Let `e_r` be `expr?`.
1. [Evaluating `e` in state `s`](expressions.md#Eval_expr) yields `v` in state `s_1`.
1. [Evaluating `e_r` in state `s_1`](expressions.md#Eval_expr) yields `v_r` in state `s_2`.
1. Let `int` be [the integer denoted by `v_r`](abstract-operations.md#as_int).
1. `int` has type `nat`.
1. Let `n` be `int`.

## Text

A substring takes a start and an optional end. Without an end it runs to the
end of the string, and the rule checks that the start is within range rather
than clamping it.

```spectec
rule Eval_expr/ESubstring-withTo:
  s |- ESubstring e_s e_f e_t ==> StringV (t[n_f : n_len]) ; s_3
 -- Eval_expr: s |- e_s ==> StringV t ; s_1
 -- Eval_expr: s_1 |- e_f ==> v_f ; s_2
 -- Eval_expr: s_2 |- e_t ==> v_t ; s_3
 -- if n_f = $as_int(v_f)
 -- if i_t = $as_int(v_t)
 -- if n_len = i_t - n_f

rule Eval_expr/ESubstring-withoutTo:
  s |- ESubstring e_s e_f eps ==> StringV (t[n_f : n_len]) ; s_2
 -- Eval_expr: s |- e_s ==> StringV t ; s_1
 -- Eval_expr: s_1 |- e_f ==> v_f ; s_2
 -- if n_f = $as_int(v_f)
 -- if n_f <= |t|
 -- if n_len = |t| - n_f

rule Eval_expr/ETrim:
  s |- ETrim e_s b ==> StringV t_1 ; s_1
 -- Eval_expr: s |- e_s ==> StringV t ; s_1
 -- if $trim_string(t, b) = t_1
```

<a id="Eval_expr/ESubstring-withTo"></a>
<a id="Eval_expr/ESubstring-withoutTo"></a>
Evaluating `expr'` in state `s` yields **`StringV`** `t[n_f : n_len]` in state `s_3`, provided:

1. `expr'` matches pattern **`ESubstring`** `_` `_` `_`.
1. Let `ESubstring e_s e_f expr?` be `expr'`.
1. `expr?` is defined.
1. Let `e_t` be `expr?`.
1. [Evaluating `e_s` in state `s`](expressions.md#Eval_expr) yields `value` in state `s_1`.
1. `value` matches pattern **`StringV`** `_`.
1. Let `StringV t` be `value`.
1. [Evaluating `e_f` in state `s_1`](expressions.md#Eval_expr) yields `v_f` in state `s_2`.
1. [Evaluating `e_t` in state `s_2`](expressions.md#Eval_expr) yields `v_t` in state `s_3`.
1. Let `int` be [the integer denoted by `v_f`](abstract-operations.md#as_int).
1. `int` has type `nat`.
1. Let `n_f` be `int`.
1. Let `i_t` be [the integer denoted by `v_t`](abstract-operations.md#as_int).
1. Let `int'` be `i_t` - `n_f`.
1. `int'` has type `nat`.
1. Let `n_len` be `int'`.

Evaluating `expr'` in state `s` yields **`StringV`** `t[n_f : n_len]` in state `s_2`, provided:

1. `expr'` matches pattern **`ESubstring`** `_` `_` `_`.
1. Let `ESubstring e_s e_f expr?` be `expr'`.
1. `expr?` is none.
1. [Evaluating `e_s` in state `s`](expressions.md#Eval_expr) yields `value` in state `s_1`.
1. `value` matches pattern **`StringV`** `_`.
1. Let `StringV t` be `value`.
1. [Evaluating `e_f` in state `s_1`](expressions.md#Eval_expr) yields `v_f` in state `s_2`.
1. Let `int` be [the integer denoted by `v_f`](abstract-operations.md#as_int).
1. `int` has type `nat`.
1. Let `n_f` be `int`.
1. `n_f` is less than or equal to the length of `t`.
1. Let `n_len` be the length of `t` - `n_f`.

<a id="Eval_expr/ETrim"></a>
Evaluating `expr` in state `s` yields **`StringV`** `t_1` in state `s_1`, provided:

1. `expr` matches pattern **`ETrim`** `_` `_`.
1. Let `ETrim e_s b` be `expr`.
1. [Evaluating `e_s` in state `s`](expressions.md#Eval_expr) yields `value` in state `s_1`.
1. `value` matches pattern **`StringV`** `_`.
1. Let `StringV t` be `value`.
1. Let `t_1` be [`t` with its leading run trimmed if `b` or its trailing run otherwise](abstract-operations.md#trim_string).

## Queries

These ask something about a value rather than building one. Size and membership
reach into the heap, so both read the object the address names.

```spectec
rule Eval_expr/ESizeOf:
  s |- ESizeOf e ==> $size_of(s_1, v_1) ; s_1
 -- Eval_expr: s |- e ==> v_1 ; s_1

rule Eval_expr/EContains:
  s |- EContains e_l e_q ==> BoolV (v_q <- v_b*) ; s_2
 -- Eval_expr: s |- e_l ==> AddrV a ; s_1
 -- Eval_expr: s_1 |- e_q ==> v_q ; s_2
 -- if $find_heap(s_2, a) = ListObj v_b*
```

<a id="Eval_expr/ESizeOf"></a>
<a id="Eval_expr/EContains"></a>
Evaluating `expr` in state `s` yields [the size of `v_1`](abstract-operations.md#size_of) in state `s_1`, provided:

1. `expr` matches pattern **`ESizeOf`** `_`.
1. Let `ESizeOf e` be `expr`.
1. [Evaluating `e` in state `s`](expressions.md#Eval_expr) yields `v_1` in state `s_1`.

Evaluating `expr` in state `s` yields **`BoolV`** `v_q is in v_b*` in state `s_2`, provided:

1. `expr` matches pattern **`EContains`** `_` `_`.
1. Let `EContains e_l e_q` be `expr`.
1. [Evaluating `e_l` in state `s`](expressions.md#Eval_expr) yields `value` in state `s_1`.
1. `value` matches pattern **`AddrV`** `_`.
1. Let `AddrV a` be `value`.
1. [Evaluating `e_q` in state `s_1`](expressions.md#Eval_expr) yields `v_q` in state `s_2`.
1. Let `obj'?` be [the object at `a` in the heap of `s_2`](abstract-operations.md#find_heap).
1. `obj'?` is defined.
1. Let `obj` be `obj'?`.
1. `obj` matches pattern **`ListObj`** `_`.
1. Let `ListObj v_b*` be `obj`.

`ETypeOf` names the type of a value, and `EInstanceOf` asks whether one value
is an instance of another, which for an AST means it was built from that
production.

```spectec
rule Eval_expr/ETypeOf:
  s |- ETypeOf e ==> StringV $type_of(s_1, v) ; s_1
 -- Eval_expr: s |- e ==> v ; s_1

rule Eval_expr/EInstanceOf:
  s |- EInstanceOf e_b e_t ==> BoolV $instance_of(v_b, v_t) ; s_2
 -- Eval_expr: s |- e_b ==> v_b ; s_1
 -- Eval_expr: s_1 |- e_t ==> v_t ; s_2
```

<a id="Eval_expr/ETypeOf"></a>
<a id="Eval_expr/EInstanceOf"></a>
Evaluating `expr` in state `s` yields **`StringV`** `$type_of(s_1, v)` in state `s_1`, provided:

1. `expr` matches pattern **`ETypeOf`** `_`.
1. Let `ETypeOf e` be `expr`.
1. [Evaluating `e` in state `s`](expressions.md#Eval_expr) yields `v` in state `s_1`.

Evaluating `expr` in state `s` yields **`BoolV`** `$instance_of(v_b, v_t)` in state `s_2`, provided:

1. `expr` matches pattern **`EInstanceOf`** `_` `_`.
1. Let `EInstanceOf e_b e_t` be `expr`.
1. [Evaluating `e_b` in state `s`](expressions.md#Eval_expr) yields `v_b` in state `s_1`.
1. [Evaluating `e_t` in state `s_1`](expressions.md#Eval_expr) yields `v_t` in state `s_2`.

## Type checks

`ETypeCheck` asks whether a value inhabits a type. It defers to
`$type_contains`, which has no clause for `YetType`, so asking about a type the
domain does not model leaves the program stuck rather than answering.

```spectec
rule Eval_expr/ETypeCheck:
  s |- ETypeCheck e type ==> BoolV $type_contains(s_1, type, v) ; s_1
 -- Eval_expr: s |- e ==> v ; s_1
```

<a id="Eval_expr/ETypeCheck"></a>
Evaluating `expr` in state `s` yields **`BoolV`** `$type_contains(s_1, type, v)` in state `s_1`, provided:

1. `expr` matches pattern **`ETypeCheck`** `_` `_`.
1. Let `ETypeCheck e type` be `expr`.
1. [Evaluating `e` in state `s`](expressions.md#Eval_expr) yields `v` in state `s_1`.

## References

A reference evaluates to a *target* rather than to a value, naming the place a
read or a write would land. Dereferencing it is a separate step, which is what
lets `IAssign` and `EExists` use the same relation as a plain read.

### Evaluating a reference

<a id="Eval_ref"></a>
`st ⊢ ref'' ⟹ % ; %`:
Resolving `ref''` in state `st`

```spectec
rule Eval_ref/variable:
  st |- variable ==> VarTarget variable ; st

rule Eval_ref/field:
  st |- Field ref expr ==> FieldTarget v_base v_field ; st_2
 -- Eval_ref: st |- ref ==> rt ; st_1
 -- if v_base = $read_ref(st_1, rt)
 -- Eval_expr: st_1 |- expr ==> v_field ; st_2
```

<a id="Eval_ref/variable"></a>
Resolving `ref` in state `st` yields **`VarTarget`** `variable` in state `st`, provided:

1. `ref` has type `variable`.
1. Let `variable` be `ref`.

<a id="Eval_ref/field"></a>
Resolving `ref'` in state `st` yields **`FieldTarget`** `v_base` `v_field` in state `st_2`, provided:

1. `ref'` matches pattern **`Field`** `_` `_`.
1. Let `Field ref expr` be `ref'`.
1. [Resolving `ref` in state `st`](expressions.md#Eval_ref) yields `rt` in state `st_1`.
1. Let `v_base` be [the value `rt` reads to in state `st_1`](abstract-operations.md#read_ref).
1. [Evaluating `expr` in state `st_1`](expressions.md#Eval_expr) yields `v_field` in state `st_2`.

<a id="refTarget"></a>
```spectec
syntax refTarget = 
   | {VarTarget variable}
   | {FieldTarget value value}
```

Reading through a target is [`$read_ref`](abstract-operations.md#read_ref), and
asking whether one resolves is
[`$exists_ref`](abstract-operations.md#exists_ref), a separate operation, so
that `EExists` does not have to read the value it is only testing for.

```spectec
rule Eval_expr/ERef:
  s |- ERef ref ==> $read_ref(s_1, refTarget) ; s_1
 -- Eval_ref: s |- ref ==> refTarget ; s_1

rule Eval_expr/EExists:
  s |- EExists ref ==> BoolV $exists_ref(s_1, refTarget) ; s_1
 -- Eval_ref: s |- ref ==> refTarget ; s_1
```

<a id="Eval_expr/ERef"></a>
<a id="Eval_expr/EExists"></a>
Evaluating `expr` in state `s` yields [the value `refTarget` reads to in state `s_1`](abstract-operations.md#read_ref) in state `s_1`, provided:

1. `expr` matches pattern **`ERef`** `_`.
1. Let `ERef ref` be `expr`.
1. [Resolving `ref` in state `s`](expressions.md#Eval_ref) yields `refTarget` in state `s_1`.

Evaluating `expr` in state `s` yields **`BoolV`** `$exists_ref(s_1, refTarget)` in state `s_1`, provided:

1. `expr` matches pattern **`EExists`** `_`.
1. Let `EExists ref` be `expr`.
1. [Resolving `ref` in state `s`](expressions.md#Eval_ref) yields `refTarget` in state `s_1`.

`$read_ref(state, refTarget)`:
The value `refTarget` reads to in state `state`

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

## Closures and continuations

A closure captures the variables it names. A continuation captures the whole
environment together with the call stack, and resuming one replaces the stack,
so it returns to where it was captured rather than to whoever resumed it.

```spectec
rule Eval_expr/EClo:
  s |- EClo id name* ==> CloV (CallV func $capture(s, name*)) ; s
 -- if $find_func(s, id) = func

rule Eval_expr/ECont:
  s |- ECont id ==> CloV (ContV func $capture_all(s) s.STACK) ; s
 -- if $find_func(s, id) = func
```

<a id="Eval_expr/EClo"></a>
<a id="Eval_expr/ECont"></a>
Evaluating `expr` in state `s` yields **`CloV`** `CallV func $capture(s, name*)` in state `s`, provided:

1. `expr` matches pattern **`EClo`** `_` `_`.
1. Let `EClo id name*` be `expr`.
1. Let `func'?` be [the function named `id` in `s`](abstract-operations.md#find_func).
1. `func'?` is defined.
1. Let `func` be `func'?`.

Evaluating `expr` in state `s` yields **`CloV`** `ContV func $capture_all(s) s.STACK` in state `s`, provided:

1. `expr` matches pattern **`ECont`** `_`.
1. Let `ECont id` be `expr`.
1. Let `func'?` be [the function named `id` in `s`](abstract-operations.md#find_func).
1. `func'?` is defined.
1. Let `func` be `func'?`.

## Allocation

Records, maps and lists live in the heap and evaluate to an address, so each
threads a new state out as well as in.

```spectec
rule Eval_expr/ERecord:
  s |- ERecord id (t_name : e_p)* ==> AddrV a ; s_2
 -- Eval_exprs: s |- e_p* ==> v_p* ; s_1
 -- if mtv = `{(t_name : v_p)*}
 -- if (a, s_2) = $alloc_record(s_1, id, mtv)

rule Eval_expr/EMap:
  s |- EMap (e_k : e_v)* ==> AddrV a ; s_3
 -- Eval_exprs: s |- e_k* ==> v_k* ; s_1
 -- Eval_exprs: s |- e_v* ==> v_v* ; s_2
 -- if mvv = `{(v_k : v_v)*}
 -- if (a, s_3) = $alloc_map(s_2, mvv)

rule Eval_expr/EList:
  s |- EList e* ==> AddrV a ; s_2
 -- Eval_exprs: s |- e* ==> v* ; s_1
 -- if (a, s_2) = $alloc_list(s_1, v*)
```

<a id="Eval_expr/ERecord"></a>
<a id="Eval_expr/EMap"></a>
<a id="Eval_expr/EList"></a>
Evaluating `expr` in state `s` yields **`AddrV`** `a` in state `s_2`, provided:

1. `expr` matches pattern **`ERecord`** `_` `_`.
1. Let `ERecord id ( t_name : e_p )*` be `expr`.
1. [Evaluating `e_p*` in state `s`](expressions.md#Eval_exprs) yields `v_p*` in state `s_1`.
1. Let `mtv` be `{( t_name : v_p )*}`.
1. Let `( a, s_2 )` be [a fresh address for the `id` record with fields `mtv` and the state that holds it](abstract-operations.md#alloc_record).

Evaluating `expr` in state `s` yields **`AddrV`** `a` in state `s_3`, provided:

1. `expr` matches pattern **`EMap`** `_`.
1. Let `EMap ( e_k : e_v )*` be `expr`.
1. [Evaluating `e_k*` in state `s`](expressions.md#Eval_exprs) yields `v_k*` in state `s_1`.
1. [Evaluating `e_v*` in state `s`](expressions.md#Eval_exprs) yields `v_v*` in state `s_2`.
1. Let `mvv` be `{( v_k : v_v )*}`.
1. Let `( a, s_3 )` be [a fresh address for the map `mvv` and the state that holds it](abstract-operations.md#alloc_map).

Evaluating `expr` in state `s` yields **`AddrV`** `a` in state `s_2`, provided:

1. `expr` matches pattern **`EList`** `_`.
1. Let `EList e*` be `expr`.
1. [Evaluating `e*` in state `s`](expressions.md#Eval_exprs) yields `v*` in state `s_1`.
1. Let `( a, s_2 )` be [a fresh address for the list `v*` and the state that holds it](abstract-operations.md#alloc_list).

Copying allocates a fresh address holding a copy of the object, and taking the
keys allocates a list of them.

```spectec
rule Eval_expr/ECopy:
  s |- ECopy e ==> AddrV a_new ; s_2
 -- Eval_expr: s |- e ==> AddrV a ; s_1
 -- if (a_new, s_2) = $copy_heap(s_1, a)

rule Eval_expr/EKeys:
  s |- EKeys e b ==> AddrV a_keylist ; s_2
 -- Eval_expr: s |- e ==> AddrV a ; s_1
 -- if (a_keylist, s_2) = $keys_heap(s_1, a, b)
```

<a id="Eval_expr/ECopy"></a>
<a id="Eval_expr/EKeys"></a>
Evaluating `expr` in state `s` yields **`AddrV`** `a_new` in state `s_2`, provided:

1. `expr` matches pattern **`ECopy`** `_`.
1. Let `ECopy e` be `expr`.
1. [Evaluating `e` in state `s`](expressions.md#Eval_expr) yields `value` in state `s_1`.
1. `value` matches pattern **`AddrV`** `_`.
1. Let `AddrV a` be `value`.
1. Let `( a_new, s_2 )` be [a fresh address holding a copy of `a` and the state that holds it](abstract-operations.md#copy_heap).

Evaluating `expr` in state `s` yields **`AddrV`** `a_keylist` in state `s_2`, provided:

1. `expr` matches pattern **`EKeys`** `_` `_`.
1. Let `EKeys e b` be `expr`.
1. [Evaluating `e` in state `s`](expressions.md#Eval_expr) yields `value` in state `s_1`.
1. `value` matches pattern **`AddrV`** `_`.
1. Let `AddrV a` be `value`.
1. Let `( a_keylist, s_2 )` be [a fresh address holding the keys of `a` sorted numerically if `b` and the state that holds it](abstract-operations.md#keys_heap).

## AST and grammar

Building an [AST node](syntax.md#syntax-trees) allocates an identity, so both
construction rules thread the state through
[`$fresh_ast_id`](abstract-operations.md#fresh_ast_id). A syntactic node
evaluates only the children that are present, then puts the holes back.

```spectec
rule Eval_expr/ESyntactic:
  s |- ESyntactic id b* n (e?)* ==> AstV (AST n_id id eps $init_syntactic(id, b*, n, (ast?)*)) ; s_2
 -- Eval_exprs: s |- $filter_some_<expr>((e?)*) ==> v* ; s_1
 -- if (ast?)* = $reinsert_holes((e?)*, v*)
 -- if (n_id, s_2) = $fresh_ast_id(s_1)

rule Eval_expr/ELexical:
  s |- ELexical id e ==> AstV (AST n_id id eps $init_lexical(id, t)) ; s_2
 -- Eval_expr: s |- e ==> StringV t ; s_1
 -- if (n_id, s_2) = $fresh_ast_id(s_1)
```

<a id="Eval_expr/ESyntactic"></a>
<a id="Eval_expr/ELexical"></a>
Evaluating `expr` in state `s` yields **`AstV`** `AST n_id id · $init_syntactic(id, b*, n, ast?*)` in state `s_2`, provided:

1. `expr` matches pattern **`ESyntactic`** `_` `_` `_` `_`.
1. Let `ESyntactic id b* n e?*` be `expr`.
1. [Evaluating the defined elements of `e?*` in state `s`](expressions.md#Eval_exprs) yields `v*` in state `s_1`.
1. Let `ast?*` be [the values `v*` put back into the holes of `e?*`](abstract-operations.md#reinsert_holes).
1. Let `( n_id, s_2 )` be [the next AST serial number of `s_1` and the state that follows it](abstract-operations.md#fresh_ast_id).

Evaluating `expr` in state `s` yields **`AstV`** `AST n_id id · $init_lexical(id, t)` in state `s_2`, provided:

1. `expr` matches pattern **`ELexical`** `_` `_`.
1. Let `ELexical id e` be `expr`.
1. [Evaluating `e` in state `s`](expressions.md#Eval_expr) yields `value` in state `s_1`.
1. `value` matches pattern **`StringV`** `_`.
1. Let `StringV t` be `value`.
1. Let `( n_id, s_2 )` be [the next AST serial number of `s_1` and the state that follows it](abstract-operations.md#fresh_ast_id).

A grammar symbol is a value rather than a lookup. `|Name|` names a production
and the flags are the arguments it is taken with. What reads one is
[`$instance_of`](abstract-operations.md#instance_of), which asks whether an AST
was built from that production.

```spectec
rule Eval_expr/EGrammarSymbol:
  s |- EGrammarSymbol t b* ==> GrammarSymbolV t b* ; s
```

<a id="Eval_expr/EGrammarSymbol"></a>
Evaluating `expr` in state `s` yields **`GrammarSymbolV`** `t` `b*` in state `s`, provided:

1. `expr` matches pattern **`EGrammarSymbol`** `_` `_`.
1. Let `EGrammarSymbol t b*` be `expr`.

## Nondeterminism

The one expression whose result is not determined by the state.

```spectec
rule Eval_expr/ERandom:
  s |- ERandom ==> NumberV $random_double ; s
```

<a id="Eval_expr/ERandom"></a>
Evaluating `expr` in state `s` yields **`NumberV`** `$random_double` in state `s`, provided:

1. `expr` matches pattern **`ERandom`**.

## <a id="unmodelled-forms"></a>Unmodelled forms

Four forms of `expr` are declared in the syntax and have no rule at all.

| Form | What it would do |
| --- | --- |
| `EParse` | Parse a string against a grammar symbol |
| `ESourceText` | Recover the source text an AST was built from |
| `EDebug` | Print a value for debugging |
| `EYet` | Stand in for a construct ESMeta has not compiled |

Reaching any of them leaves the program [stuck](running.md#stopping), the same
way an unmodelled type does. That is deliberate. A rule that answered them
would be inventing behaviour the specification does not have.
