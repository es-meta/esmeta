# Transparent Builtins

[← Abstract Operations](abstract-operations.md) · [Index](index.md)

A transparent builtin is an operation the specification declares but does not
define. One is warranted when the operation cannot be expressed here at all,
not merely when expressing it would be tedious. Three reasons recur.

- `^` parses and renders but is absent from the numeric operators, so `$pow10`
  and `$ipow` stand in for it.
- `text` is byte-indexed, so `t[n]` is a byte and cannot yield a code unit.
- IEEE-754 and `BigDecimal` semantics belong to `Double` and `java.math`, and
  restating them structurally would be a separate specification.

Answering them is split in two. The polymorphic container operations and three
of the text ones come from the standard library that every target shares, while
the rest carry semantics specific to IRes and are answered alongside the
interpreter in `targets/ires/builtins`. Each of the latter says which host
feature it stands for, so that a reader can check the declaration against the
thing it stands for.

## Sequences, sets, and maps

Answered by the standard library rather than by IRes itself.

### $rev_

<a id="rev_"></a>
`$rev_<X>(X*)`:
The reversal of `X*`

```spectec
builtin dec $rev_<X>(X*) : X*
```

### $union_set

<a id="union_set"></a>
`$union_set<K>(set<K>, set<K>')`:
The union of the sets `set<K>` and `set<K>'`

```spectec
builtin dec $union_set<K>(set<K>, set<K>) : set<K>
```

### $diff_set

<a id="diff_set"></a>
`$diff_set<K>(set<K>, set<K>')`:
The difference of the sets `set<K>` and `set<K>'`

```spectec
builtin dec $diff_set<K>(set<K>, set<K>) : set<K>
```

### $find_map

<a id="find_map"></a>
`$find_map<K, V>(map<K, V>, K)`:
The value of `K` in map `map<K, V>`

```spectec
builtin dec $find_map<K, V>(map<K, V>, K) : V?
```

### $add_map

<a id="add_map"></a>
`$add_map<K, V>(map<K, V>, K, V)`:
The map `map<K, V>` with `K` bound to `V`

```spectec
builtin dec $add_map<K, V>(map<K, V>, K, V) : map<K, V>
```

### $update_map

<a id="update_map"></a>
`$update_map<K, V>(map<K, V>, K, V)`:
The map `map<K, V>` with the value for `K` updated to `V`

```spectec
builtin dec $update_map<K, V>(map<K, V>, K, V) : map<K, V>
```

### $del_map

<a id="del_map"></a>
`$del_map<K, V>(map<K, V>, K)`:
The map `map<K, V>` with the binding for `K` removed

```spectec
builtin dec $del_map<K, V>(map<K, V>, K) : map<K, V>
```

## Text and code units

`$int_to_text`, `$cps_of_text` and `$text_of_cps` also come from the standard
library, while the two code-unit operations are IRes' own.

### $int_to_text

<a id="int_to_text"></a>
`$int_to_text(nat)`:
The decimal notation of `nat`

```spectec
builtin dec $int_to_text(nat) : text
```

### $code_units_of_text

<a id="code_units_of_text"></a>
`$code_units_of_text(text)`:
The UTF-16 code units of `text`

```spectec
builtin dec $code_units_of_text(text) : nat*
```

Equivalent to reading every index of the Java String with charAt.

### $text_of_code_units

<a id="text_of_code_units"></a>
`$text_of_code_units(nat*)`:
The string formed from the UTF-16 code units `nat*`

```spectec
builtin dec $text_of_code_units(nat*) : text
```

Java's new String(char[]) over UTF-16 code units, the inverse of the above.

### $cps_of_text

<a id="cps_of_text"></a>
`$cps_of_text(text)`:
The code points of `text`

```spectec
builtin dec $cps_of_text(text) : nat*
```

Java's String.codePoints, which pairs a surrogate pair into one code point.

### $text_of_cps

<a id="text_of_cps"></a>
`$text_of_cps(nat*)`:
The string formed from the code points `nat*`

```spectec
builtin dec $text_of_cps(nat*) : text
```

Java's new String(int[]), which splits a supplementary code point back into a surrogate pair.

## Integer arithmetic

### $pow10

<a id="pow10"></a>
`$pow10(nat)`:
10 raised to the power of `nat`

```spectec
builtin dec $pow10(nat) : int
```

Stands in for `^`, which SpecTec parses and renders but does not evaluate.

### $ipow

<a id="ipow"></a>
`$ipow(int, nat)`:
`int` raised to the power of `nat`

```spectec
builtin dec $ipow(int, nat) : int
```

Stands in for `^`, which SpecTec parses and renders but does not evaluate.

### $band

<a id="band"></a>
`$band(int, int')`:
The bitwise and of `int` and `int'`

```spectec
builtin dec $band(int, int) : int
```

Java's BigInteger.and; SpecTec has no bitwise operators.

### $bor

<a id="bor"></a>
`$bor(int, int')`:
The bitwise or of `int` and `int'`

```spectec
builtin dec $bor(int, int) : int
```

Java's BigInteger.or; SpecTec has no bitwise operators.

### $bxor

<a id="bxor"></a>
`$bxor(int, int')`:
The bitwise exclusive or of `int` and `int'`

```spectec
builtin dec $bxor(int, int) : int
```

Java's BigInteger.xor; SpecTec has no bitwise operators.

### $shl

<a id="shl"></a>
`$shl(int, nat)`:
The left shift of `int` by `nat` bits

```spectec
builtin dec $shl(int, nat) : int
```

Java's BigInteger.shiftLeft.

### $shr

<a id="shr"></a>
`$shr(int, nat)`:
The arithmetic right shift of `int` by `nat` bits

```spectec
builtin dec $shr(int, nat) : int
```

Java's BigInteger.shiftRight, an arithmetic shift that keeps the sign.

### $as_int32

<a id="as_int32"></a>
`$as_int32(int)`:
The low 32 bits of `int` as a signed integer

```spectec
builtin dec $as_int32(int) : int
```

The truncation ECMAScript's ToInt32 performs, as Java's intValue does it.

### $as_uint16

<a id="as_uint16"></a>
`$as_uint16(int)`:
The low 16 bits of `int` as an unsigned integer

```spectec
builtin dec $as_uint16(int) : int
```

The same truncation to sixteen unsigned bits, for ToUint16.

## Mathematical values

### $floor_bigdec

<a id="floor_bigdec"></a>
`$floor_bigdec(bigDecimal)`:
The greatest integer not greater than `bigDecimal`

```spectec
builtin dec $floor_bigdec(bigDecimal) : bigDecimal
```

Java.math.BigDecimal.setScale with FLOOR rounding.

### $div_bigdec

<a id="div_bigdec"></a>
`$div_bigdec(bigDecimal, bigDecimal')`:
The quotient of `bigDecimal` and `bigDecimal'` rounded to DECIMAL128

```spectec
builtin dec $div_bigdec(bigDecimal, bigDecimal) : bigDecimal
```

Rounding is what terminates a non-terminating expansion such as 1 / 3.

### $pow_bigdec

<a id="pow_bigdec"></a>
`$pow_bigdec(bigDecimal, bigDecimal')`:
`bigDecimal` raised to the power of `bigDecimal'` computed in double precision

```spectec
builtin dec $pow_bigdec(bigDecimal, bigDecimal) : bigDecimal
```

Math.pow on both operands as doubles; BigDecimal has no fractional exponent.

### $as_valid_nat

<a id="as_valid_nat"></a>
`$as_valid_nat(bigDecimal)`:
`bigDecimal` as a natural number, if it is integral, non-negative, and within 32 bits

```spectec
builtin dec $as_valid_nat(bigDecimal) : nat?
```

The guard ESMeta applies before using a mathematical value as an index.

## Transcendental operations

### $math_un

<a id="math_un"></a>
`$math_un(mop, bigDecimal)`:
The result of applying `mop` to `bigDecimal` in double precision

```spectec
builtin dec $math_un(mop, bigDecimal) : bigDecimal
```

The unary functions of java.lang.Math, applied to the operand as a double.

### $math_atan2

<a id="math_atan2"></a>
`$math_atan2(bigDecimal, bigDecimal')`:
The two-argument arctangent of `bigDecimal` and `bigDecimal'` in double precision

```spectec
builtin dec $math_atan2(bigDecimal, bigDecimal) : bigDecimal
```

Java.lang.Math.atan2.

## IEEE-754 doubles

### $add_double

<a id="add_double"></a>
`$add_double(double, double')`:
The sum of `double` and `double'`

```spectec
builtin dec $add_double(double, double) : double
```

IEEE-754 binary64 addition, as Java's + on double performs it.

### $sub_double

<a id="sub_double"></a>
`$sub_double(double, double')`:
The difference of `double` and `double'`

```spectec
builtin dec $sub_double(double, double) : double
```

IEEE-754 binary64 subtraction, as Java's - on double performs it.

### $mul_double

<a id="mul_double"></a>
`$mul_double(double, double')`:
The product of `double` and `double'`

```spectec
builtin dec $mul_double(double, double) : double
```

IEEE-754 binary64 multiplication, as Java's * on double performs it.

### $div_double

<a id="div_double"></a>
`$div_double(double, double')`:
The quotient of `double` and `double'`

```spectec
builtin dec $div_double(double, double) : double
```

IEEE-754 binary64 division, as Java's / on double performs it.

### $pow_double

<a id="pow_double"></a>
`$pow_double(double, double')`:
`double` raised to the power of `double'`

```spectec
builtin dec $pow_double(double, double) : double
```

Java.lang.Math.pow.

### $mod_double

<a id="mod_double"></a>
`$mod_double(double, double')`:
The remainder of `double` and `double'` taking the sign of the divisor

```spectec
builtin dec $mod_double(double, double) : double
```

ESMeta's %%, which adjusts Java's % so the result takes the sign of the divisor.

### $lt_double

<a id="lt_double"></a>
`$lt_double(double, double')`:
`double` is less than `double'`

```spectec
builtin dec $lt_double(double, double) : bool
```

IEEE-754 comparison, under which every comparison with NaN is false.

### $numeq_double

<a id="numeq_double"></a>
`$numeq_double(double, double')`:
`double` and `double'` are numerically equal

```spectec
builtin dec $numeq_double(double, double) : bool
```

This is the primitive comparison, under which NaN differs from itself and the two zeroes agree. Structural equality on double is the other one, matching Double.equals.

## Conversions

### $double_of_int

<a id="double_of_int"></a>
`$double_of_int(int)`:
`int` as a double

```spectec
builtin dec $double_of_int(int) : double
```

Java's widening of BigInteger to double, rounding to nearest.

### $as_int_of_double

<a id="as_int_of_double"></a>
`$as_int_of_double(double)`:
`double` truncated toward zero to a 32-bit signed integer

```spectec
builtin dec $as_int_of_double(double) : int
```

The truncation ECMAScript's ToInt32 performs on a double.

### $double_of_bigdec

<a id="double_of_bigdec"></a>
`$double_of_bigdec(bigDecimal)`:
`bigDecimal` as a double

```spectec
builtin dec $double_of_bigdec(bigDecimal) : double
```

Java.math.BigDecimal.doubleValue.

### $bigdec_of_double

<a id="bigdec_of_double"></a>
`$bigdec_of_double(double)`:
`double` as a mathematical value

```spectec
builtin dec $bigdec_of_double(double) : bigDecimal
```

New java.math.BigDecimal(double), which is exact on the bit pattern.

### $bigint_of_double

<a id="bigint_of_double"></a>
`$bigint_of_double(double)`:
The integral part of `double`

```spectec
builtin dec $bigint_of_double(double) : int
```

Java.math.BigDecimal.toBigInteger, truncating toward zero.

### $str_to_number

<a id="str_to_number"></a>
`$str_to_number(text)`:
The result of parsing `text` as a StringNumericLiteral

```spectec
builtin dec $str_to_number(text) : double
```

Parsing failure yields NaN, so this is total.

### $str_to_bigint

<a id="str_to_bigint"></a>
`$str_to_bigint(text)`:
The result of parsing `text` as a StringIntegerLiteral

```spectec
builtin dec $str_to_bigint(text) : value
```

Parsing failure yields undefined, so the result is a value rather than an integer.

### $number_to_text

<a id="number_to_text"></a>
`$number_to_text(double, nat)`:
The string representation of `double` in radix `nat`

```spectec
builtin dec $number_to_text(double, nat) : text
```

ECMAScript's Number::toString, which is not Java's Double.toString.

### $bigint_to_text

<a id="bigint_to_text"></a>
`$bigint_to_text(int, nat)`:
The string representation of `int` in radix `nat`

```spectec
builtin dec $bigint_to_text(int, nat) : text
```

Java's BigInteger.toString(radix).

## Type model

### $is_subty_record

<a id="is_subty_record"></a>
`$is_subty_record(text, text')`:
The record type `text` is `text'` or a subtype of it

```spectec
builtin dec $is_subty_record(text, text) : bool
```

The record type hierarchy comes from the type declarations of the specification, not from the state.

## Grammar

### $ast_production_idx

<a id="ast_production_idx"></a>
`$ast_production_idx(text, nat, text')`:
The index of the child named `text'` in production `text` alternative `nat`

```spectec
builtin dec $ast_production_idx(text, nat, text) : nat?
```

A lookup into ESMeta's compiled grammar, which the IR does not carry.

### $ast_optionals

<a id="ast_optionals"></a>
`$ast_optionals(text, nat)`:
Which nonterminals of production `text` alternative `nat` are optional

```spectec
builtin dec $ast_optionals(text, nat) : bool*
```

Rhs.ntsWithOptional over that same grammar; empty for an unknown production, matching ESMeta's fallback.

## Misc

### $random_double

<a id="random_double"></a>
`$random_double`:
Random double

```spectec
builtin dec $random_double : double
```

Java.lang.Math.random. The only operation here that is not a function: two calls may differ.
