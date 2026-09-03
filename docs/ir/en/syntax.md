# Syntax

[← Index](index.md) · [State →](state.md)

Names are plain text throughout, for functions, fields, productions and
variables alike.

<a id="id"></a>
```spectec
syntax id = text
```

## Functions and parameters

A function carries a main flag, its kind, its name, its parameters, its return
type, and its body. A parameter is a name, a type, and whether it is optional.

<a id="funcKind"></a>
<a id="param"></a>
```spectec
syntax func = 
   | {bool funcKind id (param)* type inst}

syntax funcKind = 
   | NumMeth
   | SynDirOp
   | ConcMeth
   | InternalMeth
   | Builtin
   | Clo
   | Cont
   | Aux
   | AbsOp

syntax param = 
   | {id type bool}
```

## Instructions

Instructions are the statement level, covering branching, calls, and the
ordinary effects. `IBlock` groups a sequence; `IIf` and `IWhile` branch on a condition;
`ICall` and `ISdoCall` transfer control into another function.

`ITrap` is administrative, and the compiler never produces one. It is how the
specification writes down that a program stopped for a stated reason; see
[stopping](running.md#stopping).

<a id="inst"></a>
```spectec
syntax inst = 
   | {IBlock (inst)*}
   | {IIf expr inst inst?}
   | {IWhile expr inst}
   | {ICall local expr (expr)*}
   | {ISdoCall local expr text (expr)*}
   | {ILet name expr}
   | {IExpand ref expr}
   | {IDelete ref expr}
   | {IPush expr expr bool}
   | {IPop local expr bool}
   | {IReturn expr}
   | {IAssert expr}
   | {ITrap trap}
   | {IPrint expr}
   | INop
   | {IAssign ref expr}
   | {IExpr expr}

syntax trap = 
   | {AssertFail expr}
   | NoReturn
   | {NoCallable value}
   | {NoBool value}
```

## Expressions

<a id="expr"></a>
```spectec
syntax expr = 
   | {EParse expr expr}
   | {EGrammarSymbol text (bool)*}
   | {ESourceText expr}
   | {EYet text}
   | {EContains expr expr}
   | {ESubstring expr expr expr?}
   | {ETrim expr bool}
   | {EUnary uop expr}
   | {EBinary bop expr expr}
   | {EVariadic vop expr*}
   | {EMathOp mop expr*}
   | {EConvert cop expr}
   | {EExists ref}
   | {ETypeOf expr}
   | {EInstanceOf expr expr}
   | {ETypeCheck expr type}
   | {ESizeOf expr}
   | {EClo id name*}
   | {ECont id}
   | {EDebug expr}
   | ERandom
   | {ELexical id expr}
   | {ESyntactic id (bool)* nat (expr?)*}
   | {ERecord id pair<text, expr>*}
   | {EMap pair<expr, expr>*}
   | {EList expr*}
   | {ECopy expr}
   | {EKeys expr bool}
   | EUndef
   | ENull
   | {ERef ref}
   | {EBigInt int}
   | {ENumber double}
   | {EMath bigDecimal}
   | {EInfinity bool}
   | {EStr text}
   | {EBool bool}
   | {EEnum id}
   | {ECodeUnit int}
```

## References

A reference names a storage location, either a variable or a field of
something a reference already reaches. Variables are split three ways because
they live in different places, with globals in the state's global environment
and both flavours of local in the current context.

<a id="ref"></a>
<a id="variable"></a>
<a id="global"></a>
<a id="local"></a>
<a id="name"></a>
<a id="temp"></a>
```spectec
syntax ref = 
   | {Field ref expr}
   | variable

syntax variable = 
   | global
   | local

syntax global = 
   | {Global id}

syntax local = 
   | temp
   | name

syntax name = 
   | {Name id}

syntax temp = 
   | {Temp int}
```

## Operators

Unary, binary, variadic, mathematical, and conversion operators are separate
syntactic classes, each evaluated by its own auxiliary function.

```spectec
syntax uop = 
   | Abs
   | Floor
   | Neg
   | Not
   | BNot

syntax bop = 
   | Add
   | Sub
   | Pow
   | Mul
   | Div
   | Mod
   | Equal
   | Eq
   | And
   | Or
   | Xor
   | BAnd
   | BOr
   | BXOr
   | LShift
   | Lt
   | RShift

syntax vop = 
   | Min
   | Max
   | Concat

syntax mop = 
   | Expm1
   | Log10
   | Log2
   | Cos
   | Cbrt
   | Exp
   | Cosh
   | Sinh
   | Tanh
   | Acos
   | Acosh
   | Asinh
   | Atanh
   | Asin
   | Atan2
   | Atan
   | Log1p
   | Log
   | Sin
   | Sqrt
   | Tan

syntax cop = 
   | ToApproxNumber
   | ToNumber
   | ToBigInt
   | ToMath
   | {ToStr expr?}
   | ToCodeUnit
```

## Syntax trees

An AST node carries an identity, the production it was built from, its parent,
and either a syntactic or a lexical payload. The leading `nat` is what makes
`=` on two nodes mean ESMeta's reference comparison rather than structural
equality; see [AST identity](state.md#ast-identity).

<a id="ast"></a>
<a id="astValue"></a>
<a id="astSyntactic"></a>
<a id="astLexical"></a>
```spectec
syntax ast = 
   | {AST nat id ast? astValue}

syntax astValue = 
   | {SYN astSyntactic}
   | {LEX astLexical}

syntax astSyntactic = {NAME id, ARGS bool*, RHSIDX nat, SUBIDX nat, CHILDREN (ast?)*, PARENT ast?}

syntax astLexical = {NAME id, STR text, CHILDREN (ast?)*, PARENT ast?}
```

A syntactic node records the arguments its production was taken with and which
right-hand side was chosen. `SUBIDX` is computed once at construction rather
than on every dispatch, because the children are fixed when the node is built.

## Values

A value is anything an expression can evaluate to. `callable` is the shape a
closure or a continuation carries.

<a id="value"></a>
<a id="callable"></a>
```spectec
syntax value = 
   | {StringV text}
   | {BoolV bool}
   | {IntV int}
   | {AstV ast}
   | {MathV bigDecimal}
   | {NumberV double}
   | {BigIntV int}
   | {InfinityV bool}
   | {GrammarSymbolV text bool*}
   | {EnumV text}
   | {CodeUnitV int}
   | {AddrV addr}
   | {CloV callable}
   | UndefV
   | NullV

syntax callable = 
   | {ContV func map<name, value> callctx*}
   | {CallV func map<name, value>}
```

Numbers are the part worth reading closely. A `double` is stored as its three
raw IEEE-754 binary64 fields rather than as a machine float, so that structural
equality on `double` is bit-pattern equality. That is what
`java.lang.Double.equals` does, and what ESMeta compares with. Zero is
`FIN POS 0 0` and negation is a sign-bit flip.

<a id="double"></a>
<a id="sign"></a>
<a id="bigDecimal"></a>
```spectec
syntax double = 
   | {FIN sign nat int}
   | {INF sign}
   | NAN

syntax sign = 
   | POS
   | NEG

syntax bigDecimal = 
   | {BigDec int int}
```

## Types

A type is what a `ETypeCheck` expression can ask about. ESMeta's `ValueTy` is a
seventeen-component lattice whose questions this cannot answer in general, but
the specification only ever asks whether one value is of one type, over a small
set of types.

What these cases do not express stays `YetType`, and `$type_contains` has no
clause for it, so an unmodelled type leaves the program **stuck** rather than
quietly answered.

<a id="type"></a>
<a id="primType"></a>
```spectec
syntax type = 
   | {RecordT text text*}
   | {CompletionT id}
   | {AstT text*}
   | {ListT type}
   | {EnumT id*}
   | {PrimT primType}
   | {UnionT type type}
   | {YetType text}

syntax primType = 
   | StringT
   | NumberT
   | NumberIntT
   | BigIntT
   | BoolT
   | MathT
   | IntT
   | CodeUnitT
   | UndefinedT
   | NullT
```
