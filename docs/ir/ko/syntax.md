# 구문

[← 목차](index.md) · [상태 →](state.md)

이름은 함수든 필드든 생성 규칙이든 변수든 모두 그냥 문자열이다.

<a id="id"></a>
```spectec
syntax id = text
```

## 함수와 매개변수

함수에는 main 표시와 종류, 이름, 매개변수, 반환 타입, 본문이 들어 있다.
매개변수는 이름과 타입, 그리고 선택적인지 여부로 이루어진다.

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

## 명령

명령은 문장에 해당한다. 분기와 호출, 그 밖의 보통 효과들이 여기 들어간다.
`IBlock`은 명령 열을 묶고, `IIf`와 `IWhile`은 조건에 따라 갈라지고, `ICall`과
`ISdoCall`은 다른 함수로 제어를 넘긴다.

`ITrap`은 실행용이 아니라 기록용이다. 컴파일러는 이것을 만들지 않는다. 프로그램이
정해진 이유로 멈췄다는 사실을 명세가 적어 두는 방법이다.
[멈추는 방식](running.md#stopping)을 함께 보라.

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

## 식

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

## 참조

참조는 값을 담아 두는 자리를 가리킨다. 변수이거나, 이미 참조가 닿은 것의 필드다.
변수가 셋으로 나뉘는 것은 사는 곳이 서로 다르기 때문이다. 전역 변수는 상태의 전역
환경에 있고, 두 종류의 지역 변수는 현재 문맥에 있다.

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

## 연산자

단항, 이항, 가변 인자, 수학, 변환 연산자는 각각 다른 구문 부류다. 저마다 자기
보조 함수로 평가한다.

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

## <a id="syntax-trees"></a>구문 트리

AST 노드는 정체성과 자기가 만들어진 생성 규칙, 부모, 그리고 구문 또는 어휘
내용물을 지닌다. 맨 앞의 `nat`이 있어서 두 노드에 대한 `=`가 구조 비교가 아니라
ESMeta의 참조 비교가 된다. [AST 정체성](state.md#ast-identity)을 함께 보라.

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

구문 노드는 자기 생성 규칙을 적용할 때 쓴 인자와 어느 오른쪽 항을 골랐는지를
적어 둔다. `SUBIDX`는 찾을 때마다가 아니라 만들 때 한 번 계산한다. 자식이 노드를
만드는 순간 정해지기 때문이다.

## <a id="values"></a>값

값은 식이 평가되어 나올 수 있는 모든 것이다. `callable`은 클로저나 연속이 지니는
모양이다.

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

수는 특히 눈여겨볼 만하다. `double`은 기계 부동소수점 수가 아니라 IEEE-754
binary64의 원시 필드 세 개로 저장한다. 그래야 `double`끼리의 구조적 비교가 곧 비트
패턴 비교가 되기 때문이다. `java.lang.Double.equals`가 하는 일이 그렇고, ESMeta도
그렇게 비교한다. 0은 `FIN POS 0 0`이고, 부정은 부호 비트를 뒤집는 것이다.

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

## 타입

타입은 `ETypeCheck` 식이 물어볼 수 있는 대상이다. ESMeta의 `ValueTy`는 성분이
열일곱 개인 격자라서, 거기서 나오는 질문에 이 명세가 다 답할 수는 없다. 하지만
명세가 실제로 묻는 것은 값 하나가 몇 안 되는 타입 중 하나에 속하는지뿐이다.

여기 있는 경우들로 표현되지 않는 것은 `YetType`으로 남는다. `$type_contains`에는
`YetType`에 대한 절이 없다. 그래서 모형에 없는 타입은 적당한 답을 받는 대신
프로그램을 **멈춰 세운다**.

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
