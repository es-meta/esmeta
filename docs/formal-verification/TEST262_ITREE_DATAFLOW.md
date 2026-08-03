# Test262 JavaScript에서 Rocq ITree 판정까지

이 문서는 현재 production Test262 경로에서 JavaScript 테스트 한 개가
어떤 파일을 거치며, 각 단계에서 어떤 입력이 어떤 출력으로 바뀌는지
설명한다. 또한 compact payload를 사용해 데이터를 OCaml에서 복원하는
이유와, payload에 포함되는 lexical SDO 및 HostCapture의 의미를 구분한다.

핵심 구조는 다음과 같다.

```text
Test262 JavaScript
  -> harness를 포함한 source + AST
  -> 테스트별 payload(source, AST, lexical SDO, hosts, expected)
  -> 공통 ECMA 명세/초기 상태와 재조립한 prog
  -> Semantics.v의 IR-to-ITree denotation
  -> ITreeExec.exec_itree
  -> 추출된 OCaml runner
  -> PASS / MISMATCH / UNSUPPORTED / OUT-OF-FUEL / CRASH
```

JavaScript마다 별도의 IR 파일을 생성하는 구조는 아니다. ECMA-262 알고리즘
전체가 공통 IR 함수 집합으로 한 번 export되고, 각 테스트의 JavaScript AST가
그 함수들이 해석할 데이터로 들어간다.

## 구체적인 테스트

이 문서에서는 다음 Test262 테스트를 사용한다.

```text
tests/test262/test/built-ins/Array/isArray/15.4.3.2-0-2.js
```

테스트 본문은 다음 한 문장이다.

```js
assert.sameValue(
  Array.isArray.length,
  1,
  "The value of Array.isArray.length is expected to be 1"
);
```

현재 inventory와 해당 campaign 안에서 사용하는 global target index는 `90`이다.
local ID는 실행 shard마다 달라진다.
보존된 `test262-full-20260801-v2` 실행에서는 `T087`이었고, 별도의 focused
shape dump에서는 `T000`이었다. 따라서 테스트를 식별할 때는 local `Txxx`보다
global index와 상대 경로를 사용해야 한다.

보존된 production 결과는 다음과 같다.

```text
T087      0.711s      52020  PASS
          built-ins/Array/isArray/15.4.3.2-0-2.js
```

근거:

- `formal/logs/test262-full-20260801-v2/results.tsv`
- `formal/logs/test262-full-20260801-v2/logs/run-00090.log`
- `formal/logs/itree-shape-array-isarray-T000.readable.log`

마지막 shape dump는 설명을 위해 실행 초반 1,398 step에서 출력을 자른 진단
로그다. production 판정은 별도로 52,020 step을 끝까지 실행했다.

## 한 번 만드는 공통 부분과 테스트마다 만드는 부분

속도를 이해하려면 먼저 두 종류의 데이터를 구분해야 한다.

### 공통 부분: 한 번 생성·컴파일·로딩

- ECMA-262 IR 함수 약 2,950개: `spec_funcs`
- 초기 global environment: `base_globals`
- 초기 heap: `init_heap`
- `Fragment.v`, `Domain.v`, `Semantics.v`, `ITreeExec.v`의 추출 결과
- native runner가 사용하는 specification snapshot

공통 Rocq facade는 `formal/validation/Spec.v`이고, 실제 큰 정의들은
`formal/validation/spec/SpecFuncs_*.v`, `SpecGlobals.v`, `SpecHeap_*.v`에
나뉘어 있다.

### 테스트별 부분: 매 테스트마다 생성

- Test262 상대 경로
- harness가 합쳐진 effective source
- 파싱된 AST
- AST lexical node에 붙은 lexical SDO 표
- 해당 ESMeta oracle 실행에서 관찰된 HostCapture query/result 표
- ESMeta의 expected `RESULT`
- ESMeta의 expected `IPrint` 목록

이 테스트별 부분만 `Txxx.fvt` payload로 전달된다.

## End-to-end 단계

### 1. 캠페인에서 테스트 선택

`formal/run-test262-full.py`가
`formal/validation/test262-inventory.tsv`의 `TARGET` 행에서 테스트를 선택한다.

이 예제의 입력과 출력은 다음과 같다.

```text
input:
  global index = 90
  relName = built-ins/Array/isArray/15.4.3.2-0-2.js

output:
  FVInitState --test262-server에 보낼 export 요청
```

전체 캠페인은 JVM과 OCaml worker를 테스트마다 재시작하지 않는다.
`FVInitState --test262-server --payload-only` JVM 하나와 여러 persistent native
worker를 재사용한다.

### 2. frontmatter, harness, 테스트 본문 결합

`src/main/scala/esmeta/test262/Test.scala`가 Test262 frontmatter를 읽어 다음
정보를 만든다.

```text
Test(
  relName,
  negative,
  flags,
  includes,
  locales,
  features,
  es5
)
```

`src/main/scala/esmeta/test262/Test262.scala`의 `loadTest`는 다음 코드를
결합하고 파싱한다.

```text
harness/assert.js
+ harness/sta.js
+ frontmatter의 includes에 지정된 추가 harness
+ 테스트 본문
```

이 예제에는 추가 `includes`가 없지만 `assert.js`와 `sta.js`는 항상 들어간다.

```text
input:
  raw Test262 JavaScript + frontmatter

output:
  tast : Ast       // 결합된 전체 JavaScript의 AST
  code : String    // 결합·정규화된 effective source
```

`assert.sameValue`는 Rocq의 `IAssert`로 특별 변환되지 않는다. `assert.js`에
정의된 평범한 JavaScript 함수로서 같은 AST와 ECMA 명세 의미론을 통해
실행된다. 실패하면 `Test262Error`를 throw한다.

### 3. ESMeta oracle 실행과 테스트별 입력 수집

`src/main/scala/esmeta/fv/FVInitState.scala`는 테스트별로 다음을 실행한다.

```scala
val (tast, code) = t262.loadTest(path, test.includes)
val initial = Initialize(cfg).from(code, tast)
val interpreter = new HostCapturingInterpreter(initial)
val finalState = interpreter.result
```

입력은 다음과 같다.

```text
cfg   = ECMA-262 전체 IR CFG
code  = harness + 테스트의 effective source
tast  = code의 parsed AST
```

출력은 다음과 같다.

```text
hosts  : List[HostCapture]
res    : Value
prints : List[Value]
```

positive Test262 테스트는 ESMeta의 최종 `RESULT`가 반드시 `Undef`여야 export
된다. `requireSuccessfulTest262Exit`가 이 조건을 검사한다. 따라서 실패한
`assert.sameValue`를 기대 결과로 저장해서 양쪽 실패를 `PASS`로 만드는 경로는
허용되지 않는다.

이 예제의 expected observable은 다음과 같다.

```text
expected result = VUndef
expected prints = []
```

과거 `T087.fvt`는 이후 shard export가 덮어썼으므로 그 실행 당시의 정확한
HostCapture entry 목록 자체는 현재 보존 로그에서 복원할 수 없다. 아래
HostCapture 절의 예시는 이 테스트가 특정 query를 사용했다고 주장하는 것이
아니라, 현재 payload와 의미론이 cache entry를 처리하는 방식을 보여준다.

### 4. compact payload 생성

`FVInitState.encodeTestPayload`와 `src/main/scala/esmeta/fv/FVPayload.scala`가
다음 논리적 tuple을 binary `FVPayload v7`로 인코딩한다.

```text
globalIndex
name
effective source
AST                         // 각 ALex node의 lexical SDO 표 포함
HostCapture entries
expected result
expected prints
```

현재 magic은 `ESFVIT07`이다. ECMAScript 문자열은 lone surrogate를 보존하기
위해 UTF-16 code unit으로, metadata는 strict UTF-8로 기록한다. 정수는 exact
decimal, finite Number는 IEEE-754 bit pattern으로 기록한다. 파일은 임시 파일을
거쳐 atomic replace된다.

보존된 전체 실행 당시 이 예제의 manifest 관계는 다음과 같았다.

```text
global 90
  -> local T087
  -> validation/payload/T087.fvt
  -> built-ins/Array/isArray/15.4.3.2-0-2.js
```

`validation/payload`는 다음 shard export가 덮어쓰는 임시 산출물이므로, 과거
`T087.fvt` 자체는 보존 로그와 달리 현재 작업 트리에 남아 있지 않을 수 있다.

### 5. payload를 만들었다가 OCaml에서 복원하는 이유

이 왕복은 의미론 결과를 다른 표현으로 계산했다가 다시 계산하는 과정이 아니다.
서로 다른 runtime 사이에서 **테스트별 데이터**를 운반하는 serialization
boundary다.

#### JVM과 extracted OCaml 사이의 데이터 경계

Test262 loader, ESMeta parser, initializer, oracle interpreter는 JVM/Scala에서
동작한다. Rocq에서 추출된 ITree 의미론과 native worker는 OCaml에서 동작한다.
현재 구조에서는 두 runtime이 같은 in-memory Scala/Rocq 값을 직접 공유하지
않으므로, source·AST·host table·expected observable을 명시적인 형식으로
전달해야 한다.

#### 테스트를 Rocq 코드가 아니라 데이터로 취급

이전 경로는 테스트 tuple 하나마다 큰 `.v` term을 생성한 뒤 다음을 반복했다.

```text
Rocq parse/typecheck
-> extraction
-> OCaml compile
-> link
```

production payload 경로는 이미 컴파일된 `make_test_tree`에 새 데이터만 넣는다.
따라서 테스트별 `coqc`, extraction, OCaml compilation, link가 없다.

#### 공통 명세를 payload마다 복제하지 않음

payload에는 2,950개 ECMA 함수와 초기 heap을 넣지 않는다. OCaml worker가 이미
로딩한 공통 `Spec`에 테스트별 다음 세 입력만 합친다.

```text
source + AST + hosts
```

이 때문에 `script_prog`에서 “복원 후 재조립” 단계가 필요하다. payload decoder는
테스트별 값을 복원하고, `script_prog`가 그 값들을 공통 명세 및 초기 상태와
결합한다.

#### persistent worker와 병렬 실행

한 native worker는 큰 specification을 한 번 로딩하고 여러 payload를 연속으로
실행한다. 여러 worker가 서로 다른 payload를 병렬 실행할 수도 있다. JVM도
`--test262-server`로 CFG, grammar, Test262 corpus를 계속 재사용한다.

#### 명시적인 검증·감사 경계

`formal/payload_codec.ml`은 version, tag, 길이, UTF-8, trailing bytes 등을
검사하고, `formal/payload_worker.ml`은 payload의 global index와 test name이
요청과 일치하는지 검사한다. `make payload-roundtrip`은 기존 Rocq tuple 경로와
compact decoder 결과를 field-by-field로 비교하는 독립 audit lane이다.

따라서 payload는 실행 속도를 위한 transport이지만, handwritten decoder가
native 테스트 경로의 추가 신뢰 경계가 된다는 사실도 문서화해야 한다.

### 6. OCaml payload decoder

`formal/payload_codec.ml`은 `.fvt`를 Rocq extraction이 생성한 `Fragment` 타입으로
직접 복원한다.

```ocaml
type test_input =
  ((((string * cstr) * ast) * host_cache_entry list)
   * (coq_val * coq_val list))
```

논리적으로는 다음 값이다.

```text
(
  name,
  source,
  ast,
  hosts,
  (expected_result, expected_prints)
)
```

decoder는 JavaScript나 IR 의미를 실행하지 않는다. binary tag를 해당 extracted
algebraic datatype constructor로 복원할 뿐이다.

`formal/payload_worker.ml`은 복원 후 다음을 호출한다.

```ocaml
let test = make_test_tree payload.test_input
let elapsed, verdict = evaluate_test ~fuel ... test
```

### 7. `script_prog`로 공통 명세와 재조립

`formal/ITreeCore.v`의 `make_test_tree`는 payload를 분해한다.

```coq
let '(name, src, a, hosts, expected) := t in
let program := script_prog src a hosts in
```

생성된 `formal/validation/Spec.v`의 `script_prog`는 다음 구조다.

```coq
mkProgFull
  spec_funcs
  (Some src)
  (Some a)
  hosts
  (("SOURCE_TEXT", VStr src) :: base_globals)
  init_heap
```

따라서 실제 `prog`의 각 필드는 다음 값을 받는다.

| 필드 | 전달되는 값 |
|---|---|
| `p_funcs` | 공통 ECMA 명세 IR 함수 |
| `p_source` | 테스트의 effective source |
| `p_cached` | 같은 source의 parsed AST |
| `p_hosts` | 테스트별 HostCapture query/result |
| `p_globals` | `SOURCE_TEXT`와 공통 초기 globals |
| `p_heap` | 공통 초기 heap |

### 8. IR operational semantics를 ITree로 해석

`formal/Semantics.v`는 `p_funcs`의 각 ESMeta IR 함수에 의미를 부여한다.

```text
IR expression/instruction/function
  -> itree crisE result
```

이 단계에서 다음이 처리된다.

- expression 및 instruction 평가
- local environment
- global/heap keyed store
- 함수 호출과 completion
- `IPrint`의 IO event
- undefined behavior의 `Take False`

JavaScript AST는 `RunJobs`, `ScriptEvaluation`, syntax-directed `Evaluation`,
`EvaluateCall` 같은 공통 specification 함수들이 읽는다. 이 예제에서는 결국
`Array.isArray.length`를 읽고 JavaScript 함수 `assert.sameValue`를 호출한다.

### 9. 닫힌 실행 ITree 구성

`formal/ITreeExec.v`의 다음 함수가 production 실행 진입점이다.

```coq
exec_itree : string -> prog -> itree coreE val
```

이 함수는 `Semantics.v`의 함수별 denotation을 다음과 결합한다.

- 함수 이름 lookup
- ordinary call frame
- first-class continuation stack
- CRIS keyed store interpreter
- entry 함수와 초기 state

닫힌 이후 runner가 관찰하는 형태는 다음과 같다.

```text
Tau next
Vis (IO "esmeta.print" value) continuation
Vis Take continuation
Vis Choose continuation
Ret value
```

현재 production Test262 경로는 `Exec.v`를 사용하지 않는다.

### 10. extraction과 native 실행

`formal/ExtractCore.v`가 `ITreeCore.make_test_tree`와 그 실행 의존성을 OCaml로
추출한다. `formal/itree_test_runtime.ml`은 추출된 ITree를 fuel 범위에서 한
transition씩 관찰한다.

```text
Tau                  -> step 증가 후 계속
IO "esmeta.print"    -> actual prints에 값 추가
Ret value            -> Completed(value, prints, steps)
Take                  -> UNSUPPORTED-EFFECT Take/UB
Choose                -> UNSUPPORTED-EFFECT Choose
fuel = 0              -> OUT-OF-FUEL
exception             -> CRASH
```

이 예제의 production 결과는 다음과 같다.

```text
actual result = VUndef
actual prints = []
steps = 52,020
```

### 11. 최종 판정

`ITreeCore.observable_outcome_eqb`가 AST reference identity를 포함한 observable
값을 비교하고, `itree_test_runtime.ml`이 verdict로 분류한다.

```text
actual   = (VUndef, [])
expected = (VUndef, [])
result   = PASS
```

ESMeta expected result와 prints는 ITree 실행 중간값으로 사용되지 않고 마지막
비교에만 사용된다. 반면 lexical SDO와 HostCapture는 아래 설명처럼 실제 실행
입력이다.

## Lexical SDO

### 무엇인가

SDO는 syntax-directed operation이다. syntactic AST node의 SDO는 해당 ECMA
알고리즘 IR 함수를 호출하지만, ESMeta의 lexical AST node는 별도 IR call frame
없이 Scala `ESValueParser`가 직접 값을 계산한다.

현재 exporter는 다음 여섯 lexical SDO를 시도한다.

```text
StringValue
NumericValue
MV
SV
TV
TRV
```

`FVInitState.lexicalSdos`는 각 lexical node에 대해
`Interpreter.eval(lex, method)`를 호출하고, 표현 가능한 결과를 AST의 `ALex`
node에 붙인다.

```text
Lexical(name, spelling)
  -> Interpreter.eval(lex, method)
  -> ALex(name, spelling, ..., [(method, lexval), ...])
```

Rocq에서 `Domain.ast_lex_sdo`가 이 표를 조회하며, `Semantics.v`의 `ISdoCall`은
receiver가 `ALex`이면 함수 호출 없이 즉시 그 값을 반환한다.

### 이 테스트의 숫자 literal `1` 예시

`assert.sameValue`의 두 번째 인자인 `1`은 개념적으로 다음과 같은 lexical
node가 된다. 실제 node에는 source slice 등 추가 필드가 있다.

```coq
ALex "NumericLiteral" "1" ...
  [ ("NumericValue", LVNumber 1.0)
  ; ("MV", LVMath 1)
  ]
```

ECMA 알고리즘이 이 node에 `NumericValue` SDO를 호출하면 실행 경로는 다음과
같다.

```text
ISdoCall(..., method = "NumericValue")
  -> ast_lex_sdo
  -> table lookup
  -> VNumber 1.0
```

다른 간단한 예는 identifier `x`다.

```coq
ALex "IdentifierName" "x" ...
  [("StringValue", LVStr "x")]
```

이 표는 runtime에 query가 생길 때마다 ESMeta를 호출하는 cache가 아니다. AST
export 시 lexical node에 고정해서 넣는 derived semantic annotation이다.

### 증명 관점의 신뢰 경계

lexical SDO는 단순 tree shape보다 강한 의미 정보다. 예를 들어 문자열 escape
해석이나 numeric literal 해석 결과를 이미 포함한다. 따라서 다음 두 관점이
가능하다.

- parser와 literal decoding까지 trusted frontend로 인정하면 허용 가능한 입력
- source-level 의미 정확성까지 Rocq에서 증명하려면 별도의 correctness 관계가
  필요한 외부 계산 결과

현재 Rocq는 table lookup이 정확히 동작하는 것은 계산하지만,
`"1"`의 `NumericValue`가 왜 `1.0`이어야 하는지는 증명하지 않는다.

표현 불가능한 lexical 결과를 근사하지는 않는다. 예를 들어 fractional `Math`
값은 반올림하지 않고 해당 SDO entry만 생략하며, 그 entry를 실제로 조회하면
UB가 된다. 다른 표현 가능한 SDO entry는 유지된다.

## HostCapture cache

### 무엇인가

HostCapture는 IR operational semantics만으로 현재 Rocq model이 직접 계산하지
않는 host primitive의 **query와 ESMeta가 계산한 result**를 테스트 export 시
기록한 표다.

```text
ESMeta oracle 실행 중 host primitive 호출
  -> HostCapture(query, result)
  -> payload의 host_cache_entry
  -> prog.p_hosts
  -> Rocq typed_host_cache_lookup(query)
  -> result 반환, 없거나 타입이 틀리면 UB
```

현재 query 종류는 다음 12개다.

| Query | 대표 의미 |
|---|---|
| `HQParseText` | 동적 source parsing |
| `HQToStr` | Number/BigInt의 radix 문자열 변환 |
| `HQStrToNumber` | String에서 Number 변환 |
| `HQStrToBigInt` | String에서 BigInt 변환 |
| `HQNumberPow` | Number exponentiation |
| `HQDoubleToLongChecked` | checked double-to-integer 변환 |
| `HQMathOp` | 정수 Math 영역의 host 수학 함수 |
| `HQMathToNumber` | exact Math integer에서 Number 변환 |
| `HQNumberMathOp` | Number add/mul/div/pow terminal 연산 |
| `HQNumberSin` | Number sine |
| `HQNumberMathCompare` | Number와 Math integer 비교 |
| `HQNumberToMath` | finite Number에서 Math 변환 |

### 문자열에서 Number로 변환하는 예

다음과 같은 JavaScript 경로를 생각할 수 있다.

```js
Number("1.5")
```

생성된 명세 IR이 `CToNumber`를 문자열 값에 적용할 때 Scala oracle은 다음
entry를 기록한다.

```text
HostCapture.StrToNumber("1.5", Number(1.5))
```

payload에서는 논리적으로 다음 Rocq 값이 된다.

```coq
mkHostCacheEntry
  (HQStrToNumber (cu "1.5"))
  (VNumber 1.5)
```

Rocq 실행은 다음처럼 진행된다.

```text
CToNumber (VStr "1.5")
  -> host_cop_query
  -> HQStrToNumber "1.5"
  -> typed_host_cache_lookup p_hosts
  -> VNumber 1.5
```

Rocq는 result가 `VNumber`인지 확인하지만, 문자열 `"1.5"`와 Number `1.5`의
변환 관계 자체를 계산하거나 증명하지는 않는다.

### 동적 parse 예시

동적 `EParse`에는 다음과 같은 entry가 사용된다.

```coq
HQParseText source "Script" params
  -> VAst ...       // 성공
  -> VUndef         // parse failure sentinel
```

초기 Test262 전체 source를 Script로 읽는 경우에는 `p_source`와 `p_cached`가
일치하므로 cached AST fast path를 사용한다. 다른 문자열이나 grammar symbol을
runtime에 parse하는 경우에는 `HQParseText` HostCapture가 필요하다.

### lookup의 안전성과 한계

`Domain.typed_host_cache_lookup`은 다음을 확인한다.

- query가 정확히 일치하는가
- result constructor가 query에 허용된 타입인가

예를 들어 `HQStrToNumber`의 result는 반드시 `VNumber`여야 한다. cache entry가
없거나 result 타입이 잘못되면 `None`이 되고 ITree 의미론은 `Take/UB`로
종료한다. 즉 임의의 기본값을 넣거나 조용히 PASS하지는 않는다.

하지만 이 검사는 다음 명제를 증명하지 않는다.

```text
hc_result = query의 ECMAScript상 올바른 결과
```

따라서 HostCapture를 사용하는 경로의 Rocq 정리는 주어진 host table의 정확성에
조건부다. 모든 실행 의미론을 Rocq 내부에 두려면 이 12개를 Rocq 계산으로
대체하거나, 최소한 각 query/result 관계를 Rocq에서 검증해야 한다.

## Lexical SDO와 HostCapture의 차이

| 항목 | Lexical SDO | HostCapture |
|---|---|---|
| 생성 시점 | AST export 시 모든 lexical node 조사 | ESMeta oracle 실행 중 실제 host 호출 시 |
| 저장 위치 | 각 `ALex` node의 `sdos` 필드 | `prog.p_hosts` |
| key | SDO method 이름 | typed host query와 operands |
| 사용 위치 | lexical `ISdoCall` | conversion, math, dynamic parse 등 |
| 누락 시 | 해당 SDO 호출이 UB | 해당 host query가 UB |
| 성격 | parser/literal elaboration에 가까운 derived 의미 정보 | 실행 중간 결과 oracle |
| 현재 Rocq가 증명하는 것 | table lookup과 이후 실행 | typed query lookup과 이후 실행 |
| 현재 Rocq가 증명하지 않는 것 | lexeme에서 SDO result가 나온 이유 | query에서 host result가 나온 이유 |

사용자가 parser와 lexical literal decoding을 trusted frontend로 인정한다면
lexical SDO는 그 경계 안에 둘 수 있다. HostCapture는 프로그램 실행 중의
의미 결과이므로 독립적인 Rocq 실행을 목표로 할 때 더 직접적인 제거 대상이다.

## expected observable과 실행 cache의 차이

payload의 세 종류의 외부 유래 값을 혼동하지 않아야 한다.

| 값 | ITree 실행 중 사용되는가 | 역할 |
|---|---:|---|
| AST와 lexical SDO | 예 | 프로그램 입력 및 lexical 의미 lookup |
| HostCapture entries | 예 | host primitive 결과 lookup |
| expected result/prints | 아니오 | 실행 완료 후 actual observable과 비교 |

expected result와 prints는 차등 테스트의 사후 oracle이다. 반면 lexical SDO와
HostCapture는 ITree의 실행 결과에 영향을 주는 입력이다.

## 핵심 파일 지도

| 파일 | 역할 |
|---|---|
| `src/main/scala/esmeta/test262/Test.scala` | frontmatter 해석 |
| `src/main/scala/esmeta/test262/Test262.scala` | harness 결합과 JavaScript parsing |
| `src/main/scala/esmeta/fv/FVInitState.scala` | 명세/초기 상태 export, ESMeta oracle, lexical SDO 및 hosts 수집 |
| `src/main/scala/esmeta/fv/FVPayload.scala` | compact v7 binary encoder |
| `formal/Fragment.v` | IR, AST, value, prog, host query 자료형 |
| `formal/Domain.v` | 순수 연산, lexical SDO lookup, typed host lookup |
| `formal/Semantics.v` | ESMeta IR의 ITree denotation |
| `formal/ITreeExec.v` | 함수 호출과 store를 닫아 `itree coreE val` 생성 |
| `formal/validation/Spec.v` | 공통 명세/초기 상태와 테스트별 입력을 합치는 `script_prog` |
| `formal/ITreeCore.v` | `make_test_tree`와 observable 비교 |
| `formal/ExtractCore.v` | production core extraction 진입점 |
| `formal/payload_codec.ml` | payload를 extracted `Fragment` 값으로 복원 |
| `formal/payload_worker.ml` | persistent worker protocol과 test tree 생성 |
| `formal/itree_test_runtime.ml` | ITree step 실행과 verdict 분류 |
| `formal/run-test262-full.py` | 전체 캠페인, shard, exporter/worker orchestration |

## 한 줄 요약

```text
raw JS
  -> harness를 포함한 source와 AST
  -> lexical SDO/HostCapture/expected를 포함한 per-test payload
  -> OCaml에서 같은 Fragment 값으로 복원
  -> 이미 컴파일·로딩된 ECMA spec 및 initial state와 script_prog로 결합
  -> Semantics.v/ITreeExec.v에서 실행
  -> actual observable을 expected observable과 비교
  -> PASS
```

payload encode/decode는 공통 명세를 매 테스트마다 다시 Rocq compile하지 않기
위한 데이터 운반 최적화다. 다만 payload 안의 lexical SDO와 HostCapture가 외부에서
계산된 의미 정보라는 사실은 별도의 증명·신뢰 경계로 남는다.
