# 상태

[← 구문](syntax.md) · [식 →](expressions.md)

상태에는 함수 표와 현재 문맥, 잠시 멈춰 둔 호출자들의 스택, 전역 환경, 힙, 출력
흐름, 그리고 새로 만든 AST 노드에 번호를 붙이는 계수기가 들어 있다.

상태(state)는 함수 테이블, 현재 컨텍스트, 일시 중단된 호출자(caller)들의 스택, 전역 환경, 
힙(heap), 출력 스트림, 그리고 새로 할당된 AST 노드에 이름을 부여하는 카운터로 이루어져 있다.

<a id="state"></a>
```spectec
syntax state = {FUNC map<id, func>, CTX context, STACK callctx*, GLOBAL map<global, value>, HEAP memory, OUT value*, ASTID nat}
```

이러한 필드 중 하나를 읽거나 쓰는 모든 연산은 [추상 연산 (Abstract Operations)](abstract-operations.md#state-operations)
에 정의되어 있으므로, 다른 어떤 추론 규칙도 해당 레코드에 직접 접근하지 않는다.

## 함수 표

ESMeta가 CFG를 찾는 이유가 바로 `FUNC`다. `cfg.getFunc(fname)`은 이름을 받아 그
이름의 함수를 돌려준다. 의미론이 CFG에서 더 가져다 쓰는 것은 없다. CFG의 진입
노드도 결국 `func` 안에 이미 있는 본문을 컴파일한 모습이기 때문이다. 함수들을
상태에 담아 두면 명세 전체가 IR 수준에 머무를 수 있다.

함수 표는 프로그램을 읽어 들이는 순간 정해진다. 그래서 만드는 연산
([`$funcs_of`](abstract-operations.md#funcs_of))과 찾는 연산
([`$find_func`](abstract-operations.md#find_func))은 있지만 새로 묶는 연산은 없다.
평가 중에 함수가 늘어나지는 않기 때문이다.

main 표시가 붙은 함수는 정확히 하나여야 한다. 하나도 없거나 여럿이면 오류다. 두
경우 모두 [`$main_func`](abstract-operations.md#main_func)가 정의되지 않으므로,
그런 프로그램은 시작점을 아무렇게나 정하는 대신 멈춰 선다.

## 문맥과 호출 스택

문맥(Context)은 ESMeta의 `Context`에서 커서를 뺀 것이다. 앞으로 실행할 명령들은 여기
저장하지 않고 [명령 규칙](instructions.md)이 들고 다닌다. CFG가 없으니 "함수의
어디까지 왔는가"는 그 규칙들이 쥐고 있는 명령 열 자체다. 그것을 따로 적어 둬야
하는 것은 잠시 멈춰 둔 프레임뿐이다.

<a id="context"></a>
<a id="callctx"></a>
```spectec
syntax context = {FUNC func, LOCAL map<local, value>}

syntax callctx = {CTX context, INST inst*, RETID local}
```

그래서 `callctx`에는 호출자의 문맥 전체와 다시 이어 갈 커서, 결과를 받을 지역
변수가 들어간다. [`$callee_context`](abstract-operations.md#callee_context)는 호출이
들어갈 문맥을 만들고, [`$bind_params`](abstract-operations.md#bind_params)는 인자를
묶는다.

## 변수

지역 변수는 현재 문맥에, 전역 변수는 상태에 있다. 읽을 때는
[`$find_var`](abstract-operations.md#find_var)를 거치고, 여기서
[`$find_local`](abstract-operations.md#find_local)이나
[`$find_global`](abstract-operations.md#find_global)로 갈라진다. 묶을 때도 그
자리에서 레코드를 고치지 않고
[`$bind_local`](abstract-operations.md#bind_local)과
[`$bind_global`](abstract-operations.md#bind_global)을 거친다.

## 힙

힙은 주소에서 객체로 가는 사상에 크기 계수기를 더한 것이다. 계수기가 있으니 새
주소를 짐작하지 않고 계산해 낼 수 있고,
[`$alloc_heap`](abstract-operations.md#alloc_heap)이 다음 주소를 내어 준다.

<a id="memory"></a>
<a id="addr"></a>
<a id="obj"></a>
```spectec
syntax memory = {MAP map<addr, obj>, SIZE nat}

syntax addr = 
   | {NamedAddr text}
   | {DynamicAddr int}

syntax obj = 
   | {RecordObj text map<text, value>}
   | {MapObj map<value, value>}
   | {ListObj value*}
   | {YetObj text text}
```

## <a id="ast-identity"></a>AST 동등성

평가 중에 만든 AST에는 구조만으로는 줄 수 없는 동등성이 필요하다. 모양이 같아도
따로 만든 두 노드는 ESMeta에서 서로 다른 것으로 취급되기 때문이다. 그래서 노드마다 일련
번호를 붙인다. 이 계수기는 이 명세에만 있는 장치다. ESMeta에는 그런 필드가 없다.
[`$fresh_ast_id`](abstract-operations.md#fresh_ast_id)는 번호를 읽으면서 동시에
하나 올린다.

## 출력 스트림 (Output Stream)

`OUT`은 출력 스트림이다. `IPrint`가 평가한 값을 여기에 차례대로 덧붙인다. ESMeta의
`IPrint`는 `println`을 부르고 아무것도 남기지 않는다. 하지만 인터프리터의 백트래킹 때문에, 
나중에 실패할 규칙 안에서 출력한 것은 다시 거둬들일 방법이 없다. 
흐름을 상태에 담아 두면 필드 하나를 더 쓰는 대신, 출력이 눈에 보이면서 되돌리기에도 같이 따라온다.

값을 어떻게 보여 줄지는 표현의 문제다. 그래서 `OUT`에는 글자가 아니라 값 자체를
담는다.
