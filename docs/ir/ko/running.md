# 프로그램 실행

[← 명령](instructions.md) · [추상 연산 →](abstract-operations.md)

작업은 인터프리터에 관계 하나와 거기에 넣을 값들을 건넨다. 그래서 [명령 한
단계](instructions.md)를 반복하는 구동부도 호스트 언어가 아니라 명세 안에 있어야
한다. 이 구동부는 그 단계를 0번 이상 반복한 것이고, 구성은 단계와 같다. 상태와
앞으로 실행할 명령 열이다.

<a id="Steps"></a>
`s ⊢ inst'''* ⟹ % ; %`:
Running `inst'''*` in state `s`

```spectec
rule Steps/step:
  s |- inst* ==> s_2 ; inst_2*
 -- Step: s |- inst* ==> s_1 ; inst_1*
 -- Steps: s_1 |- inst_1* ==> s_2 ; inst_2*
```

<a id="Steps/step"></a>
Running `inst*` in state `s` yields `inst_2*` in state `s_2`, provided:

1. [Stepping `inst*` in state `s`](instructions.md#Step) yields `inst_1*` in state `s_1`.
1. [Running `inst_1*` in state `s_1`](running.md#Steps) yields `inst_2*` in state `s_2`.

## <a id="stopping"></a>프로그램이 멈추는 방식

끝나는 자리는 둘이다. 둘 다 더는 줄어들지 않고, 둘 다 자기가 멈춰 선 명령 열을
같이 알려 준다. 그래서 부르는 쪽에서 둘을 구별할 수 있다.

명령 열이 비어 있고 스택도 비어 있으면 끝까지 실행된 프로그램이다.

```spectec
rule Steps/done:
  s |- eps ==> s ; eps
 -- if s.STACK = eps
```

<a id="Steps/done"></a>
Running `inst*` in state `s` yields `·` in state `s`, provided:

1. `inst*` is an empty list.
1. `s.STACK` is equal to `·`.

맨 앞에 `ITrap`이 있으면 정해진 이유로 멈춘 프로그램이다.

```spectec
rule Steps/trap:
  s |- (ITrap trap :: inst*) ==> s ; (ITrap trap :: inst*)
```

<a id="Steps/trap"></a>
Running `inst''*` in state `s` yields `ITrap trap :: inst*` in state `s`, provided:

1. `inst''*` is a non-empty list.
1. Let `inst' :: inst*` be `inst''*`.
1. `inst'` matches pattern **`ITrap`** `_`.
1. Let `ITrap trap` be `inst'`.

둘 다 자기에게 해당하는 단계가 없으니 진행 규칙은 적용되지 않는다. 그래도 이 둘을
먼저 맞춰 보는데, 적용되지 않는 규칙은 한 번 실패해 봐야 알 수 있기 때문이다.

### 끝맺지 못하는 세 가지

"오류" 하나로 묶었으면 섞였을 결과 셋을 명세는 구별한다.

| 결과 | 드러나는 모습 | 뜻 |
| --- | --- | --- |
| 완료 | `eps`, 빈 스택 | 끝까지 실행되었다 |
| 트랩 | 맨 앞의 `ITrap` | 명세가 정한 이유로 멈췄다 |
| 멈춰 섬 | 맞는 규칙 없음 | 이 경우를 위한 규칙이 없다 |

트랩은 이름이 붙은 끝이다. [식](expressions.md)은 big-step으로 평가하니 값 대신
트랩을 내보낼 길이 없다. 그래서 이름이 붙을 수 있는 것은 명령 쪽 실패뿐이고, 식 쪽
실패는 그냥 멈춰 선다. 트랩의 사유는 정확히 그 명령 쪽 실패들이다.

<a id="trap"></a>
```spectec
syntax trap = 
   | {AssertFail expr}
   | NoReturn
   | {NoCallable value}
   | {NoBool value}
```

멈춰 서는 것은 덮어야 할 결함이 아니다. 답할 수 없는 질문에 억지로 답하는 대신
"이건 아직 모형에 없다"고 명세가 소리 내어 말하는 방식이다.
[`ETypeCheck`](expressions.md#type-checks)에서 모형에 없는 타입을 물었을 때도 같은
길로 여기에 온다.

## 진입점

`$init_state`는 함수 표를 읽어 들이고 main 함수 위에 문맥을 연다. `$init_insts`는
거기서 시작할 본문이다. main 함수의 매개변수는 무시한다. 인자를 넣어 주는 것이
없기 때문이다.

### 초기 상태

`$init_state(program)`:
The initial state for `program`

```spectec
def $init_state(program) = $empty_state($init_context($main_func(program)))[FUNC = $funcs_of(program)]
```

### 초기 문맥

`$init_context(func)`:
An initial context on `func`

```spectec
def $init_context(func) = {FUNC func, LOCAL $empty_map<local, value>}
```

### 초기 명령 열

`$init_insts(program)`:
The initial instruction sequence of `program`

```spectec
def $init_insts(program) = $body_of($main_func(program))
```

프로그램을 실행하려면 명령 열이 끝까지 소진되어야 한다. 그래서 트랩에 빠지는
프로그램에는 도출이 아예 없다. `assert` 실패가 "결과가 트랩인 작업"이 아니라
*실패한* 작업이 되는 것은 이 때문이다.

```spectec
rule Run:
   |- program ==> s
 -- Steps: $init_state(program) |- $init_insts(program) ==> s ; eps
```

<a id="Run"></a>
Running `program` yields `s`, provided:

1. [Running the initial instruction sequence of `program` in state the initial state for `program`](running.md#Steps) yields `inst*` in state `s`.
1. `inst*` is an empty list.
