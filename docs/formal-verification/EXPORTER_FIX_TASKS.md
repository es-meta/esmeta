# ESMeta FV 익스포터 수정 — 완료 현황과 남은 결정

이 문서는 2026-08-02 정적 감사에서 발견한 익스포터·실행 의미론 결함의 현재
상태를 기록한다. 이전 판본의 T0–T7 실행 프롬프트는 모두 적용되었으므로 더 이상
작업 지시로 사용하지 않는다.

## 검증 결과를 읽는 법

2026-08-01의 전체 32,207개 실행은 수정 전 기준선이다.

| 결과 | 개수 |
|---|---:|
| PASS | 27,238 |
| ESMETA_FAILED | 4,359 |
| NOT_REPRESENTABLE | 436 |
| UNSUPPORTED | 174 |
| MISMATCH / BUILD_ERROR / CRASH / TIMEOUT | 0 |

근거: `formal/logs/test262-full-20260801-v2/summary.txt`.

수정 뒤에는 이 32,207개 전체를 다시 실행하지 않았다. 대신 위 기준선의
`NOT_REPRESENTABLE` 436건과 `UNSUPPORTED` 174건, 합계 610건을 v7 payload와
ITree worker로 다시 실행했다.

| 표적 실행 | PASS | UNSUPPORTED | 그 밖의 결과 |
|---|---:|---:|---:|
| 과거 실패 610건 | 609 | 1 | 0 |
| 최종 잔여 16건 | 15 | 1 | 0 |

근거:

- `formal/logs/test262-archived-failures-final-v2-20260802/summary.txt`
- `formal/logs/test262-residual-final-v2-20260802/summary.txt`

그 뒤 추가된 split/provenance 및 `TimeClip`/`ToUint8Clamp` 보강 후에도 전체 610건을
다시 돌리지는 않았다. 최종 바이너리에서는 과거 decimal
`NOT_REPRESENTABLE` 대표 1건과 유일한 `Math.random`을 다시 실행했다.

| 최종 spot check | 결과 |
|---|---|
| `built-ins/Array/S15.4_A1.1_T7.js` | PASS |
| `built-ins/Math/random/S15.8.2.14_A1.js` | `UNSUPPORTED-EFFECT Take/UB` |

근거:

- `formal/logs/test262-final-nr-smoke-v2-20260802/summary.txt`
- `formal/logs/test262-final-random-smoke-20260802/summary.txt`

따라서 “현재 전체 캠페인이 32,206 PASS”라고 주장하면 안 된다. 정확한 주장은
“보존된 v7 표적 실행에서 과거 `NOT_REPRESENTABLE`/`UNSUPPORTED` 610건 중 609건이
PASS했고, 최종 빌드의 대표 재검사에서도 decimal 사례는 PASS, 유일한
`Math.random`은 의도적으로 fail-closed한다”이다.

## 완료된 수정

| 항목 | 상태 | 현재 구현과 회귀 |
|---|---|---|
| Number/Math 평가 순서 | 완료 | 왼쪽 식과 변환을 먼저 끝낸 뒤 오른쪽을 평가한다. `NumberMathRegression.v`가 fail-fast, 한 번 평가, cache hit/miss와 혼합 비교를 검사한다. |
| Number→BigInt | 완료 | 유한 IEEE-754 Binary64 값을 2^53 제한 없이 0 방향으로 정확히 절단한다. `NumberToBigIntRegression.v`가 경계를 검사한다. |
| Number↔Math 혼합 연산 | 완료 | `HQNumberMathOp`, `HQNumberMathCompare`, `HQNumberToMath`의 typed host query로 Scala `BigDecimal` 경계를 보존한다. NaN과 ±Infinity cache 항목은 거부한다. |
| `LVUndef` | 완료 | Test262의 `TV` 값을 payload와 Rocq 값 영역에 `undefined`로 보존한다. 현재 payload magic은 `ESFVIT07`이다. |
| `EParse` 예외 경계 | 완료 | 지원하는 operand fragment를 왼쪽부터 평가하고, 일반 평가 예외만 parse recovery로 잡는다. cache/model 결함은 UB로 남긴다. `EParseRecoveryRegression.v`가 순서와 음성 사례를 검사한다. |
| Constructor/FunctionObject 구조형 타입 | 완료 | 잘못된 명목 특수케이스를 제거하고 `TRecordFields "Object" ...`로 번역한다. |
| 정제 타입 생성자 | 완료 | `TMathIntSet`, `TInfinity`, `TBoolSet`, `TStrSet`과 `ty_check_query` 회귀를 추가했다. |
| 진단 메시지 | 완료 | `bad(what)`이 거부한 타입 component를 메시지에 포함한다. |
| 낡은 eager type-resolution 경로 | 완료 | 죽은 `addr_of`/`ty_addrs_needed`와 도달 불가능한 pure `TListOf` 분기를 제거했다. live 경로는 재귀적인 `ty_check_query`다. |
| production fingerprint | 완료 | `src/main/resources` 전체와 `_CoqProject`/`SpecSources.mk`가 선언한 분할 TyModel·Spec 의미론 closure를 포함한다. shard 연속성, facade/import 일치, 누락 파일을 fail-fast하고 `.vo`·로그·validation-only `Exec.v`는 제외한다. |
| runner verdict 중복 | 완료 | modular driver와 persistent worker가 `itree_test_runtime.ml`의 실행·분류·사유·포맷을 공유한다. 존재하지 않던 `exec_diagnostic` 경로를 제거했다. 전체 Python 회귀 50개가 통과했다. |
| `TimeClip` | 완료 | `8.64 × 10^15`를 정확한 Binary64 Number 경계 `±8640000000000000`의 순수 Number 비교로 정규화한다. 원래 oracle에 없던 host query를 만들지 않으며 경계·인접값·분수 회귀가 이를 고정한다. |
| `ToUint8Clamp` | 완료 | clamp, floor, midpoint, round-half-to-even을 기존 실행 가능 IR로 shape-check 정규화한다. NaN, ±Infinity, -0, 양쪽 half-even과 경계를 회귀한다. |
| spec export coverage | 완료 | `FVSpecScan`과 실제 exporter가 같은 정규화 본문을 검사하며 2950/2951이다. 유일한 생략 함수는 `INTRINSICS.Math.random`의 `ERandom`이다. |

Runner 변경 뒤 fresh source build/link와 대표 T000 실행도 확인했다. 결과는 32,440
step의 PASS였고, 같은 closed tree의 shape는 `Tau x 32440` 다음 `Ret VUndef`였다.

## JavaScript 동치 증명 빌드 상태

`FVInitState --js-equiv`는 실제 JavaScript 파일 6개를 처리한다. 앞의 4개는 frontend
보존 검사로 남아 있고, ASI 두 파일은 raw byte가 다르지만 ESMeta의 automatic semicolon
insertion 뒤 effective source, AST, typed host answers가 모두 같아야만 alias를 생성한다.
`JSEquivProof.v`의 `asi_optional_chain_closed_js_equiv`는 이 prepared-program 동일성에서
두 `exec_itree`의 `eutt`를 증명하며 `make js-equiv`로 일반 `coqc`에서 컴파일된다.

이는 optional-chain과 handwritten guard의 계산 동치 증명은 아니다. 그 이전 쌍의
monolithic `vm_compute`는 25분 뒤 compressed memory와 swap 약 124 GB에서 중단됐고,
작은 fuel조차 결과 reification 비용이 비현실적이었다. 그 더 강한 정리는 열린 과제로
남기며 현재 compiled theorem의 범위와 섞어 보고하지 않는다.

이는 Test262 runner의 PASS 판정이나 위 610건 표적 통계와 별개다. 증명 artifact
생성만 확인하려면 `make js-equiv-artifacts`, 정리까지 확인하려면 `make js-equiv`를
사용한다.

## 남은 한 건: `Math.random`

과거 실패 610건 중 유일한 비-PASS는 다음 테스트다.

```
built-ins/Math/random/S15.8.2.14_A1.js
```

현재 결과는 `UNSUPPORTED-EFFECT Take/UB`다. 이는 decimal이나 payload 표현 실패가
아니다. `Math.random`은 비결정적 선택의 의미를 요구하며, Test262 실행기가 증명용
`Take`/`Choose` 이벤트에 임의의 운영 의미를 부여하지 않기 때문에 의도적으로
fail-closed한다.

한 번 관찰한 난수를 cache에 넣어 PASS로 만드는 것은 올바른 수정이 아니다. 다음 중
어떤 의미를 채택할지 별도 설계 결정을 한 뒤 구현해야 한다.

- 실행 시 난수 handler를 두고 ESMeta와 결과 집합을 비교한다.
- `Take`/`Choose`의 비결정적 trace 의미로 허용 결과를 증명한다.
- Test262의 `Math.random` 검사를 만족하는 결정적 witness 정책을 명세하고 정당화한다.

이 결정 전까지 `Math.random` 한 건은 `UNSUPPORTED`로 유지한다.

## 재검증 명령

전체 32,207개 캠페인은 사용자가 명시적으로 요청할 때만 실행한다. 일상 회귀에는
다음을 사용한다.

```sh
# 저장소 루트
sbt "testOnly esmeta.fv.*"

# Rocq 회귀
cd formal
make NumberMathRegression.vo NumberToBigIntRegression.vo \
  EParseRecoveryRegression.vo ITreeCoreRegression.vo
make itree-worker-native
```

`make regressions`와 `make check`는 compiled `JSEquivProof.v`까지 포함한다.

## 구조 감사와 정리 결정

- `Exec.v`는 삭제하지 않는다. production Test262 엔진은
  `Semantics.v`/`ITreeExec.v`지만, `Exec.v`는 bounded differential oracle와
  assertion/proof computation 회귀의 실제 소비자가 남아 있다. PO-013 또는 동등한
  agreement bridge가 생기기 전에는 두 역할이 상호 보완적이다.
- native snapshot lane은 유지한다. 분할 Spec facade의 Rocq 선언 소유권을 보존하는
  얇은 `SpecFuncs`/`SpecGlobals`/`SpecHeap` compatibility module을 생성하고,
  snapshot writer도 실제 선언 모듈을 직접 연다. `itree-native-core`뿐 아니라
  snapshot 생성, native worker 링크, 대표 Test262 실행까지 fresh 검증했다.
- static modular/payload round-trip lane은 production과 독립인 audit oracle이므로
  유지한다. `SpecAlgorithmITree`와 smoke extractor는 diagnostic임을 명시하되,
  동일한 coverage가 production 회귀에 들어오기 전에는 삭제하지 않는다.
- `validation/SpecRun.v`, attic proof, 오래된 binary·탐색 로그는 정리 후보지만 이번
  작업에서는 삭제하지 않았다. 보존된 전체/표적 캠페인 로그가 현재 수치의 근거이고,
  snapshot provenance binding도 아직 열린 과제이므로 먼저 canonical evidence와
  diagnostic coverage를 고정한 뒤 별도 cleanup에서 제거한다.

실패 집합의 상태를 확인할 때는 위 두 보존 로그를 근거로 사용한다. 새 의미론 변경이
있으면 별도 state directory에서 610개 표적 집합을 먼저 실행하고, 그 결과가 안정된
뒤에만 전체 캠페인을 고려한다.
