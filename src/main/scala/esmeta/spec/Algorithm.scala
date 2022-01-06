package esmeta.spec

import esmeta.lang

/** abstract algorithms */
case class Algorithm(
  head: Head,
  id: String,
  body: lang.Stmt,
  code: String,
)

/** algorithm heads */
enum Head:
  /** abstract opration (AO) heads */
  case AbstractOperationHead(name: String, params: List[Param])

  /** abstract method heads */
  case AbstractMethodHead(
    base: String,
    methodName: String,
    receiverParam: Param,
    origParams: List[Param],
  )

  /** syntax-directed operation (SOD) heads */
  case SyntaxDirectedOperationHead(
    lhsName: String,
    idx: Int,
    subIdx: Int,
    rhsParams: List[Param],
    methodName: String,
    isStatic: Boolean,
    withParams: List[Param],
    needPrefix: Boolean,
  )

  /** buil-in operation  heads */
  case class BuiltinOperationHead(
    ref: lang.Ref,
    origParams: List[Param],
  )

/** algorithm parameters */
case class Param(
  name: String,
  kind: ParamKind,
)

/** algorithm parameters */
enum ParamKind:
  case Normal, Optional, Variadic
