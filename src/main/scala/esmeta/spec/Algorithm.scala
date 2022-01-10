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
  /** abstract operation (AO) heads */
  case AbstractOperationHead(
    name: String,
    params: List[Param],
    isHostDefined: Boolean,
  )

  /** numeric method heads */
  case NumericMethodHead(ty: String, name: String, params: List[Param])

  /** syntax-directed operation (SOD) heads */
  case SyntaxDirectedOperationHead(
    lhsName: String,
    idx: Int,
    subIdx: Int,
    rhsParams: List[Param],
    methodName: String,
    isStatic: Boolean,
    withParams: List[Param],
  )

  /** concrete method heads */
  case ConcreteMethodHead(
    methodName: String,
    receiverParam: Param,
    params: List[Param],
  )

  /** internal method heads */
  case InternalMethodHead(
    methodName: String,
    receiverParam: Param,
    params: List[Param],
  )

  /** buil-in heads */
  case BuiltinHead(
    ref: String, // TODO more precisely represent references
    params: List[Param],
  )

/** algorithm parameters */
case class Param(
  name: String,
  kind: Param.Kind,
  ty: String, // TODO more precisely represent parameter types
)
object Param:
  enum Kind:
    case Normal, Optional, Variadic
