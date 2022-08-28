package esmeta.spec

import esmeta.lang.*
import esmeta.spec.util.Parser

/** algorithm parameters */
case class Param(
  name: String,
  kind: Param.Kind = Param.Kind.Normal,
  ty: Type = UnknownType,
) extends SpecElem
object Param extends Parser.From[Param]:
  enum Kind extends SpecElem:
    case Normal, Optional, Variadic, Ellipsis
