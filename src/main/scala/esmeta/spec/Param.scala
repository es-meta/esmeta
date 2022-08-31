package esmeta.spec

import esmeta.lang.*
import esmeta.spec.util.Parser

/** algorithm parameters */
case class Param(
  name: String,
  ty: Type,
  kind: Param.Kind = Param.Kind.Normal,
) extends SpecElem
object Param extends Parser.From[Param](Parser.param):
  enum Kind extends SpecElem:
    case Normal, Optional, Variadic, Ellipsis
