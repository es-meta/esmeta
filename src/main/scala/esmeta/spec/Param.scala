package esmeta.spec

import esmeta.lang.*
import esmeta.spec.util.Parser

/** algorithm parameters */
case class Param(
  name: String,
  ty: Type,
  kind: ParamKind = ParamKind.Normal,
) extends SpecElem
object Param extends Parser.From(Parser.param)
enum ParamKind extends SpecElem:
  case Normal, Optional, Variadic, Ellipsis
