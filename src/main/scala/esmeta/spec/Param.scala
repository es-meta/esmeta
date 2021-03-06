package esmeta.spec

/** algorithm parameters */
case class Param(
  name: String,
  kind: Param.Kind = Param.Kind.Normal,
  ty: Type = UnknownType,
) extends SpecElem
object Param:
  enum Kind extends SpecElem:
    case Normal, Optional, Variadic, Ellipsis
