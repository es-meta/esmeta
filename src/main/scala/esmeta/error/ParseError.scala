package esmeta.error

sealed abstract class ParseError(msg: String)
  extends ESMetaError(s"[ParseError] $msg")

case class TooManySemicolonInsertion(max: Int)
  extends ParseError(s"More than $max semicolon insertions needed")

case class WrongNumberOfParserParams(name: String, list: List[Boolean])
  extends ParseError(s"wrong number of parameters for $name: $list")
