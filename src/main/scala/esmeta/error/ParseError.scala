package esmeta.error

sealed abstract class ParseError(msg: String)
  extends ESMetaError(msg, "ParseError")

case class TooManySemicolonInsertion(max: Int)
  extends ParseError(s"More than $max semicolon insertions needed")

case class WrongNumberOfParserParams(name: String, list: List[Boolean])
  extends ParseError(s"wrong number of parameters for $name: $list")

case class ESValueParserFailed(str: String)
  extends ParseError(s"ESValueParser failed: $str")

case object UnexpectedParseResult
  extends ParseError(s"unexpected parsing result")
