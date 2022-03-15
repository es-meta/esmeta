package esmeta.lang

import esmeta.lang.util.*

// metalanguage types
// TODO more detailed instead of strings
case class Type(name: String) extends Syntax {
  // normalize type string
  // TODO refactor
  def normalized: Type =
    val trimmed = (if (name startsWith "a ") name.drop(2)
                   else if (name startsWith "an ") name.drop(3)
                   else name).replace("-", "").replace("|", "").trim
    Type(trimmed.split(" ").map(_.capitalize).mkString)
}
object Type extends Parser.From[Type]
