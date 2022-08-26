package esmeta.typing.util

import esmeta.LINE_SEP
import esmeta.typing.*
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*

/** stringifier for types */
class Stringifier(target: Stringifier.Target) {
  // elements
  given elemRule: Rule[TypeElem] = (app, elem) =>
    elem match
      case elem: Type => typeRule(app, elem)

  // types
  given typeRule: Rule[Type] = (app, ty) =>
    app >> (ty match // TODO
      case NameT(str) => str
      case _          => "unknown"
    )
}
object Stringifier:
  enum Target { case Lang, IR }
