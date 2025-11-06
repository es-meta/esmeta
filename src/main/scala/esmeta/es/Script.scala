package esmeta.es

import esmeta.spec.*
import esmeta.spec.BuiltinPath.*

/** ECMAScript script program */
case class Script(
  code: Code,
  name: String,
  supported: Boolean = true,
) extends ESElem

enum Code:
  case Normal(sourceText: String)
  case Builtin(func: String, thisArg: Option[String], args: List[String])

  override def toString: String = this match
    case Normal(sourceText) => sourceText
    case Builtin(func, thisArg, args) =>
      s"$func${thisArg.fold(args)(_ :: args).mkString("(", ", ", ")")};"

  def length = this.toString.length
