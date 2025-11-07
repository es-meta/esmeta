package esmeta.es

import esmeta.util.BaseUtils.*

/** ECMAScript script program */
case class Script(code: Code, name: String, supported: Boolean) extends ESElem

enum Code {
  case Normal(sourceText: String)
  case Builtin(
    func: String,
    thisArg: Option[String],
    args: List[String],
    preStmts: Option[String],
    postStmts: Option[String],
  )

  override def toString: String = this match
    case Normal(sourceText) => sourceText
    case Builtin(func, thisArg, args, preStmts, postStmts) =>
      val pre = preStmts.getOrElse("")
      val post = postStmts.getOrElse("")
      val argsStr = thisArg.fold(args)(_ :: args).mkString("(", ", ", ")")
      val builtinCall = s"$func$argsStr;"
      s"$pre$builtinCall$post"

  def length: Int = this.toString.length

  def targetArgs: List[Target] = this match
    case builtin: Builtin =>
      val args = for {
        (arg, i) <- builtin.args.zipWithIndex
      } yield Target.BuiltinArg(arg, i)
      builtin.thisArg match
        case Some(thisArg) => Target.BuiltinThis(thisArg) :: args
        case None          => args
    case _ => raise("target must be builtin")
}
