package esmeta.es

/** ECMAScript program code */
enum Code {
  case Simple(sourceText: String)
  case Builtin(form: BuiltinForm)

  override def toString: String = this match
    case Simple(s)  => s
    case Builtin(f) => f.toString

  def length: Int = this.toString.length
}

/** builtin call forms, where `path` is a JS expression accessing the builtin */
enum BuiltinForm {
  case Call(path: String, thisArg: String, args: List[String])
  case Construct(
    path: String,
    args: List[String],
    newTarget: Option[String] = None,
  )

  override def toString: String = this match
    case Call(path, thisArg, args) =>
      val argsStr = (thisArg :: args).mkString(", ")
      s"$path.call($argsStr);"
    case Construct(path, args, Some(nt)) =>
      val argsStr = args.mkString(", ")
      s"Reflect.construct($path, [$argsStr], $nt);"
    case Construct(path, args, None) =>
      val argsStr = args.mkString(", ")
      s"new $path($argsStr);"

  /** mutable operands, in the order they appear in the JS expression */
  def operands: List[String] = this match
    case Call(_, thisArg, args)       => thisArg :: args
    case Construct(_, args, Some(nt)) => args :+ nt
    case Construct(_, args, None)     => args

  /** rebuild with new operands, keeping the builtin path intact */
  def withOperands(ops: List[String]): BuiltinForm = this match
    case Call(p, _, _)         => Call(p, ops.head, ops.tail)
    case Construct(p, _, None) => Construct(p, ops)
    case Construct(p, _, Some(_)) =>
      val (args, nt) = ops.splitAt(ops.size - 1)
      Construct(p, args, Some(nt.head))
}
