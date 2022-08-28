package esmeta.ty

/** unknown type */
case class UnknownTy(msg: Option[String] = None) extends Ty {

  /** completion check */
  def isCompletion: Boolean = msg.exists(_ contains "Completion")
}
object UnknownTy:
  def apply(str: String): UnknownTy =
    UnknownTy(if (str == "unknown") None else Some(str))
