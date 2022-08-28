package esmeta.typing

/** unknown type */
case class UnknownTy(msg: String = "unknown") extends Ty {

  /** completion check */
  def isCompletion: Boolean = name contains "Completion"
}
