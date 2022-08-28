package esmeta.typing

case class RecordTy(names: Set[String] = Set()) {

  /** bottom check */
  def isBottom: Boolean = names.isEmpty

  /** union type */
  def |(that: RecordTy): RecordTy = RecordTy(this.names | that.names)

  /** intersection type */
  def &(that: RecordTy): RecordTy = RecordTy(this.names & that.names)

  /** prune type */
  def --(that: RecordTy): RecordTy = RecordTy(this.names -- that.names)
}
