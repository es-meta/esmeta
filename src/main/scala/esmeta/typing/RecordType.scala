package esmeta.typing

case class RecordTy(names: Set[String] = Set()) {

  /** union type */
  def |(that: RecordTy): RecordTy = RecordTy(this.names | that.names)

  /** intersection type */
  def &(that: RecordTy): RecordTy = RecordTy(this.names & that.names)

  /** prune type */
  def --(that: RecordTy): RecordTy = RecordTy(this.names -- that.names)
}
