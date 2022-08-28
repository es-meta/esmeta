package esmeta.typing

/** completion record types */
case class CompTy(
  normal: PureValueTy = PureValueTy(),
  abrupt: Boolean = false,
) {

  /** union type */
  def |(that: CompTy): CompTy = CompTy(
    this.normal | that.normal,
    this.abrupt | that.abrupt,
  )

  /** intersection type */
  def &(that: CompTy): CompTy = CompTy(
    this.normal & that.normal,
    this.abrupt & that.abrupt,
  )

  /** prune type */
  def --(that: CompTy): CompTy = CompTy(
    this.normal -- that.normal,
    this.abrupt -- that.abrupt,
  )
}
