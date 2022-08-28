package esmeta.typing

/** completion record types */
case class CompTy(
  normal: PureValueTy = PureValueTy(),
  abrupt: Boolean = false,
) {

  /** bottom check */
  def isBottom: Boolean =
    this.normal.isBottom &
    !this.abrupt

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
