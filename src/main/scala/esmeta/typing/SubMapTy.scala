package esmeta.typing

/** sub map types */
case class SubMapTy(
  key: PureValueTy = PureValueTy(),
  value: PureValueTy = PureValueTy(),
) {

  /** union type */
  def |(that: SubMapTy): SubMapTy = SubMapTy(
    this.key | that.key,
    this.value | that.value,
  )

  /** intersection type */
  def &(that: SubMapTy): SubMapTy = SubMapTy(
    this.key & that.key,
    this.value & that.value,
  )

  /** prune type */
  def --(that: SubMapTy): SubMapTy = SubMapTy(
    this.key -- that.key,
    this.value -- that.value,
  )
}
