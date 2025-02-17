package esmeta.domain

import esmeta.util.Appender.*

/** abstract optional domain */
class OptDomain[T, E: Lattice.Ops: AbsDomain.Ops[T]](domain: AbsDomain[T, E])
  extends AbsDomain[Option[T], AbsOpt[E]]
  with Lattice[AbsOpt[E]] {
  lazy val Top = AbsOpt(domain.Top, true)
  lazy val Bot = AbsOpt(domain.Bot, false)
  lazy val Exists = AbsOpt(domain.Top, false)
  lazy val Absent = AbsOpt(domain.Bot, true)
  override def alpha(elems: Iterable[Option[T]]): AbsOpt[E] = alpha(elems.toSet)
  def apply(opt: Option[E]): AbsOpt[E] = opt.fold(Absent)(AbsOpt(_, false))
  def apply(value: E, absent: Boolean): AbsOpt[E] = AbsOpt(value, absent)
  def alpha(elems: Set[Option[T]]): AbsOpt[E] = AbsOpt(
    value = domain.alpha(elems.flatten),
    absent = elems.contains(None),
  )
}
case class AbsOpt[E: Lattice.Ops](value: E, absent: Boolean) {
  def exists: Flat[Boolean] =
    var set = Set.empty[Boolean]
    if (value.nonBottom) set += true
    if (absent) set += false
    Flat(set)
}

given [E: Lattice.Ops]: Lattice.Ops[AbsOpt[E]] with
  extension (x: AbsOpt[E]) {
    def isTop: Boolean = x.value.isTop && x.absent
    def isBottom: Boolean = x.value.isBottom && !x.absent
    def ⊑(y: AbsOpt[E]): Boolean =
      (x.value ⊑ y.value) && (x.absent ⊑ y.absent)
    def ⊔(y: AbsOpt[E]): AbsOpt[E] =
      AbsOpt(x.value ⊔ y.value, x.absent ⊔ y.absent)
    def ⊓(y: AbsOpt[E]): AbsOpt[E] =
      AbsOpt(x.value ⊓ y.value, x.absent ⊓ y.absent)
  }

given [T, E]: AbsDomain.GenericOps[Option[T], AbsOpt[E]] with
  extension (x: AbsOpt[E]) {
    def contains(value: Option[T]): Boolean = ???
    def toBSet: BSet[Option[T]] = ???
    def toFlat: Flat[Option[T]] = ???
  }

given [E: Rule]: Rule[AbsOpt[E]] = (app, opt) =>
  if (opt.absent) app >> "[?] "
  app >> opt.value
