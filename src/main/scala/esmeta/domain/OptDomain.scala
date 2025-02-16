package esmeta.domain

import esmeta.util.Appender.*

/** abstract optional domain */
class OptDomain[T, E](domain: AbsDomain[T, E])
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
case class AbsOpt[E](value: E, absent: Boolean)

given [E]: Lattice.Ops[AbsOpt[E]] with
  extension (x: AbsOpt[E]) {
    def isTop: Boolean = ???
    def isBottom: Boolean = ???
    def ⊑(y: AbsOpt[E]): Boolean = ???
    def ⊔(y: AbsOpt[E]): AbsOpt[E] = ???
    def ⊓(y: AbsOpt[E]): AbsOpt[E] = ???
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
