package esmeta.analyzer.es

import esmeta.util.Appender.*
import esmeta.domain.{*, given}, BSet.*, Flat.*

/** bindings */
trait BindingDecl { self: ESAnalyzer =>
  case class Binding(
    value: AbsValue = AbsValue.Bot,
    uninit: Boolean = false,
    absent: Boolean = false,
  )
  object Binding {
    lazy val Top: Binding = Binding(AbsValue.Top, true, true)
    lazy val Exist: Binding = Binding(AbsValue.Top, true, false)
    lazy val Init: Binding = Binding(AbsValue.Top, false, false)
    lazy val Uninit: Binding = Binding(AbsValue.Bot, true, false)
    lazy val Absent: Binding = Binding(AbsValue.Bot, false, true)
    lazy val Bot: Binding = Binding(AbsValue.Bot, false, false)
  }
  given Lattice.Ops[Binding] with
    extension (x: Binding) {
      def isTop: Boolean = x.value.isTop && x.uninit && x.absent
      def isBottom: Boolean = x.value.isBottom && !x.uninit && !x.absent
      def ⊑(y: Binding): Boolean =
        (x.value ⊑ y.value) &&
        (x.uninit ⊑ y.uninit) &&
        (x.absent ⊑ y.absent)
      def ⊔(y: Binding): Binding = Binding(
        x.value ⊔ y.value,
        x.uninit ⊔ y.uninit,
        x.absent ⊔ y.absent,
      )

      def ⊓(y: Binding): Binding = Binding(
        x.value ⊓ y.value,
        x.uninit ⊓ y.uninit,
        x.absent ⊓ y.absent,
      )
    }
  given Rule[Binding] = (app, elem) => {
    import Binding.*
    val Binding(value, uninit, absent) = elem
    var tags = ""
    if (uninit) tags += "U"
    if (absent) tags += "A"
    if (tags.nonEmpty) app >> "[" >> tags >> "] "
    app >> value
  }
}
