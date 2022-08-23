package esmeta.ai.domain.addr

import esmeta.ai.*
import esmeta.ai.domain.*
import esmeta.ir.*
import esmeta.state.Addr
import esmeta.util.Appender.*
import java.util.ResourceBundle.Control

/** abstract address domain */
case class Domain(val Part: Partition)
  extends domain.Domain[Addr]
  with Prunable[Addr]
  with Meetable[Addr] {

  /** address partition elements */
  type Part = Part.Elem

  /** abstract partition element domain */
  val AbsPart = new domain.Domain[Part] with domain.SetDomain[Part]("addr")
  type AbsPart = AbsPart.Elem

  /** elements */
  case class Elem(part: AbsPart) extends Iterable[Part] {

    /** iterators */
    final def iterator: Iterator[Part] = part.iterator
  }

  /** top element */
  val Top = exploded("top abstract address is not supported")

  /** bottom element */
  val Bot = Elem(AbsPart.Bot)

  /** abstraction functions */
  def alpha(elems: Iterable[Addr]): Elem = Top

  /** abstraction functions with contexts */
  def alpha(parts: Set[Part]): Elem = Elem(AbsPart(parts))
  def alpha(parts: Part*): Elem = Elem(AbsPart(parts))
  def apply(parts: Set[Part]): Elem = Elem(AbsPart(parts))
  def apply(parts: Part*): Elem = Elem(AbsPart(parts))

  /** appender */
  given rule: Rule[Elem] = (app, elem) => app >> elem.part

  /** element interfaces */
  extension (elem: Elem) {

    /** partial order */
    def ⊑(that: Elem): Boolean = elem.part ⊑ that.part

    /** join operator */
    def ⊔(that: Elem): Elem = Elem(elem.part ⊔ that.part)

    /** meet operator */
    def ⊓(that: Elem): Elem = Elem(elem.part ⊓ that.part)

    /** prune operator */
    def -(that: Elem): Elem = Elem(elem.part - that.part)

    /** contains check */
    def contains(target: Part): Boolean = elem.part contains target
  }
}
