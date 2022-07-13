package esmeta.analyzer.domain

import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*

// domain
trait Domain {
  // bottom element
  val Bot: Elem

  // element
  type Elem <: ElemTrait

  // appender
  given rule: Rule[Elem]

  // element traits
  trait ElemTrait { this: Elem =>
    // partial order
    def ⊑(that: Elem): Boolean

    // join operator
    def ⊔(that: Elem): Elem

    // not partial order
    def !⊑(that: Elem): Boolean = !(this ⊑ that)

    // bottom check
    def isBottom: Boolean = this == Bot

    // conversion to string
    override def toString: String = stringify(this)
  }
}
