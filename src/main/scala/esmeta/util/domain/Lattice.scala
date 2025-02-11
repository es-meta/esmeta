package esmeta.util.domain

/** complete lattice */
trait Lattice {

  /** abstract element type */
  type Elem

  /** top element */
  def Top: Elem

  /** bottom element */
  def Bot: Elem

  extension (elem: Elem) {

    /** top element check */
    def isTop: Boolean

    /** bottom element check */
    def isBottom: Boolean

    /** partial order */
    def ⊑(that: Elem): Boolean

    /** not partial order */
    inline def !⊑(that: Elem): Boolean = !(elem ⊑ that)

    /** join operator */
    def ⊔(that: Elem): Elem

    /** meet operator */
    def ⊓(that: Elem): Elem
  }
}
