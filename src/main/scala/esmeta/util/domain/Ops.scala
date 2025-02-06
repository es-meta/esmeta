package esmeta.util.domain

/** operations on elements of an abstract domain */
trait Ops[A, Elem]:
  extension (elem: Elem)

    /** top element check */
    def isTop: Boolean

    /** bottom element check */
    def isBottom: Boolean

    /** partial order */
    def ⊑(that: Elem): Boolean
    inline def <=(that: Elem): Boolean = elem ⊑ that

    /** join operator */
    def ⊔(that: Elem): Elem
    inline def ||(that: Elem): Elem = elem ⊔ that

    /** meet operator */
    def ⊓(that: Elem): Elem
    inline def &&(that: Elem): Elem = elem ⊓ that

    /** contains operator */
    def contains(value: A): Boolean

    /** concretization to set domain */
    def gamma: BSet[A]

    /** concretization to flat domain */
    def getSingle: Flat[A]

/** type alias to use ElemOps as context bound */
type ElemOps[A] = [T] =>> Ops[A, T]
