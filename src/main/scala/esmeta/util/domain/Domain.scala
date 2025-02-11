package esmeta.util.domain

/** domains */
trait Domain {

  /** element type */
  type Elem

  /** bottom element */
  def Bot: Elem
}
