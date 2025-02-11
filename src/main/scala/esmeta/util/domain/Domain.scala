package esmeta.util.domain

/** domains */
trait Domain {

  /** element type */
  type Elem

  /** top element */
  def Top: Elem

  /** bottom element */
  def Bot: Elem
}
