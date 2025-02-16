package esmeta.domain

/** domains */
trait Domain[A] {

  /** top element */
  def Top: A

  /** bottom element */
  def Bot: A
}
