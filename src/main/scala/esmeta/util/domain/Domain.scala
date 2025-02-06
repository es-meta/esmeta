package esmeta.util.domain

import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*

// -----------------------------------------------------------------------------
// abstract domain
// -----------------------------------------------------------------------------
trait Domain[A, Elem: ElemOps[A]]:

  /** top element */
  def Top: Elem

  /** bottom element */
  def Bot: Elem

  /** abstraction */
  def alpha(elems: Iterable[A]): Elem

  /** abstraction */
  def alpha(elems: A*): Elem = alpha(elems)

  /** abstraction */
  def apply(elems: Iterable[A]): Elem = alpha(elems)

  /** abstraction */
  def apply(elems: A*): Elem = alpha(elems)
