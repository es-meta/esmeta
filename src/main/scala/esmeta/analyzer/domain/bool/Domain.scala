package esmeta.analyzer.domain.bool

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.Bool

/** abstract boolean domain */
trait Domain extends domain.Domain[Bool] {

  /** boolean element interfaces */
  extension (elem: Elem) {

    /** unary negation */
    def unary_! : Elem

    /** logical OR */
    def ||(that: Elem): Elem

    /** logical AND */
    def &&(that: Elem): Elem

    /** conditional */
    def cond[A](dom: domain.Domain[A])(
      thenBranch: => dom.Elem,
      elseBranch: => dom.Elem,
    ): dom.Elem =
      (if (elem.gamma contains T) thenBranch else dom.Bot) ⊔
      (if (elem.gamma contains F) elseBranch else dom.Bot)

    /** conditional */
    def cond[A](stDom: state.Domain)(dom: domain.Domain[A])(
      thenBranch: stDom.monad.Result[dom.Elem],
      elseBranch: stDom.monad.Result[dom.Elem],
    ): stDom.monad.Result[dom.Elem] = for {
      st <- stDom.monad.get
    } yield cond(dom)(thenBranch.eval(st), elseBranch.eval(st))
  }
}
