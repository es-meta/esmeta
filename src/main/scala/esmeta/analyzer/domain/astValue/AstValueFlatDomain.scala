package esmeta.analyzer.domain.astValue

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

trait AstValueFlatDomainDecl { self: Self =>

  /** flat domain for AST values */
  object AstValueFlatDomain
    extends AstValueDomain
    with FlatDomain[AstValue]("AST") {

    /** element interfaces */
    extension (elem: Elem) {

      /** join operator */
      override def ⊔(that: Elem): Elem = (elem, that) match
        case (Bot, _)                     => that
        case (_, Bot)                     => elem
        case (Base(l), Base(r)) if l == r => elem
        case _ => exploded(s"Merged AST value is not supported")
    }
  }
}
