package esmeta.analyzer.domain.nt

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

trait GrammarSymbolSetDomainDecl { self: Self =>

  /** set domain for nonterminals */
  class GrammarSymbolSetDomain(
    maxSizeOpt: Option[Int] = None, // max size of set
  ) extends GrammarSymbolDomain
    with SetDomain[GrammarSymbol]("nt", maxSizeOpt)
}
