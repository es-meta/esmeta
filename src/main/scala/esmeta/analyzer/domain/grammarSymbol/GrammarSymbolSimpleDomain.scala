package esmeta.analyzer.domain.nt

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

trait GrammarSymbolSimpleDomainDecl { self: Self =>

  /** simple domain for nonterminals */
  object GrammarSymbolSimpleDomain
    extends GrammarSymbolDomain
    with SimpleDomain[GrammarSymbol]("nt")
}
