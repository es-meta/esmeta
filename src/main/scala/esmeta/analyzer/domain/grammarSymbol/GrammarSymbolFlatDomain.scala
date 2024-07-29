package esmeta.analyzer.domain.nt

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

trait GrammarSymbolFlatDomainDecl { self: Self =>

  /** flat domain for nonterminals */
  object GrammarSymbolFlatDomain
    extends GrammarSymbolDomain
    with FlatDomain[GrammarSymbol]("nt")
}
