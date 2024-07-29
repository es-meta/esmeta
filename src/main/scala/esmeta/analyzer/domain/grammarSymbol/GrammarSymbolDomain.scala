package esmeta.analyzer.domain.nt

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.GrammarSymbol

trait GrammarSymbolDomainDecl { self: Self =>

  /** abstract nonterminal domain */
  trait GrammarSymbolDomain extends Domain[GrammarSymbol]
}
