package esmeta.analyzer.domain.nt

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

/** set domain for nonterminals */
class SetDomain(
  maxSizeOpt: Option[Int] = None, // max size of set
) extends nt.Domain
  with domain.SetDomain[Nt]("nt", maxSizeOpt)
