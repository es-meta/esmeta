package esmeta.spec

import org.jsoup.nodes.*

/** specification utilities */
object Utils {
  import Symbol.*

  /** ordering of productions */
  given Ordering[Production] =
    Ordering.by(prod => (prod.kind.ordinal, prod.lhs.name))

  /** extensions for Elements */
  extension (elem: Element) {

    /** walker for ancestors */
    def walkAncestor[T](
      f: Element => T,
      base: T,
      join: (T, T) => T,
    ): T =
      val parent = elem.parent
      if (parent == null) base
      else join(f(parent), parent.walkAncestor(f, base, join))

    /** checks whether an element is in appendix */
    def isInAnnex: Boolean =
      elem.walkAncestor(_.tagName == "emu-annex", false, _ || _)
  }

  /** extensions for grammars */
  extension (grammar: Grammar) {

    /** get the index mapping for grammars */
    def getIdxMap(forWeb: Boolean = false): Map[String, (Int, Int)] = (for {
      prod <- if (forWeb) grammar.prodsForWeb else grammar.prods
      pair <- prod.getIdxMap
    } yield pair).toMap
  }

  /** extensions for productions */
  extension (prod: Production) {

    /** get the index mapping for productions */
    def getIdxMap: Map[String, (Int, Int)] = (for {
      (rhs, i) <- prod.rhsList.zipWithIndex
      (name, j) <- rhs.allNames.zipWithIndex
    } yield prod.lhs.name + ":" + name -> (i, j)).toMap
  }

  /** extensions for RHSs */
  extension (rhs: Rhs) {

    /** get RHS all names */
    def allNames: List[String] =
      rhs.symbols.foldLeft(List[String]("")) {
        case (names, Terminal(term)) => names.map(_ + term)
        case (names, Nonterminal(name, _, optional)) =>
          names.flatMap(x => {
            if (optional) List(x, x + name) else List(x + name)
          })
        case (names, ButNot(base, _)) =>
          names.map(_ + base.name)
        case (names, ButOnlyIf(base, _, _)) =>
          names.map(_ + base.name)
        case (names, _) => names
      }

    /** get non-terminals in an RHS */
    def getNts: List[Nonterminal] = rhs.symbols.flatMap(_.getNt)

    /** get parameters from RHSs */
    def getRhsParams: List[Param] = {
      import Param.Kind.*
      val names = rhs.getNts.map(_.name)
      val duplicated = names.filter(p => names.count(_ == p) > 1).toSet
      var counter = Map[String, Int]()
      val paramNames = names.map(name => {
        if (duplicated contains name) {
          val k = counter.getOrElse(name, 0)
          counter += name -> (k + 1)
          s"$name$k"
        } else name
      })
      paramNames.map(Param(_, Normal, "unknown"))
    }
  }

  /** extensions for symbols */
  extension (symbol: Symbol) {

    /** get an non-terminal or nothing from a symbol */
    def getNt: Option[Nonterminal] = symbol match {
      case (nt: Nonterminal)     => Some(nt)
      case ButNot(base, _)       => Some(base)
      case ButOnlyIf(base, _, _) => Some(base)
      case _                     => None
    }
  }
}
