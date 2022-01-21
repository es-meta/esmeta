package esmeta.spec

import esmeta.lang.{Step, YetStep, StepCollector}
import esmeta.util.HtmlUtils.*
import org.jsoup.nodes.*

/** specification utilities */
object Utils {

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

    /** checks whether an element is of Chapter 5. Notational Conventions */
    def isNotation: Boolean =
      elem.parent match {
        case null => false
        case parent =>
          if (parent.id == "sec-notational-conventions") true
          else parent.isNotation
      }
  }

  // TODO optimize this by removing redundant computation
  /** extensions for specifications */
  extension (spec: Spec) {

    /** get incomplete algorithms */
    def incompleteAlgorithms: List[Algorithm] =
      spec.algorithms.filter(!_.complete)

    /** get complete algorithms */
    def completeAlgorithms: List[Algorithm] =
      spec.algorithms.filter(_.complete)

    /** get all algorithm steps */
    def allSteps: List[Step] = for {
      algo <- spec.algorithms
      step <- algo.steps
    } yield step

    /** get incomplete algorithm steps */
    def incompleteSteps: List[Step] =
      allSteps.filter(_.isInstanceOf[YetStep])

    /** get complete algorithm steps */
    def completeSteps: List[Step] =
      allSteps.filter(!_.isInstanceOf[YetStep])
  }

  // TODO optimize this by removing redundant computation
  /** extensions for algorithms */
  extension (algo: Algorithm) {

    /** get element of algorithm */
    def getElem(docu: Document): Option[Element] =
      val id = algo.id
      if id == "" then None
      else {
        val query = s"emu-clause[id=${id}]:not([example])"
        val elems = docu.select(query).toList
        // TODO : case when elems.length > 2 ?
        if elems.length == 1 then Some(elems.last) else None
      }

    /** check whether it is incomplete */
    def complete: Boolean = incompleteSteps.isEmpty

    /** get all steps */
    def steps: List[Step] = StepCollector(algo.body)

    /** get incomplete algorithm steps */
    def incompleteSteps: List[Step] =
      steps.filter(_.isInstanceOf[YetStep])

    /** get complete algorithm steps */
    def completeSteps: List[Step] =
      steps.filter(!_.isInstanceOf[YetStep])
  }

  /** extensions for grammars */
  extension (grammar: Grammar) {

    /** get the index mapping for grammars */
    def idxMap(forWeb: Boolean = false): Map[String, (Int, Int)] = (for {
      prod <- if (forWeb) grammar.prodsForWeb else grammar.prods
      pair <- prod.idxMap
    } yield pair).toMap
  }

  /** extensions for productions */
  extension (prod: Production) {

    /** get name */
    def name: String = prod.lhs.name

    /** get the index mapping for productions */
    def idxMap: Map[String, (Int, Int)] = (for {
      (rhs, i) <- prod.rhsList.zipWithIndex
      (name, j) <- rhs.allNames.zipWithIndex
    } yield prod.lhs.name + ":" + name -> (i, j)).toMap

    /** get non-terminals in RHSs */
    def getNts: List[Nonterminal] = for {
      rhs <- prod.rhsList
      nt <- rhs.getNts
    } yield nt
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
