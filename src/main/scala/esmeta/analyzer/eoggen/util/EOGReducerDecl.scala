package esmeta.analyzer.eoggen.util

import esmeta.analyzer.eoggen.*
import esmeta.cfg.*
import esmeta.ir.{ISdoCall}
import esmeta.util.*
import esmeta.util.BaseUtils.raise
import esmeta.util.HtmlUtils.escapeES
import scala.annotation.{tailrec, targetName}
import esmeta.util.SystemUtils.dumpFile
import scala.util.chaining.*
import esmeta.util.HtmlUtils.escapeHtml

trait EOGReducerDecl { self: Self =>

  object Reducer {
    def apply(eog: EOG): EOG = fix(reduce)(eog)

    // why this works weird
    private def reduce = reduceNode >>> reduceBranch >>> reduceDeadEnd

    /* reduce p => node -> c as p => c, when node is not marked */
    private def reduceNode(eog: EOG): EOG = {
      val target = (for {
        np <- eog.nodes
        cSet = eog.edges.collect { case `np` -> child => child }
        if (cSet.size == 1) // only one child
        child <- cSet
        if !np.marked
      } yield np -> child).headOption

      for { (np -> child) <- target } yield eog.copy(
        nodes = eog.nodes - np,
        edges = eog.edges.flatMap {
          case `np` -> `child` => None
          case `np` -> other   => raise("unreachable")
          case parent -> `np`  => Some(parent -> child)
          case fallback        => Some(fallback)
        },
      )
    }.getOrElse(eog)

    /* reduce 'foldable' branchfes */
    private def reduceBranch(eog: EOG): EOG = {
      val target = (for {
        np <- eog.nodes
        cSet = eog.edges.collect { case `np` -> child => child }
        if cSet.size == 2 // only two children
        arbitrary <- cSet // iterate all cases
        // assert : np -> arbitrary
        // ccSet = eog.edges.collect { case `arbitrary` -> child => child }
        // if ccSet.size == 2 // two grand-children (branch)
        cpOpt = eog.nodes.find(cp =>
          eog.edges.contains(np -> cp) &&
          eog.edges.contains(arbitrary -> cp),
        )
        cp <- cpOpt // common grand-child
        // assert : np ------------> cp
        //           \-> arbitrary /^
        if !np.marked
        if !arbitrary.marked
      } yield np -> arbitrary -> cp).headOption

      for { (np -> arbitrary -> cp) <- target } yield
        val edges = eog.edges.flatMap {
          case `np` -> `cp` => None
          case fallback     => Some(fallback)
        }
        eog.copy(edges = edges)
    }.getOrElse(eog)

    /* remove non-return point ends */
    private def reduceDeadEnd(eog: EOG): EOG = {
      val target = (for {
        cp <- eog.nodes
        if cp != exitPoint
        outs = eog.edges.collect { case `cp` -> c => c }
        if outs.isEmpty // no children
        if !cp.marked
      } yield cp).headOption

      for (np <- target)
        yield eog.copy(
          nodes = eog.nodes - np,
          edges = eog.edges.filter {
            case p -> `np` => false
            case `np` -> c => raise("unreachable")
            case _         => true
          },
        )
    }.getOrElse(eog)

    extension [A, B](f: A => B) {
      inline def >>>[C](g: B => C): A => C = (x: A) => g(f(x))
    }
  }
}
