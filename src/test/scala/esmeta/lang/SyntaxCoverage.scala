package esmeta.lang

import esmeta.ESMetaTest
import esmeta.lang.util.{UnitWalker => LangUnitWalker}
import scala.collection.mutable.{Set => MSet}

/** coverage checks for the metalanguage syntax */
trait SyntaxCoverage extends ESMetaTest {

  /** check whether the given syntax covers all the metalanguage syntax */
  def checkSyntaxCoverage(desc: String)(syntax: => Iterable[Syntax]): Unit =
    for ((category, names) <- leafNames) check(s"$desc ($category)") {
      val covered = coveredNames(syntax)
      val missing = names.filterNot(covered.contains)
      if (missing.nonEmpty) fail(s"uncovered: ${missing.mkString(", ")}")
    }

  /** check whether the given syntax covers the whole `LangTest` corpus */
  def checkCorpusCoverage(desc: String)(syntax: => Iterable[Syntax]): Unit =
    for ((category, corpus) <- corpus) check(s"$desc ($category)") {
      val handled = syntax.toSet
      val missing = corpus.collect { case (name, x) if !handled(x) => name }
      if (missing.nonEmpty) fail(s"unhandled: ${missing.mkString(", ")}")
    }

  /** the `LangTest` corpus by category */
  private def corpus: List[(String, List[(String, Syntax)])] = List(
    "Step" -> LangTest.allSteps,
    "Expression" -> LangTest.allExprs,
    "Condition" -> LangTest.allConds,
    "Reference" -> LangTest.allRefs,
  )

  /** names of the leaf cases of the metalanguage syntax by category */
  private lazy val leafNames: List[(String, List[String])] = List(
    "Step" -> leaves[Step],
    "Expression" -> leaves[Expression],
    "Condition" -> leaves[Condition],
    "Reference" -> leaves[Reference],
  )

  /** names of the leaf cases appearing in the given syntax */
  private def coveredNames(syntax: Iterable[Syntax]): Set[String] =
    val names: MSet[String] = MSet()
    val walker = new LangUnitWalker {
      override def walk(step: Step): Unit =
        names += step.getClass.getSimpleName; super.walk(step)
      override def walk(expr: Expression): Unit =
        names += expr.getClass.getSimpleName; super.walk(expr)
      override def walk(cond: Condition): Unit =
        names += cond.getClass.getSimpleName; super.walk(cond)
      override def walk(ref: Reference): Unit =
        names += ref.getClass.getSimpleName; super.walk(ref)
    }
    syntax.foreach {
      case step: Step       => walker.walk(step)
      case expr: Expression => walker.walk(expr)
      case cond: Condition  => walker.walk(cond)
      case ref: Reference   => walker.walk(ref)
      case _                =>
    }
    names.toSet
}
