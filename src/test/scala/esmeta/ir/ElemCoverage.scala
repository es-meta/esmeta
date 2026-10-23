package esmeta.ir

import esmeta.ESMetaTest
import esmeta.ir.util.{UnitWalker => IRUnitWalker}
import scala.collection.mutable.{Set => MSet}

/** coverage checks for the IR elements */
trait ElemCoverage extends ESMetaTest {

  /** check whether the given elements cover all the IR elements */
  def checkElemCoverage(desc: String)(elems: => Iterable[IRElem]): Unit =
    for ((category, names) <- leafNames) check(s"$desc ($category)") {
      val covered = coveredNames(elems)
      val missing = names.filterNot(covered.contains)
      if (missing.nonEmpty) fail(s"uncovered: ${missing.mkString(", ")}")
    }

  /** check whether the given elements cover the whole `IRTest` corpus */
  def checkCorpusCoverage(desc: String)(elems: => Iterable[Any]): Unit =
    for ((category, corpus) <- corpus) check(s"$desc ($category)") {
      val handled = elems.toSet
      val missing = corpus.collect { case (name, x) if !handled(x) => name }
      if (missing.nonEmpty) fail(s"unhandled: ${missing.mkString(", ")}")
    }

  /** the `IRTest` corpus by category */
  private def corpus: List[(String, List[(String, IRElem)])] = List(
    "Inst" -> IRTest.allInsts,
    "Expr" -> IRTest.allExprs,
    "Ref" -> IRTest.allRefs,
  )

  /** names of the leaf cases of the IR elements by category */
  private lazy val leafNames: List[(String, List[String])] = List(
    "Inst" -> leaves[Inst],
    "Expr" -> leaves[Expr],
    "Ref" -> leaves[Ref],
  )

  /** names of the leaf cases appearing in the given elements */
  private def coveredNames(elems: Iterable[IRElem]): Set[String] =
    val names: MSet[String] = MSet()
    val walker = new IRUnitWalker {
      override def walk(inst: Inst): Unit =
        names += inst.getClass.getSimpleName; super.walk(inst)
      override def walk(expr: Expr): Unit =
        names += expr.getClass.getSimpleName; super.walk(expr)
      override def walk(ref: Ref): Unit =
        names += ref.getClass.getSimpleName; super.walk(ref)
    }
    elems.foreach {
      case func: Func => walker.walk(func)
      case inst: Inst => walker.walk(inst)
      case expr: Expr => walker.walk(expr)
      case ref: Ref   => walker.walk(ref)
      case _          =>
    }
    names.toSet
}
