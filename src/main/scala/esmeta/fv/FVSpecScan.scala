package esmeta.fv

import esmeta.cfgBuilder.CFGBuilder
import esmeta.compiler.Compiler
import esmeta.extractor.Extractor
import esmeta.ir.*
import scala.collection.mutable.{Map => MMap}
import scala.util.{Failure, Success, Try}

/** Scan the compiled spec IR and report which IR constructors block
  * translation into the Rocq model, ordered by how many functions each
  * blocks.
  *
  * SOURCE OF TRUTH. Every rejection below is decided by calling the real
  * exporter (`FVExport.rocqExpr` / `rocqTy` / `rocqFunc`), never by a
  * parallel list of "supported" constructors. An earlier version kept its
  * own whitelist and drifted badly: it reported 2909/2951 translatable
  * while `rocqFunc` actually rejected 1287 functions, because it never
  * checked `rocqTy`'s type whitelist or optional parameters. The final
  * cross-check at the end of `blockers` makes that class of drift
  * impossible to hide — a function with no reported blocker that the
  * exporter still refuses is reported as `UNCAUGHT`.
  *
  * Input: the spec CFG, built here (~30 s) rather than read from a dump,
  * so parameters and types are visible and nothing can be stale.
  *
  * Usage:
  *   sbt "runMain esmeta.fv.FVSpecScan"                 # whole spec
  *   sbt "runMain esmeta.fv.FVSpecScan OrdinaryGet ..." # named closure
  */
object FVSpecScan {

  /** collect every reason the exporter would refuse this function */
  def blockers(f: Func): Set[String] = {
    val found = scala.collection.mutable.Set[String]()

    if (f.params.exists(_.optional)) found += "param:optional"

    val walker = new esmeta.ir.util.UnitWalker {
      override def walk(e: Expr): Unit = {
        e match {
          // the type whitelist is ADR-11's, and it is checked by asking
          // the exporter, not by restating it here
          case ETypeCheck(_, ty) =>
            Try(FVExport.rocqTy(ty)) match
              case Failure(_) => found += s"ty:${ty.ty}"
              case Success(_) => ()
          case _ =>
            // ask the exporter itself; the message names the offending
            // node, so nesting does not misattribute the reason
            Try(FVExport.rocqExpr(e)) match
              case Failure(FVExport.Unsupported(msg)) => found += reasonOf(msg)
              case _                                  => ()
        }
        super.walk(e)
      }
    }
    walker.walk(f.body)

    // cross-check: no silent divergence between this scan and the exporter
    if (found.isEmpty) Try(FVExport.rocqFunc(f)) match
      case Failure(FVExport.Unsupported(msg)) => found += s"UNCAUGHT:$msg"
      case Failure(err) => found += s"UNCAUGHT:${err.getClass.getSimpleName}"
      case Success(_)   => ()

    found.toSet
  }

  /** normalise an exporter message into a stable histogram key.  The
    * message always names the offending node, so a rejection nested deep
    * inside a probed parent is still attributed correctly. */
  private def reasonOf(msg: String): String =
    if (msg.startsWith("expr: ")) s"expr:${msg.drop(6)}"
    else if (msg.startsWith("ty: ")) s"ty:${msg.drop(4)}"
    else if (msg.startsWith("non-integer Math")) "expr:EMath(non-integer)"
    else if (msg.startsWith("cop: ToStr")) "expr:EConvert(ToStr)"
    else if (msg.startsWith("optional param")) "param:optional"
    else msg

  def main(args: Array[String]): Unit = {
    println("[fv] extracting spec and building CFG (this takes a while)")
    val cfg = CFGBuilder(Compiler(Extractor()))
    val funcs = cfg.program.funcs
    println(s"[fv] parsed ${funcs.size} spec functions")

    val selected =
      if (args.isEmpty) funcs
      else {
        val want = args.toSet
        funcs.filter(f => want.exists(w => f.name.contains(w)))
      }
    println(s"[fv] scanning ${selected.size} function(s)")

    val perFunc = selected.map(f => f.name -> blockers(f))
    val clean = perFunc.filter(_._2.isEmpty)
    println(
      f"[fv] translatable as-is: ${clean.size}%d / ${selected.size}%d " +
      f"(${100.0 * clean.size / math.max(1, selected.size)}%.0f%%)",
    )

    val hist = MMap[String, Int]()
    for ((_, bs) <- perFunc; b <- bs) hist(b) = hist.getOrElse(b, 0) + 1
    println("[fv] blocking constructs, by number of functions blocked:")
    for ((c, n) <- hist.toList.sortBy(-_._2).take(25))
      println(f"    $n%5d  $c")
    if (hist.size > 25) println(f"    ... ${hist.size - 25} more")

    // cumulative gain: how many functions unlock as blockers are added
    val ordered = hist.toList.sortBy(-_._2).map(_._1)
    var supported = Set[String]()
    println("[fv] cumulative unlock if implemented in this order:")
    for (c <- ordered.take(12)) {
      supported += c
      val ok = perFunc.count(_._2.subsetOf(supported))
      println(f"    +$c%-34s => $ok%5d / ${selected.size}%d functions")
    }
    if (clean.nonEmpty)
      println("[fv] examples already translatable: " +
        clean.take(8).map(_._1).mkString(", "))
  }
}
