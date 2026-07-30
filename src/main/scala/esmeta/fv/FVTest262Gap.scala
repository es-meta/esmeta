package esmeta.fv

import esmeta.cfg.CFG
import esmeta.cfgBuilder.CFGBuilder
import esmeta.compiler.Compiler
import esmeta.es.*
import esmeta.extractor.Extractor
import esmeta.interpreter.Interpreter
import esmeta.state.*
import esmeta.test262.Test262

/** How far is the model from running real Test262 tests?
  *
  * For each sampled test, run it with ESMeta while recording which spec
  * functions the run enters, then intersect that set with the functions
  * `FVExport` cannot translate. The intersection is exactly what the
  * model would get stuck on — a reachability-driven work list, as opposed
  * to the whole-spec frequency histogram, which over-counts work that no
  * real test ever reaches.
  *
  * Measurement only; writes nothing.
  *
  * Usage: sbt "runMain esmeta.fv.FVTest262Gap [n]"
  */
object FVTest262Gap {

  def main(args: Array[String]): Unit = {
    val n = args.headOption.flatMap(_.toIntOption).getOrElse(20)
    println("[fv] extracting spec and building CFG")
    val cfg = CFGBuilder(Compiler(Extractor()))
    given CFG = cfg
    val t262 = Test262(Test262.getVersion(None), cfg)

    // ESMeta's own filter lists decide which tests it claims to support
    val targets = t262.allTargetTests
    println(s"[fv] ESMeta's supported Test262 tests: ${targets.size}")
    val lang = targets.filter(_.relName.startsWith("language/"))
    println(s"[fv] of those, under language/: ${lang.size}")

    val omitted: Map[String, Set[String]] = cfg.program.funcs
      .map(f => f.name -> FVSpecScan.blockers(f))
      .filter(_._2.nonEmpty)
      .toMap
    println(s"[fv] spec functions the exporter cannot translate: ${omitted.size}")

    val sample = lang.sortBy(_.relName).grouped(math.max(1, lang.size / n))
      .map(_.head).take(n).toList
    println(s"[fv] sampling ${sample.size} test(s)\n")

    var runnable = 0
    val missHist = scala.collection.mutable.Map[String, Int]()
    val blockerHist = scala.collection.mutable.Map[String, Int]()
    for (t <- sample) {
      val path = s"${esmeta.TEST262_TEST_DIR}/${t.relName}"
      val res =
        try {
          val (ast, code) = t262.loadTest(path)
          val st = Initialize(cfg).from(code, ast)
          val visited = scala.collection.mutable.Set[String]()
          val interp = new Interpreter(st, timeLimit = Some(60)) {
            override def step: Boolean =
              visited += this.st.context.func.name
              super.step
          }
          interp.result
          Right(visited.toSet)
        } catch { case e: Throwable => Left(e.toString.take(80)) }
      res match
        case Left(err) => println(f"  ${t.relName}%-58s ESMeta failed: $err")
        case Right(visited) =>
          val miss = visited.filter(omitted.contains)
          if (miss.isEmpty) runnable += 1
          for (m <- miss) {
            missHist(m) = missHist.getOrElse(m, 0) + 1
            for (b <- omitted(m)) blockerHist(b) = blockerHist.getOrElse(b, 0) + 1
          }
          println(f"  ${t.relName}%-58s entered=${visited.size}%4d " +
            f"missing=${miss.size}%3d")
    }

    println(s"\n[fv] tests whose whole reachable set is already exported: " +
      s"$runnable / ${sample.size}")
    println("[fv] most-wanted missing functions (by tests blocked):")
    for ((f, c) <- missHist.toList.sortBy(-_._2).take(20))
      println(f"[fv]   $c%3d  $f  <-  ${omitted(f).toList.sorted.mkString(", ")}")
    println("[fv] blockers behind them (by test-function pairs):")
    for ((b, c) <- blockerHist.toList.sortBy(-_._2).take(15))
      println(f"[fv]   $c%4d  $b")
  }
}
