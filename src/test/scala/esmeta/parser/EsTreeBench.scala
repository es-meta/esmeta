package esmeta.parser

import esmeta.*
import esmeta.es.Ast
import esmeta.extractor.Extractor
import esmeta.parser.estree.FastParser
import esmeta.spec.Grammar
import esmeta.util.HtmlUtils.*
import esmeta.util.SystemUtils.*
import scala.util.Try

/** a benchmark of the two ECMAScript parsers
  *
  * {{{
  * SAMPLE=20 sbt 'Test/runMain esmeta.parser.EsTreeBench tests/test262/test'
  * }}}
  */
object EsTreeBench {

  lazy val grammar: Grammar =
    new Extractor(readFile(SPEC_HTML).toHtml).grammar

  def main(args: Array[String]): Unit =
    // the reference parser needs a deeper stack than a JVM gives by default
    val thread = new Thread(null, () => run(args), "bench", 1L << 29)
    thread.start()
    thread.join()

  private def run(args: Array[String]): Unit =
    val paths = if (args.isEmpty) List(s"$TEST_DIR/es") else args.toList
    val files = paths.flatMap { path =>
      val file = new java.io.File(path)
      if (file.isDirectory)
        walkTree(file)
          .map(_.toString)
          .filter(name => jsFilter(name))
          .toList
          .sorted
      else List(path)
    }
    val sampled = sys.env.get("SAMPLE").map(_.toInt) match
      case Some(step) if step > 1 =>
        files.zipWithIndex.collect {
          case (file, idx) if idx % step == 0 => file
        }
      case _ => files
    val targets = sys.env.get("LIMIT").map(_.toInt).fold(sampled)(sampled.take)
    val codes = targets.map(readFile)

    val goal = sys.env.getOrElse("GOAL", "Script")
    val slow = ESParser(grammar)(goal)
    val bare = FastParser(grammar, fallback = false)(goal)
    val fast = FastParser(grammar, fallback = true)(goal)

    // warm up both parsers, so that the JIT and the resource unpacking do
    // not land on whichever runs first
    val warmup = codes.take(20)
    for (code <- warmup) { Try(slow.from(code)); Try(bare.from(code)) }

    // the programs both parsers accept, to compare like with like: the rest are
    // rejected by the early errors of ESTree, where the fallback does the work
    val accepted = codes.filter(code => Try(bare.from(code)).isSuccess)
    println(
      f"- ${targets.length}%,d files with the goal symbol $goal," +
      f" ${accepted.length}%,d accepted by both",
    )

    println("## parsing the programs both accept")
    val slowTime = time("grammar", accepted, slow)
    val bareTime = time("ESTree ", accepted, bare)
    println(f"- speedup: ${slowTime.toDouble / bareTime}%.1fx")

    println("## parsing every program, as `-fast-parse` does")
    println(f"  (${targets.length - accepted.length}%,d of them fall back)")
    val allSlow = time("grammar", codes, slow)
    val allFast = time("ESTree ", codes, fast)
    println(f"- speedup: ${allSlow.toDouble / allFast}%.1fx")

  private def time(label: String, codes: List[String], parser: AstFrom): Long =
    var ok = 0
    val start = System.nanoTime
    for (code <- codes) if (Try(parser.from(code)).isSuccess) ok += 1
    val elapsed = System.nanoTime - start
    val ms = elapsed / 1000000.0
    println(
      f"- $label: ${ms / 1000}%8.2fs total, " +
      f"${ms / codes.length}%7.2fms per file ($ok%,d parsed)",
    )
    elapsed
}
