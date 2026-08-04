package esmeta.parser

import esmeta.*
import esmeta.es.Ast
import esmeta.es.util.AstDiff
import esmeta.extractor.Extractor
import esmeta.parser.estree.FastParser
import esmeta.spec.Grammar
import esmeta.util.HtmlUtils.*
import esmeta.util.SystemUtils.*
import scala.collection.mutable.{Map => MMap}

/** a development harness that compares the two ECMAScript parsers
  *
  * {{{
  * sbt 'Test/runMain esmeta.parser.EsTreeCheck tests/es'
  * SAMPLE=40 sbt 'Test/runMain esmeta.parser.EsTreeCheck tests/test262/test'
  * }}}
  */
object EsTreeCheck {

  /** the grammar alone, without the cost of extracting the algorithms */
  lazy val grammar: Grammar =
    new Extractor(readFile(SPEC_HTML).toHtml).grammar

  /** the goal symbol, `Script` or `Module` */
  lazy val goal: String = sys.env.getOrElse("GOAL", "Script")

  lazy val slowParser: AstFrom = ESParser(grammar)(goal)
  lazy val fastParser: AstFrom = FastParser(grammar, fallback = false)(goal)

  /** the `module` flag of a Test262 test */
  private val moduleFlag = raw"flags:\s*\[[^\]]*\bmodule\b".r

  def main(args: Array[String]): Unit =
    // the reference parser is a packrat parser and needs a deep stack for the
    // most nested programs of Test262, deeper than the default of a JVM
    val thread = new Thread(null, () => run(args), "check", 1L << 29)
    thread.start()
    thread.join()

  private def run(args: Array[String]): Unit =
    val paths = if (args.isEmpty) List(s"$TEST_DIR/es") else args.toList
    val (dirs, named) =
      paths.partition(path => new java.io.File(path).isDirectory)
    val files = dirs.flatMap { path =>
      walkTree(new java.io.File(path))
        .map(_.toString)
        .filter(name => jsFilter(name))
        .toList
        .sorted
    }
    // `SAMPLE=n` keeps every n-th file, spreading the sample over directories
    val sampled = sys.env.get("SAMPLE").map(_.toInt) match
      case Some(step) if step > 1 =>
        files.zipWithIndex.collect {
          case (file, idx) if idx % step == 0 => file
        }
      case _ => files
    // a scanned directory of Test262 holds mostly scripts, so with the module
    // goal only the tests flagged as modules are kept; a file given by name is
    // always kept, whatever it holds
    val selected =
      if (goal != "Module") sampled
      else
        sampled.filter(file => moduleFlag.findFirstIn(readFile(file)).isDefined)
    val scanned =
      sys.env.get("LIMIT").map(_.toInt).fold(selected)(selected.take)
    val targets = named ++ scanned
    println(s"- checking ${targets.length} files with the goal symbol $goal")

    var same = 0
    var neither = 0
    val diffs = MMap[String, List[(String, String)]]()
    val onlySlow = MMap[String, List[String]]()
    val onlyFast = MMap[String, List[String]]()
    def add[T](
      map: MMap[String, List[T]],
      key: String,
      value: T,
    ): Unit = map += key -> (value :: map.getOrElse(key, Nil))

    val start = System.currentTimeMillis
    for (file <- targets)
      val code = readFile(file)
      (attempt(slowParser.from(code)), attempt(fastParser.from(code))) match
        case (Left(_), Left(_)) => neither += 1
        case (Right(_), Left(e)) =>
          add(onlyFast, message(e), file)
        case (Left(e), Right(_)) =>
          add(onlySlow, message(e), file)
        case (Right(expect), Right(actual)) if sys.env.contains("LOC") =>
          // `LOC=1` also compares the locations, which the ASTs do not carry
          // into their equality, using the stringifier of ESMeta
          val shown = (ast: Ast) => ast.toString(location = true)
          if (shown(expect) == shown(actual)) same += 1
          else
            val reason = firstLocDiff(shown(expect), shown(actual))
            add(diffs, group(reason), (reason, file))
        case (Right(expect), Right(actual)) =>
          AstDiff.parentLinks(actual) match
            case Some(reason) => add(diffs, group(reason), (reason, file))
            case None         =>
          AstDiff(expect, actual) match
            case None => same += 1
            case Some(reason) =>
              add(diffs, group(reason), (reason, file))
              if (sys.env.contains("DUMP"))
                println(s"### $file")
                println(s"- reason: $reason")
                println(s"- expected:\n${dump(expect)}")
                println(s"- actual:\n${dump(actual)}")
    val elapsed = (System.currentTimeMillis - start) / 1000.0

    val diffCount = diffs.values.map(_.length).sum
    println(f"- done in $elapsed%.1fs")
    println(s"- identical ASTs         : $same")
    println(s"- DIFFERENT ASTs         : $diffCount")
    println(s"- rejected by both       : $neither")
    println(s"- rejected by ESTree only: ${onlyFast.values.map(_.length).sum}")
    println(s"- rejected by ESMeta only: ${onlySlow.values.map(_.length).sum}")

    if (diffs.nonEmpty)
      println()
      println("### AST differences, most frequent first")
      for ((key, cases) <- diffs.toList.sortBy(-_._2.length)) {
        println(f"  ${cases.length}%5d  $key")
        for ((reason, file) <- cases.take(2)) {
          println(s"           $reason")
          println(s"           in $file")
        }
      }

    report(
      "### rejected by the ESTree parser only (fallback applies)",
      onlyFast,
    )
    report("### rejected by the reference parser only", onlySlow)

  private def report(title: String, map: MMap[String, List[String]]): Unit =
    if (map.nonEmpty)
      println()
      println(title)
      for ((key, files) <- map.toList.sortBy(-_._2.length).take(40)) {
        println(f"  ${files.length}%5d  $key")
        for (file <- files.take(3)) println(s"           e.g. $file")
      }

  /** the first line on which two stringified ASTs differ */
  private def firstLocDiff(expect: String, actual: String): String =
    val pairs = expect.linesIterator.zip(actual.linesIterator)
    pairs.find { case (l, r) => l != r } match
      case Some((l, r)) => s"loc ${l.trim.take(80)} != ${r.trim.take(80)}"
      case None         => "loc: different number of lines"

  /** parse without letting a failure of one file end the comparison
    *
    * A `StackOverflowError` of the packrat parser has to be caught too, which
    * rules out [[scala.util.Try]].
    */
  private def attempt(parse: => Ast): Either[Throwable, Ast] =
    try Right(parse)
    catch { case e: Throwable => Left(e) }

  /** a compact view of the shape of an AST, for diagnosing a difference */
  def dump(ast: Ast, indent: String = ""): String = ast match
    case lex: esmeta.es.Lexical => s"$indent${lex.name} `${lex.str}`"
    case syn: esmeta.es.Syntactic =>
      val head =
        s"$indent${syn.name}[${syn.rhsIdx}]${syn.args.mkString("(", ",", ")")}"
      (head :: syn.children.toList.map {
        case Some(child) => dump(child, indent + "  ")
        case None        => s"$indent  -"
      }).mkString("\n")

  /** the part of a message that groups similar failures together */
  private def group(reason: String): String =
    reason.takeWhile(_ != '@').trim.take(160)

  private def message(e: Throwable): String =
    Option(e.getMessage)
      .map(_.linesIterator.next.replaceAll("\\(\\d+:\\d+\\)", "").trim)
      .getOrElse(e.getClass.getName)
}
