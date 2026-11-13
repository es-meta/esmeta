package esmeta.opcount

import esmeta.ECMA262_DIR
import esmeta.extractor.Extractor
import esmeta.lang.*
import esmeta.lang.util.{Parser, UnitWalker}
import esmeta.util.HtmlUtils.*
import esmeta.util.SystemUtils.*
import io.circe.*, io.circe.syntax.*
import scala.sys.process.*

/** Count unparsed (`Yet`) steps in every `<emu-alg>` of ECMA-262 revisions,
  * using only the current metalanguage `Parser` (no `LegacyParser`), no manual
  * algorithms and no bugfix patches. Algorithms are taken from all
  * `emu-alg:not([example])` elements, whether or not their heads can be
  * extracted, and `<emu-import>`ed files are inlined.
  */
object CountYets {

  /** attributes each Yet to the innermost enclosing SubStep */
  class Counter extends UnitWalker {
    var steps = 0
    var yetSteps = 0 // steps that are YetStep as a whole
    var yetExprs = 0 // YetExpression occurrences
    var stepsWithYetExpr = 0 // non-Yet steps containing >= 1 YetExpression
    val yets = scala.collection.mutable.ListBuffer[String]()
    private var stack: List[Boolean] = Nil // has-yet flag per open SubStep
    override def walk(subStep: SubStep): Unit =
      steps += 1
      stack = false :: stack
      subStep.step match
        case YetStep(YetExpression(str, block)) =>
          yetSteps += 1
          yets += str
          block.foreach(walk)
        case _ => super.walk(subStep)
      if (stack.head && !subStep.step.isInstanceOf[YetStep])
        stepsWithYetExpr += 1
      stack = stack.tail
    override def walk(expr: Expression): Unit = expr match
      case YetExpression(str, block) =>
        yetExprs += 1
        yets += s"[expr] $str"
        if (stack.nonEmpty) stack = true :: stack.tail
        block.foreach(walk)
      case _ => super.walk(expr)
  }

  /** counting result of a single specification */
  case class Result(
    label: String,
    algorithms: Int,
    steps: Int,
    yetSteps: Int,
    stepsWithYetExpr: Int,
    yetExprs: Int,
    algorithmsWithYet: Int,
    parseErrors: Int,
    headedAlgorithms: Int,
    headedAlgorithmsWithYet: Int,
    details: List[Json], // only algorithms with a yet or a parse error
  ) {
    def unparsedSteps: Int = yetSteps + stepsWithYetExpr
    def ratio: Double = 100.0 * unparsedSteps / (steps max 1)
    def line: String =
      f"$label%-24s algs=$algorithms%5d steps=$steps%6d yetSteps=$yetSteps%5d " +
      f"stepsWithYetExpr=$stepsWithYetExpr%4d unparsed=$unparsedSteps%5d " +
      f"($ratio%.2f%%) algsWithYet=$algorithmsWithYet%4d " +
      f"errors=$parseErrors%d headed=$headedAlgorithms%d"
    def summary: Json = Json.obj(
      "algorithms" -> algorithms.asJson,
      "steps" -> steps.asJson,
      "yetSteps" -> yetSteps.asJson,
      "stepsWithYetExpr" -> stepsWithYetExpr.asJson,
      "unparsedSteps" -> unparsedSteps.asJson,
      "yetExprs" -> yetExprs.asJson,
      "algorithmsWithYet" -> algorithmsWithYet.asJson,
      "parseErrors" -> parseErrors.asJson,
      "headedAlgorithms" -> headedAlgorithms.asJson,
      "headedAlgorithmsWithYet" -> headedAlgorithmsWithYet.asJson,
    )
  }

  /** normalize the formatting of an `<emu-alg>` body without touching its
    * wording, as ecmarkup accepts more than the metalanguage parser:
    *   - drop carriage returns and expand tabs to two spaces
    *   - number every step `1.` (ecmarkup renumbers them anyway)
    *   - indent each nesting level by two spaces
    *   - join a step's text wrapped onto following lines into one line (lines
    *     inside HTML blocks such as `<figure>` keep their offset from the step
    *     above them)
    */
  def normalize(code: String): String =
    val StepNum = "^( *)\\d+\\.( .*|)$".r
    val BlockOpen = "<(figure|table|pre|emu-table|emu-grammar)\\b".r
    val BlockClose = "</(figure|table|pre|emu-table|emu-grammar)>".r
    var stack: List[Int] = Nil // original indents of open levels
    var base = -1
    var last: Option[(Int, Int)] = None // (original, new) indent of last step
    var depth = 0 // nesting of HTML blocks
    val out = scala.collection.mutable.ArrayBuffer[String]()
    for (line <- code.replace("\r", "").replace("\t", "  ").split("\n", -1)) {
      val indent = line.takeWhile(_ == ' ').length
      val trimmed = line.trim
      line match
        case StepNum(_, rest) if depth == 0 =>
          while (stack.nonEmpty && stack.head > indent) stack = stack.tail
          if (stack.isEmpty || stack.head < indent) stack = indent :: stack
          if (base < 0) base = indent
          val ni = base + 2 * (stack.size - 1)
          last = Some((indent, ni))
          out += " " * ni + "1." + rest
        case _ =>
          val before = depth
          depth += BlockOpen.findAllIn(trimmed).size
          last match
            case Some((oi, ni)) if trimmed.nonEmpty =>
              if (before == 0 && depth == 0 && out.nonEmpty)
                out(out.size - 1) += " " + trimmed
              else out += " " * (ni + indent - oi).max(0) + line.drop(indent)
            case _ => out += line
          depth = (depth - BlockClose.findAllIn(trimmed).size).max(0)
    }
    out.mkString("\n")

  /** count unparsed steps in a spec.html content */
  def count(label: String, html: String): Result =
    val document = html.toHtml
    val extractor = new Extractor(document)
    val elems = document.getElems("emu-alg:not([example])")
    var (steps, yetSteps, yetExprs, withExpr, errors) = (0, 0, 0, 0, 0)
    var (algsWithYet, headed, headedWithYet) = (0, 0, 0)
    val details = for {
      (elem, idx) <- elems.zipWithIndex
      code = normalize(elem.html.unescapeHtml)
      heads =
        try extractor.extractHeads(elem).map(_._1.fname)
        catch { case _: Throwable => Nil }
      clause = Option(elem.parent).map(_.id).getOrElse("")
      c = Counter()
      error =
        try { c.walk(Parser.parseBy(Parser.step)(code)); None }
        catch {
          case e: Throwable =>
            // the whole algorithm is unparsed: count its numbered steps
            // (except `Else`/`Otherwise`, which the AST folds into `If`)
            val n = StepLine.findAllIn(code).count(!ElseLine.matches(_))
            c.steps = n; c.yetSteps = n; c.stepsWithYetExpr = 0
            Some(String.valueOf(e.getMessage).take(200))
        }
      hasYet = c.yetSteps + c.stepsWithYetExpr > 0 || error.isDefined
      _ = {
        if (error.isDefined) errors += 1
        steps += c.steps; yetSteps += c.yetSteps; yetExprs += c.yetExprs
        withExpr += c.stepsWithYetExpr
        if (hasYet) algsWithYet += 1
        if (heads.nonEmpty) { headed += 1; if (hasYet) headedWithYet += 1 }
      }
      if hasYet
    } yield Json.obj(
      "idx" -> idx.asJson,
      "clause" -> clause.asJson,
      "heads" -> heads.asJson,
      "steps" -> c.steps.asJson,
      "yetSteps" -> c.yetSteps.asJson,
      "stepsWithYetExpr" -> c.stepsWithYetExpr.asJson,
      "yetExprs" -> c.yetExprs.asJson,
      "yets" -> c.yets.toList.asJson,
      "error" -> error.asJson,
    )
    Result(
      label,
      elems.size,
      steps,
      yetSteps,
      withExpr,
      yetExprs,
      algsWithYet,
      errors,
      headed,
      headedWithYet,
      details,
    )

  // ---------------------------------------------------------------------------
  // reading ECMA-262 revisions
  // ---------------------------------------------------------------------------
  private val StepLine = "(?m)^[ \\t]*\\d+\\.[ \\t].*$".r
  private val ElseLine = "[ \\t]*\\d+\\.[ \\t]+(Else|Otherwise)\\b.*".r
  private val PrPattern = "pr:(\\d+)".r
  private val FilePattern = "file:(.+)".r
  private val ImportPattern =
    """<emu-import\s+href="([^"]+)"\s*>\s*(</emu-import>)?""".r

  /** label and source of a target (`rev`, `label=rev`, `pr:N`, or
    * `label=file:<path>`)
    */
  def parseTarget(target: String): (String, String) = target match
    case PrPattern(n) => (s"pr$n", s"refs/esmeta-count-yets/pr/$n")
    case _ =>
      target.split("=", 2) match
        case Array(label, rev) => (label, rev)
        case _                 => (target, target)

  /** fetch heads of pull requests (`pr:N` targets) at once */
  def fetchPullRequests(targets: List[String], repo: String): Unit =
    val prs = targets.collect { case PrPattern(n) => n }
    for (group <- prs.grouped(200)) {
      val refspecs =
        group.map(n => s"+refs/pull/$n/head:refs/esmeta-count-yets/pr/$n")
      val code = (Seq("git", "-C", repo, "fetch", "-q", "origin") ++ refspecs).!
      if (code != 0) throw Exception(s"failed to fetch pull requests: $group")
    }

  /** file content at a git revision */
  def show(repo: String, rev: String, path: String): String =
    val out = new java.io.ByteArrayOutputStream
    val code = (Seq("git", "-C", repo, "show", s"$rev:$path") #> out).!
    if (code != 0) throw Exception(s"cannot read $rev:$path")
    out.toString("UTF-8")

  /** spec.html at a git revision with `<emu-import>`s inlined recursively */
  def specAt(repo: String, rev: String): String =
    def inlined(path: String): String =
      val base = Option(java.nio.file.Paths.get(path).getParent)
      ImportPattern.replaceAllIn(
        show(repo, rev, path),
        m => {
          val target = base.fold(java.nio.file.Paths.get(m.group(1)))(
            _.resolve(m.group(1)),
          )
          scala.util.matching.Regex.quoteReplacement(
            inlined(target.normalize.toString),
          )
        },
      )
    inlined("spec.html")

  /** a local spec file (e.g., a proposal's spec.emu) with `<emu-import>`s
    * inlined and its `<ins>`/`<del>` changes applied
    */
  def specFile(path: String): String =
    def inlined(p: java.nio.file.Path): String =
      ImportPattern.replaceAllIn(
        readFile(p.toString),
        m => {
          val target = p.getParent.resolve(m.group(1)).normalize
          scala.util.matching.Regex.quoteReplacement(
            if (java.nio.file.Files.isRegularFile(target)) inlined(target)
            else "",
          )
        },
      )
    val p = java.nio.file.Paths.get(path).toAbsolutePath
    if (!java.nio.file.Files.isRegularFile(p))
      throw Exception(s"no such spec file: $path")
    applyInsDel(inlined(p))

  /** drop `<del>` contents, unwrap `<ins>`, and drop emptied steps */
  def applyInsDel(html: String): String =
    val noDel = "(?s)<del\\b[^>]*>.*?</del>".r.replaceAllIn(html, "")
    val noIns = "</?ins\\b[^>]*>".r.replaceAllIn(noDel, "")
    "(?m)^[ \\t]*\\d+\\.[ \\t]*\\n".r.replaceAllIn(noIns, "")

  /** count all targets, printing a summary line for each */
  def run(
    targets: List[String],
    repo: String = ECMA262_DIR,
    out: Option[String] = None,
  ): List[Result] =
    fetchPullRequests(targets, repo)
    val results = for (target <- targets) yield {
      val (label, rev) = parseTarget(target)
      val html = rev match
        case FilePattern(path) => specFile(path)
        case _                 => specAt(repo, rev)
      val result = count(label, html)
      println(result.line)
      result
    }
    for (dir <- out) {
      mkdir(dir)
      dumpFile(
        Json.obj(results.map(r => r.label -> r.summary): _*).spaces2,
        s"$dir/summary.json",
      )
      dumpFile(
        results.map(_.line).mkString("", "\n", "\n"),
        s"$dir/summary.txt",
      )
      dumpFile(
        Json.obj(results.map(r => r.label -> r.details.asJson): _*).spaces2,
        s"$dir/details.json",
      )
    }
    results

  /** usage: CountYets <out dir> <target> ... */
  def main(args: Array[String]): Unit =
    run(args.toList.tail, out = Some(args(0)))
}
