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
  * using only the current metalanguage `Parser` (no `LegacyParser`), no
  * manual algorithms and no bugfix patches. Algorithms are taken from all
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

  /** count unparsed steps in a spec.html content */
  def count(label: String, html: String): Result =
    val document = html.toHtml
    val extractor = new Extractor(document)
    val elems = document.getElems("emu-alg:not([example])")
    var (steps, yetSteps, yetExprs, withExpr, errors) = (0, 0, 0, 0, 0)
    var (algsWithYet, headed, headedWithYet) = (0, 0, 0)
    val details = for {
      (elem, idx) <- elems.zipWithIndex
      code = elem.html.unescapeHtml
      heads =
        try extractor.extractHeads(elem).map(_._1.fname)
        catch { case _: Throwable => Nil }
      clause = Option(elem.parent).map(_.id).getOrElse("")
      c = Counter()
      error =
        try { c.walk(Parser.parseBy(Parser.step)(code)); None }
        catch {
          case e: Throwable => Some(String.valueOf(e.getMessage).take(200))
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
  private val PrPattern = "pr:(\\d+)".r
  private val ImportPattern =
    """<emu-import\s+href="([^"]+)"\s*>\s*(</emu-import>)?""".r

  /** label and git revision of a target (`rev`, `label=rev`, or `pr:N`) */
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
      val refspecs = group.map(n => s"+refs/pull/$n/head:refs/esmeta-count-yets/pr/$n")
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

  /** count all targets, printing a summary line for each */
  def run(
    targets: List[String],
    repo: String = ECMA262_DIR,
    out: Option[String] = None,
  ): List[Result] =
    fetchPullRequests(targets, repo)
    val results = for (target <- targets) yield {
      val (label, rev) = parseTarget(target)
      val result = count(label, specAt(repo, rev))
      println(result.line)
      result
    }
    for (dir <- out) {
      mkdir(dir)
      dumpFile(
        Json.obj(results.map(r => r.label -> r.summary): _*).spaces2,
        s"$dir/summary.json",
      )
      dumpFile(results.map(_.line).mkString("", "\n", "\n"), s"$dir/summary.txt")
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
