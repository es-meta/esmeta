package esmeta.injector

import esmeta.RESOURCE_DIR
import esmeta.cfg.CFG
import esmeta.es.*
import esmeta.es.util.{Coverage, JsonProtocol, UnitWalker}
import esmeta.es.util.Coverage.Cond
import esmeta.parser.ESValueParser
import esmeta.spec.*
import esmeta.state.{GLOBAL_RESULT, Undef}
import esmeta.util.SystemUtils.{readFile, readJson}
import io.circe.Decoder
import java.io.File
import scala.collection.mutable.{LinkedHashSet, ListBuffer}
import scala.util.Try

class InteractionOracle(cfg: CFG) {
  private val cov = Coverage(cfg, timeLimit = Some(2))

  /** select interaction tests for the branch sides represented by this program
    */
  def apply(
    source: String,
    ownedCoverage: Option[Set[(Int, Boolean)]] = None,
  ): List[String] = {
    val sides = ownedCoverage.getOrElse {
      Try {
        val interp = cov.run(source)
        if (
          !interp.isTimeout && interp.supported &&
          interp.result.globals.contains(GLOBAL_RESULT)
        )
          interp.touchedCondViews.keys
            .map(c => (c.cond.branch.id, c.cond.cond))
            .toSet
        else Set.empty[(Int, Boolean)]
      }.getOrElse(Set.empty)
    }
    if (sides.isEmpty) Nil
    else
      select(source, sides).toList
        .sortBy(_._1)
        .map(_._2)
        .distinct
        .filterNot(_ == source)
  }

  private def select(
    js: String,
    conds: Set[(Int, Boolean)],
  ): Map[(Int, Boolean), String] = {
    val ast = cfg.scriptParser.fromWithSourceText(js)._1
    val sites = positions(ast).zipWithIndex.toMap
    val name = loggerName(js, ast)
    def probe(selected: Set[Int]): String =
      render(ast, sites, selected, name, logging = false)
    lazy val initial = touched(probe(Set.empty))
    val groups = sites.values.toList.sorted.foldLeft(Map(Set[Int]() -> conds)) {
      (groups, site) =>
        groups.toList
          .flatMap { (selected, keys) =>
            val next = selected + site
            val kept = keys intersect touched(probe(next))
            List(next -> kept, selected -> (keys -- kept))
          }
          .filter(_._2.nonEmpty)
          .toMap
    }
    groups.toList.flatMap { (selected, keys) =>
      val wrapped = render(ast, sites, selected, name)
      keys.map { key =>
        key -> (if (selected.nonEmpty || initial(key)) wrapped else js)
      }
    }.toMap
  }

  /** the greedy selection of `select`, running the specification only for the
    * sites the safety analysis cannot vouch for; the others are assumed to keep
    * every branch side. Returns the variants, the probe runs, and the number of
    * candidate sites.
    */
  def selectGuided(
    js: String,
    conds: Set[(Int, Boolean)],
    unsafe: Set[Int],
    unspecified: Set[Int],
  ): (List[String], Int, Int) = {
    val ast = cfg.scriptParser.fromWithSourceText(js)._1
    val sites = positions(ast).zipWithIndex.toMap
    val name = loggerName(js, ast)
    var runs = 0
    def probe(selected: Set[Int]): String =
      render(ast, sites, selected, name, logging = false)
    def run(code: String): Set[(Int, Boolean)] = { runs += 1; touched(code) }
    lazy val initial = run(probe(Set.empty))
    val candidates = sites.values.toList.sorted.filterNot(unspecified)
    val groups = candidates.foldLeft(Map(Set[Int]() -> conds)) {
      (groups, site) =>
        groups.toList
          .flatMap { (selected, keys) =>
            val next = selected + site
            val kept =
              if (unsafe(site)) keys intersect run(probe(next)) else keys
            List(next -> kept, selected -> (keys -- kept))
          }
          .filter(_._2.nonEmpty)
          .toMap
    }
    val chosen = groups.toList.flatMap { (selected, keys) =>
      val wrapped = render(ast, sites, selected, name)
      keys.map { key =>
        key -> (if (selected.nonEmpty || initial(key)) wrapped else js)
      }
    }.toMap
    val variants =
      chosen.toList.sortBy(_._1).map(_._2).distinct.filterNot(_ == js)
    (variants, runs, sites.size)
  }

  /** a program that tags the value of every candidate site (all value sites, as
    * the greedy oracle considers), and the name of its tagger
    */
  def tagging(js: String): Option[(String, String)] = {
    val ast = cfg.scriptParser.fromWithSourceText(js)._1
    val sites = positions(ast).zipWithIndex.toMap
    Option.when(sites.nonEmpty) {
      val name = LazyList.from(0).map(i => s"__tag$i").find(!js.contains(_)).get
      (render(ast, sites, sites.values.toSet, name, tag = true), name)
    }
  }

  def wrap(js: String): String = {
    val ast = cfg.scriptParser.fromWithSourceText(js)._1
    val sites = positions(ast).zipWithIndex.toMap
    val name = loggerName(js, ast)
    render(ast, sites, sites.values.toSet, name)
  }

  private def loggerName(js: String, ast: Ast): String =
    val identifiers = LinkedHashSet[String]()
    val walker = new UnitWalker {
      override def walk(lex: Lexical): Unit =
        ESValueParser.StringValue.of.get(lex.name).foreach { parse =>
          identifiers += parse(lex.str).str
        }
    }
    walker.walk(ast)
    val suffix = LazyList
      .from(0)
      .map(i => if (i == 0) "" else i.toString)
      .find(s =>
        List(s"L$s", s"logs$s", s"logState$s")
          .forall(n => !identifiers(n) && !js.contains(n)),
      )
      .get
    s"L$suffix"

  private def touched(js: String): Set[(Int, Boolean)] = Try {
    val interp = cov.run(js)
    // Reject exceptions that escape the instrumentation's catch.
    if (interp.result(GLOBAL_RESULT) == Undef)
      interp.touchedCondViews.keys
        .map(c => (c.cond.branch.id, c.cond.cond))
        .toSet
    else Set.empty
  }.getOrElse(Set.empty)

  private val operators = Set(
    "Expression",
    "ConditionalExpression",
    "ShortCircuitExpression",
    "CoalesceExpression",
    "LogicalORExpression",
    "LogicalANDExpression",
    "BitwiseORExpression",
    "BitwiseXORExpression",
    "BitwiseANDExpression",
    "EqualityExpression",
    "RelationalExpression",
    "ShiftExpression",
    "AdditiveExpression",
    "MultiplicativeExpression",
    "ExponentiationExpression",
  )

  private def positions(ast: Ast): Vector[Vector[Int]] = {
    val sites = LinkedHashSet[Vector[Int]]()
    def visit(ast: Ast, path: Vector[Int]): Unit = {
      val children = ast.children.zipWithIndex.collect {
        case (Some(child), i) => (child, i)
      }
      for ((child, i) <- children)
        val pattern = i == 0 &&
          Set("AssignmentExpression", "ForInOfStatement")(ast.name) &&
          children.size > 1 && child.chains.exists(c =>
            c.name == "ArrayLiteral" || c.name == "ObjectLiteral",
          )
        if (!pattern) visit(child, path :+ i)
      def add(child: Ast, i: Int): Unit =
        unwrapped(child) match
          case Lexical(
                "NullLiteral" | "BooleanLiteral" | "NumericLiteral" |
                "StringLiteral" | "BigIntLiteral",
                _,
              ) =>
          case _ => sites += path :+ i
      ast match {
        case syn: Syntactic =>
          val terms =
            cfg.grammar.nameMap(syn.name).rhsVec(syn.rhsIdx).ts.map(_.term)
          val values = syn.name match {
            case "ArgumentList" | "ElementList" | "SpreadElement" |
                "PropertyDefinition" =>
              children.filter(_._1.name == "AssignmentExpression")
            case "Initializer"                                => children
            case "ExpressionStatement" if !stringLiteral(syn) => children
            case "ReturnStatement" | "ThrowStatement" | "ConciseBody" |
                "AsyncConciseBody" | "ComputedPropertyName" =>
              children.filter((c, _) => c.name.endsWith("Expression"))
            case "MemberExpression" | "CallExpression"
                if terms.contains(".") || terms.contains("[") =>
              children.take(1).filterNot((c, _) => c.name == "Super") ++
              children.drop(1).filter(_._1.name == "Expression")
            case "MemberExpression" | "NewExpression"
                if terms.contains("new") =>
              children.take(1)
            case "AssignmentExpression" if children.size > 1 =>
              children.drop(1).filter(_._1.name == "AssignmentExpression")
            case "UnaryExpression"
                if terms.nonEmpty &&
                !terms.contains("typeof") &&
                !terms.contains("delete") =>
              children
            case "AwaitExpression" | "YieldExpression" =>
              children.filter((c, _) => c.name.endsWith("Expression"))
            case name if operators(name) && children.size > 1 =>
              children.filterNot(_._1.name == "MultiplicativeOperator")
            case _ => Vector.empty
          }
          for ((child, i) <- values) add(child, i)
        case _ =>
      }
    }
    visit(ast, Vector.empty)
    sites.toVector
  }

  private def unwrapped(ast: Ast): Ast = ast match {
    case syn: Syntactic
        if cfg.grammar.nameMap(syn.name).rhsVec(syn.rhsIdx).ts.isEmpty =>
      syn.children.flatten match
        case Vector(child) => unwrapped(child)
        case _             => ast
    case _ => ast
  }

  private def stringLiteral(ast: Ast): Boolean = {
    val expr = ast.chains
      .collectFirst {
        case Syntactic("ExpressionStatement", _, _, Some(expr) +: _) => expr
      }
      .getOrElse(ast)
    unwrapped(expr) match
      case Lexical("StringLiteral", _) => true
      case _                           => false
  }

  private def render(
    ast: Ast,
    sites: Map[Vector[Int], Int],
    selected: Set[Int],
    name: String,
    logging: Boolean = true,
    tag: Boolean = false,
  ): String = {
    val logStateName = s"logState${name.stripPrefix("L")}"
    val prologue = ast.flattenStmt.takeWhile(stringLiteral)
    val freshNames =
      Injector.resultNames(ast, ast.toString(grammar = Some(cfg.grammar)))
    val resultNames = ListBuffer.empty[String]
    def text(ast: Ast, path: Vector[Int], isTopLevel: Boolean): String = {
      val code = ast match {
        case Lexical(_, str) => str
        case syn: Syntactic =>
          val children = syn.children.zipWithIndex
          val childrenAreTopLevel = isTopLevel && Set(
            "Script",
            "ScriptBody",
            "StatementList",
            "StatementListItem",
            "Statement",
          )(syn.name)
          if (isTopLevel && syn.name == "ExpressionStatement") {
            if (prologue.exists(_.chains.exists(_ eq syn))) ""
            else {
              val resultName = freshNames.next()
              resultNames += resultName
              val expr = children.collectFirst {
                case (Some(child), i) => text(child, path :+ i, false)
              }.get
              s"$resultName = ($expr);"
            }
          } else {
            val childIterator = children.iterator
            cfg.grammar
              .nameMap(syn.name)
              .rhsVec(syn.rhsIdx)
              .symbols
              .map {
                case Terminal(term)                          => term
                case Empty | NoLineTerminator | _: Lookahead => ""
                case symbol if symbol.getNt.isDefined =>
                  val (child, i) = childIterator.next()
                  child.fold("")(text(_, path :+ i, childrenAreTopLevel))
                case _ => ""
              }
              .filter(_.nonEmpty)
              .mkString(" ")
          }
      }
      sites.get(path).filter(selected).fold(code) { k =>
        if (tag) s"($name(($code), $k))" else s"($name(($code)))"
      }
    }
    val body = text(ast, Vector.empty, true)
    val directives = prologue
      .map(_.toString(grammar = Some(cfg.grammar)))
      .mkString("\n")
    val declarations =
      if (resultNames.isEmpty) "" else resultNames.mkString("let ", ", ", ";")
    if (tag) s"""$directives
var $name = (v, k) => v;
$declarations
try {
$body
} catch (e) {}"""
    else s"""$directives
${InteractionOracle.runtime(name, logging)}
$declarations
try {
$body
} catch (e) {
  $logStateName.threw = true;
  $logStateName.error = e instanceof Error ? e.name : e;
}"""
  }
}

object InteractionOracle {

  /** reuse solver or fuzzer ownership data when it accompanies the input */
  def loadOwnedCoverage(
    cfg: CFG,
    dir: File,
  ): Map[String, Set[(Int, Boolean)]] = {
    val roots = List(dir, dir.getParentFile).filter(_ != null)
    roots
      .find(root => new File(root, "branch-coverage.json").isFile)
      .fold(
        Map.empty[String, Set[(Int, Boolean)]],
      ) { root =>
        val protocol = JsonProtocol(cfg)
        import protocol.given
        given Decoder[(Cond, String)] = c =>
          for {
            cond <- c.downField("condView").downField("cond").as[Cond]
            script <- c.downField("script").as[String]
          } yield (cond, script)
        readJson[List[(Cond, String)]](
          new File(root, "branch-coverage.json").getPath,
        ).groupMap { (_, script) =>
          val direct = new File(root, script)
          val file =
            if (direct.isFile) direct else new File(root, s"minimal/$script")
          file.getCanonicalPath
        } { (cond, _) => (cond.branch.id, cond.cond) }
          .map((path, sides) => path -> sides.toSet)
      }
  }

  /** replace logging helpers with trap-free helpers for spec execution */
  def withoutTraps(
    cfg: CFG,
    ast: Ast,
    source: String,
    observers: List[String],
  ): String = {
    val names = observers.map(name => s"L${name.stripPrefix("logState")}").toSet
    val edits = ast.flattenStmt.flatMap { stmt =>
      for {
        name <- stmt.chains.collectFirst {
          case Syntactic("VariableDeclaration", _, _, Some(binding) +: _) =>
            binding.toString(grammar = Some(cfg.grammar)).trim
        }
        if names(name)
        loc <- stmt.loc
      } yield {
        val template = runtime(name, logging = false)
        val helper = template.substring(template.indexOf(s"var $name ="))
        (loc.start.offset, loc.end.offset, helper)
      }
    }
    edits.sortBy(-_._1).foldLeft(source) {
      case (text, (start, end, helper)) =>
        text.patch(start, helper, end - start)
    }
  }

  def observers(cfg: CFG, ast: Ast): List[String] =
    ast.flattenStmt.toList.flatMap { stmt =>
      stmt.chains
        .collectFirst {
          case Syntactic("VariableDeclaration", _, _, Some(binding) +: _) =>
            binding.toString(grammar = Some(cfg.grammar)).trim
        }
        .filter { name =>
          name.matches("L[0-9]*") &&
          stmt == cfg.scriptParser
            .fromWithSourceText(runtime(name))
            ._1
            .flattenStmt
            .last
        }
        .map(name => s"logState${name.stripPrefix("L")}")
    }

  private lazy val runtimeTemplate =
    readFile(s"$RESOURCE_DIR/interaction-oracle.js").trim

  private lazy val probeTemplate = runtimeTemplate.replaceAll(
    """(?s)/\* \$traps:start \*/.*?/\* \$traps:end \*/""",
    "id,",
  )

  private def runtime(name: String, logging: Boolean = true): String = {
    val suffix = name.stripPrefix("L")
    val template = if (logging) runtimeTemplate else probeTemplate
    template
      .replace("$L", name)
      .replace("$logs", s"logs$suffix")
      .replace("$logState", s"logState$suffix")
  }
}
