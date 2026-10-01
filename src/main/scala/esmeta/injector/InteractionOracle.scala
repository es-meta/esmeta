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
    else {
      val ast = cfg.scriptParser.fromWithSourceText(source)._1
      val sites = positions(ast).zipWithIndex.toMap
      val selected = sites.values.toSet
      val name = loggerName(source, ast)
      val covered = touched(render(ast, sites, selected, name, logging = false))
      Option
        .when(sides.exists(covered)) {
          render(ast, sites, selected, name)
        }
        .toList
    }
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

  private def positions(ast: Ast): Vector[Vector[Int]] = {
    type Site = (Ast, Vector[Int])
    val sites = LinkedHashSet[Vector[Int]]()
    def children(node: Ast, path: Vector[Int]): Vector[Site] =
      node.children.zipWithIndex.collect {
        case (Some(child), i) => (child, path :+ i)
      }
    def peel(node: Ast, path: Vector[Int]): Site = node match {
      case syn: Syntactic
          if cfg.grammar.nameMap(syn.name).rhsVec(syn.rhsIdx).ts.isEmpty =>
        children(node, path) match {
          case Vector((child, childPath)) => peel(child, childPath)
          case _                          => (node, path)
        }
      case _ => (node, path)
    }
    def add(node: Ast, path: Vector[Int]): Unit = unwrapped(node) match {
      case Lexical(
            "NullLiteral" | "BooleanLiteral" | "NumericLiteral" |
            "StringLiteral" | "BigIntLiteral",
            _,
          ) =>
      case _ => sites += path
    }
    def array(node: Ast, path: Vector[Int]): Unit = {
      val (value, valuePath) = peel(node, path)
      value.name match {
        case "ArrayLiteral" | "ElementList" | "SpreadElement" =>
          for ((child, childPath) <- children(value, valuePath)) {
            if (
              child.name == "AssignmentExpression" && value.name != "SpreadElement"
            )
              add(child, childPath)
            else array(child, childPath)
          }
        case "Elision" =>
        case _         => add(node, path)
      }
    }
    def arguments(node: Ast, path: Vector[Int]): Vector[Site] = {
      if (node.name == "AssignmentExpression") Vector((node, path))
      else
        children(node, path).flatMap { (child, childPath) =>
          arguments(child, childPath)
        }
    }
    def visit(node: Ast, path: Vector[Int]): Unit = {
      val (call, callPath) = peel(node, path)
      val parts = children(call, callPath)
      val args = parts.find(_._1.name == "Arguments")
      args match {
        case Some((args, argsPath))
            if parts.size >= 2 && Set(
              "CoverCallExpressionAndAsyncArrowHead",
              "CallExpression",
              "MemberExpression",
            )(call.name) =>
          val (callee, calleePath) = parts.head
          val name =
            callee.toString(grammar = Some(cfg.grammar)).replaceAll("\\s+", "")
          val inputs = arguments(args, argsPath)
          if (name == "Reflect.construct") {
            inputs.zipWithIndex.foreach {
              case ((input, inputPath), i) =>
                if (i == 1) array(input, inputPath) else add(input, inputPath)
            }
          } else {
            val (entry, entryPath) = peel(callee, calleePath)
            val receiver = children(entry, entryPath)
            val method = name.endsWith(".call") || (
              entry.name == "MemberExpression" && receiver.size > 1 &&
              !call.toString(grammar = Some(cfg.grammar)).startsWith("new ")
            )
            if (method) receiver.headOption.foreach { (value, valuePath) =>
              add(value, valuePath)
            }
            else add(callee, calleePath)
            inputs.foreach { (input, inputPath) =>
              val spread = input.parent.exists {
                case syn: Syntactic =>
                  cfg.grammar
                    .nameMap(syn.name)
                    .rhsVec(syn.rhsIdx)
                    .ts
                    .exists(_.term == "...")
                case _ => false
              }
              if (spread) array(input, inputPath) else add(input, inputPath)
            }
          }
        case _ =>
          parts.foreach { (child, childPath) => visit(child, childPath) }
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
      sites.get(path).filter(selected).fold(code) { _ => s"($name(($code)))" }
    }
    val body = text(ast, Vector.empty, true)
    val directives = prologue
      .map(_.toString(grammar = Some(cfg.grammar)))
      .mkString("\n")
    val declarations =
      if (resultNames.isEmpty) "" else resultNames.mkString("let ", ", ", ";")
    val runtime = InteractionOracle.runtime(name, logging)
    val split = runtime.indexOf(s"function $name()")
    s"""$directives
${runtime.take(split).trim}
$declarations
try {
$body
} catch (e) {
  $logStateName.threw = true;
  $logStateName.error = e instanceof Error ? e.name : e;
}
// Logging Proxy
${runtime.drop(split)}"""
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
          case Syntactic("FunctionDeclaration", _, _, Some(binding) +: _) =>
            binding.toString(grammar = Some(cfg.grammar)).trim
        }
        if names(name)
        loc <- stmt.loc
      } yield {
        val template = runtime(name, logging = false)
        val helper = template.substring(template.indexOf(s"function $name()"))
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
          case Syntactic("FunctionDeclaration", _, _, Some(binding) +: _) =>
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
