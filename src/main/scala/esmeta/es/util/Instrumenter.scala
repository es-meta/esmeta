package esmeta.es.util

import esmeta.RESOURCE_DIR
import esmeta.cfg.CFG
import esmeta.es.*
import esmeta.parser.ESValueParser
import esmeta.spec.*
import esmeta.state.{GLOBAL_RESULT, Undef}
import esmeta.util.{ConcurrentPolicy => CP, ProgressBar}
import esmeta.util.SystemUtils.readFile
import java.util.concurrent.{ConcurrentHashMap => CMMap}
import scala.collection.mutable.LinkedHashSet
import scala.jdk.CollectionConverters.*
import scala.util.Try

class Instrumenter(cfg: CFG) {
  private val cov = Coverage(cfg, timeLimit = Some(2))

  def apply(
    programsBySide: Map[(Int, Boolean), String],
  ): Map[(Int, Boolean), String] = {
    val programs = programsBySide.toList.groupMap(_._2)(_._1)
    val result = CMMap[(Int, Boolean), String](programsBySide.asJava)
    val bar = ProgressBar(
      msg = s"instrumenting ${programs.size} programs",
      iterable = programs,
      concurrent = CP.Fixed(Runtime.getRuntime.availableProcessors),
    )
    bar.foreach { (js, conds) =>
      for {
        (cond, program) <- instrument(js, conds.toSet)
      } result.put(cond, program)
    }
    println(s"Instrumentation: ${bar.summary.time.simpleString}")
    result.asScala.toMap
  }

  def instrument(
    js: String,
    conds: Set[(Int, Boolean)],
  ): Map[(Int, Boolean), String] = {
    val ast = cfg.scriptParser.fromWithSourceText(js)._1
    val sites = positions(ast).zipWithIndex.toMap
    val name = loggerName(js, ast)
    def program(selected: Set[Int]): String =
      render(ast, sites, selected, name)
    val baseline = program(Set.empty)
    val initial = touched(baseline)
    val groups = sites.values.toList.sorted.foldLeft(Map(Set[Int]() -> conds)) {
      (groups, site) =>
        groups.toList
          .flatMap { (selected, keys) =>
            val next = selected + site
            val kept = keys intersect touched(program(next))
            List(next -> kept, selected -> (keys -- kept))
          }
          .filter(_._2.nonEmpty)
          .toMap
    }
    groups.toList.flatMap { (selected, keys) =>
      val wrapped = program(selected)
      keys.map { key =>
        key -> (if (selected.nonEmpty || initial(key)) wrapped else js)
      }
    }.toMap
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
  ): String = {
    val state = s"logState${name.stripPrefix("L")}"
    def text(ast: Ast, path: Vector[Int], top: Boolean): String = {
      val code = ast match {
        case Lexical(_, str) => str
        case syn: Syntactic =>
          val children = syn.children.zipWithIndex
          val nested = top && Set(
            "Script",
            "ScriptBody",
            "StatementList",
            "StatementListItem",
            "Statement",
          )(syn.name)
          if (top && syn.name == "ExpressionStatement" && stringLiteral(syn)) ""
          else if (top && syn.name == "ExpressionStatement")
            val expr = children.collectFirst {
              case (Some(child), i) => text(child, path :+ i, false)
            }.get
            s"$state.results.push(($expr));"
          else {
            val cs = children.iterator
            cfg.grammar
              .nameMap(syn.name)
              .rhsVec(syn.rhsIdx)
              .symbols
              .map {
                case Terminal(term)                          => term
                case Empty | NoLineTerminator | _: Lookahead => ""
                case symbol if symbol.getNt.isDefined =>
                  val (child, i) = cs.next()
                  child.fold("")(text(_, path :+ i, nested))
                case _ => ""
              }
              .filter(_.nonEmpty)
              .mkString(" ")
          }
      }
      sites.get(path).filter(selected).fold(code) { id =>
        s"($name(($code), ${id + 1}))"
      }
    }
    val body = text(ast, Vector.empty, true)
    val directives = ast.flattenStmt
      .takeWhile(stringLiteral)
      .map(_.toString(grammar = Some(cfg.grammar)))
      .mkString("\n")
    val names = LinkedHashSet[String]()
    def bindings(ast: Ast): Unit = ast match {
      case syn: Syntactic if syn.name == "BindingIdentifier" =>
        names += syn.toString(grammar = Some(cfg.grammar))
      case syn: Syntactic
          if Set(
            "Initializer",
            "FormalParameters",
            "FunctionBody",
            "GeneratorBody",
            "AsyncFunctionBody",
            "AsyncGeneratorBody",
            "ClassTail",
            "ComputedPropertyName",
          )(syn.name) =>
      case _ => ast.children.flatten.foreach(bindings)
    }
    ast.flattenStmt
      .filter { stmt =>
        stmt.chains.exists(c =>
          c.name == "Declaration" || c.name == "VariableStatement",
        )
      }
      .foreach(bindings)
    val capture = names.toList
      .map(n => s"$state.results.push($n);")
      .mkString("\n")
    s"""$directives
${Instrumenter.runtime(name)}
try {
$body
$capture
} catch (e) {
  $state.threw = true;
  $state.error = e instanceof Error ? e.name : e;
}"""
  }
}

object Instrumenter {
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
    readFile(s"$RESOURCE_DIR/instrumentation.js").trim

  private def runtime(name: String): String = {
    val suffix = name.stripPrefix("L")
    runtimeTemplate
      .replace("$L", name)
      .replace("$logs", s"logs$suffix")
      .replace("$logState", s"logState$suffix")
  }
}
