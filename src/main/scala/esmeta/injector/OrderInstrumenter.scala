package esmeta.injector

import esmeta.LINE_SEP
import esmeta.cfg.CFG
import esmeta.es.*
import esmeta.es.util.UnitWalker
import esmeta.util.Loc
import scala.collection.mutable.LinkedHashSet

/** Instruments expression evaluation order */
object OrderInstrumenter {

  private val targetNames = Set(
    "AssignmentExpression",
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
    "UnaryExpression",
    "LeftHandSideExpression",
    "PrimaryExpression",
  )

  // `typeof x` is a value even though its operand is a reference
  private def consumesReference(ast: Ast): Boolean = ast match
    case Syntactic("UnaryExpression", _, i, _)  => i != 0
    case Syntactic("UpdateExpression", _, i, _) => i != 0
    case Syntactic("AwaitExpression", _, _, _)  => true
    case Syntactic("YieldExpression", _, _, _)  => true
    case _                                      => false

  // double parentheses keep a top-level comma inside one argument
  private val helper =
    s"""var __order = [];
      |function __instrument(value, k) {
      |  __order.push({ value, k });
      |  return value;
      |}
      |""".stripMargin

  def apply(cfg: CFG, src: String): String = {
    given CFG = cfg
    // source text with the semicolons the parser inserted
    val (ast, source) = cfg.scriptParser.fromWithSourceText(src)
    val targetLocs = LinkedHashSet[Loc]()

    def add(ast: Ast): Unit =
      for { loc <- ast.loc if loc.start.offset < loc.end.offset }
        targetLocs += loc
    // undo what the chain of a child already added
    def unwrap(ast: Ast): Unit =
      for { node <- ast.chains; loc <- node.loc } targetLocs -= loc

    object Collector extends UnitWalker {
      override def walk(ast: Ast): Unit =
        super.walk(ast)
        if (consumesReference(ast)) add(ast)
        else if (targetNames.contains(ast.name) && !isExcluded(ast)) add(ast)
        // a base is read with GetValue even where the access is a reference
        ast match
          case Syntactic("MemberExpression", _, 1 | 2 | 7, Some(base) +: _)
              if !isExcludedBase(base) =>
            add(base)
          case Syntactic("CallExpression", _, 4 | 5 | 7, Some(base) +: _)
              if !isExcludedBase(base) =>
            add(base)
          // a callee reference supplies `this`, so wrap only value callees
          case Syntactic("CallExpression", _, 3, Some(base) +: _)
              if !producesReference(base) =>
            add(base)
          case Syntactic("MemberExpression", _, 6, Some(base) +: _)
              if !isExcludedBase(base) =>
            add(base)
          case Syntactic("NewExpression", _, 1, Some(base) +: _)
              if !isExcludedBase(base) =>
            add(base)
          case Syntactic("OptionalExpression", _, 0 | 1 | 2, Some(base) +: _)
              if !producesReference(base) =>
            add(base)
          // directives (`"use strict"`)
          case Syntactic("ExpressionStatement", _, _, Some(expr) +: _)
              if isStringLiteral(expr) =>
            unwrap(expr)
          // NamedEvaluation positions
          case Syntactic("Initializer", _, _, Some(expr) +: _)
              if isAnonymousFunction(expr) =>
            unwrap(expr)
          case Syntactic(
                "AssignmentExpression",
                _,
                4 | 6 | 7 | 8,
                _ +: Some(rhs) +: _,
              ) if isAnonymousFunction(rhs) =>
            unwrap(rhs)
          case Syntactic("PropertyDefinition", _, 2, _ +: Some(value) +: _)
              if isAnonymousFunction(value) =>
            unwrap(value)
          case _ =>
    }
    Collector.walk(ast)

    val helperOffset = ast.flattenStmt
      .find(stmt => !isStringStatement(stmt))
      .flatMap(_.loc)
      .map(_.start.offset)
      .getOrElse(source.length)

    instrument(source, targetLocs.toVector, helperOffset)
  }

  private def isStringLiteral(ast: Ast): Boolean = ast.chains.exists {
    case Syntactic("Literal", _, 3, _) => true
    case _                             => false
  }

  private def isStringStatement(stmt: Ast): Boolean = stmt.chains.exists {
    case Syntactic("ExpressionStatement", _, _, Some(expr) +: _) =>
      isStringLiteral(expr)
    case _ => false
  }

  private def isAnonymousFunction(ast: Ast): Boolean = ast.chains.exists {
    case Syntactic("ArrowFunction" | "AsyncArrowFunction", _, _, _) => true
    case Syntactic(
          "FunctionExpression" | "GeneratorExpression" |
          "AsyncFunctionExpression" | "AsyncGeneratorExpression" |
          "ClassExpression",
          _,
          _,
          None +: _,
        ) =>
      true
    case _ => false
  }

  // FIXME: replace ad-hoc filtering
  private def isExcluded(ast: Ast): Boolean = ast.chains.exists {
    // NOTE: Wrapping these forms may turn a reference into a value
    case Syntactic("IdentifierReference", _, _, _)      => true
    case Syntactic("SuperProperty", _, _, _)            => true
    case Syntactic("OptionalExpression", _, _, _)       => true
    case Syntactic("OptionalChain", _, _, _)            => true
    case Syntactic("MemberExpression", _, 1 | 2 | 7, _) => true
    case Syntactic("CallExpression", _, 4 | 5 | 7, _)   => true

    // NOTE: Wrapping these forms may break cover grammar
    case Syntactic("ArrayLiteral", _, _, _)                         => true
    case Syntactic("ObjectLiteral", _, _, _)                        => true
    case Syntactic("AssignmentExpression", _, 4 | 5 | 6 | 7 | 8, _) => true
    case _                                                          => false
  }

  // the Loc of `x[e]` ends before the closing `]`
  private def isExcludedBase(ast: Ast): Boolean = ast.chains.exists {
    case Syntactic("MemberExpression", _, 1, _)    => true
    case Syntactic("CallExpression", _, 4, _)      => true
    case Syntactic("IdentifierReference", _, _, _) => true
    case Syntactic("SuperProperty", _, _, _)       => true
    case Syntactic("OptionalExpression", _, _, _)  => true
    case Syntactic("OptionalChain", _, _, _)       => true
    case Syntactic("ArrayLiteral", _, _, _)        => true
    case Syntactic("ObjectLiteral", _, _, _)       => true
    case _                                         => false
  }

  private def producesReference(ast: Ast): Boolean = isExcludedBase(ast) ||
    (ast match
      case Syntactic("MemberExpression", _, 1 | 2 | 7, _) => true
      case Syntactic("CallExpression", _, 4 | 5 | 7, _)   => true
      case _                                              => false
    )

  private def instrument(
    source: String,
    targetLocs: Vector[Loc],
    helperOffset: Int,
  ): String = {
    val sites = targetLocs.zipWithIndex
    val starts = sites.groupBy { case (loc, _) => loc.start.offset }
    val ends = sites.groupBy { case (loc, _) => loc.end.offset }
    val result = StringBuilder()

    for (offset <- 0 to source.length) {
      if (offset == helperOffset) {
        if (result.nonEmpty && !result.last.isWhitespace) result ++= LINE_SEP
        result ++= helper
      }

      for {
        (loc, k) <- ends.getOrElse(offset, Vector()).sortBy {
          case (loc, _) => -loc.start.offset
        }
      } result ++= s"), $k))"

      for {
        (loc, _) <- starts.getOrElse(offset, Vector()).sortBy {
          case (loc, _) => -loc.end.offset
        }
      } result ++= "(__instrument(("

      if (offset < source.length) result += source(offset)
    }
    result.toString
  }
}
