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
    "UnaryExpression",
    "LeftHandSideExpression",
    "PrimaryExpression",
  )

  private val helper =
    s"""var __order = [];
      |function __instrument(value, k) {
      |  __order.push({ value, k });
      |  return value;
      |}
      |""".stripMargin

  def apply(cfg: CFG, src: String): String = {
    given CFG = cfg
    val (ast, source) = cfg.scriptParser.fromWithCode(src)
    val targetLocs = LinkedHashSet[Loc]()

    object Collector extends UnitWalker {
      override def walk(ast: Ast): Unit =
        super.walk(ast)
        if (targetNames.contains(ast.name) && !isExcluded(ast)) {
          for { loc <- ast.loc if loc.start.offset < loc.end.offset }
            targetLocs += loc
        }
    }
    Collector.walk(ast)

    val helperOffset = ast.flattenStmt.headOption
      .flatMap(_.loc)
      .map(_.start.offset)
      .getOrElse(source.length)

    instrument(source, targetLocs.toVector, helperOffset)
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
      } result ++= s", $k))"

      for {
        (loc, _) <- starts.getOrElse(offset, Vector()).sortBy {
          case (loc, _) => -loc.end.offset
        }
      } result ++= "(__instrument("

      if (offset < source.length) result += source(offset)
    }
    result.toString
  }
}
