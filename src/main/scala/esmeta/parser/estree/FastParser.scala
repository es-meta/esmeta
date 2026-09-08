package esmeta.parser.estree

import esmeta.es.{Ast, Syntactic}
import esmeta.parser.{AstFrom, ESParser}
import esmeta.spec.Grammar
import esmeta.util.SystemUtils.readFile

/** ECMAScript parser that builds ASTs through ESTree */
case class FastParser(grammar: Grammar, fallback: Boolean = true) {

  private val converter = ESTreeConverter(grammar)
  private lazy val reference = ESParser(grammar)

  /** get a parser for a goal symbol */
  def apply(goal: String): AstFrom = new AstFrom {

    private val sourceType = if (goal == "Module") "module" else "script"
    private lazy val slow = reference(goal)

    // without Node.js there is nothing to fall back from, so the reference
    // parser is used directly instead of failing once per program
    private lazy val usable = !fallback || ESTreeParser.canUse

    def from(str: String): Ast =
      if (!fallback) convert(str)
      else if (!usable) slow.from(str)
      else
        try convert(str)
        catch { case _: Throwable => slow.from(str) }

    def fromFile(filename: String): Ast = fromFileWithCode(filename)._1

    def fromWithCode(str: String): (Ast, String) = (from(str), str)

    def fromFileWithCode(filename: String): (Ast, String) =
      val code = readFile(filename)
      val ast = from(code)
      update(ast, Some(filename))
      (ast, code)

    private def convert(str: String): Ast =
      converter(ESTreeParser.from(str, sourceType), str, goal)
  }

  /** record the file of every location of an AST */
  private def update(ast: Ast, filename: Option[String]): Unit =
    for (loc <- ast.loc) loc.filename = filename
    ast match
      case ast: Syntactic =>
        for { child <- ast.children.flatten } update(child, filename)
      case _ =>
}
