package esmeta.parser.estree

import esmeta.es.{Ast, Syntactic}
import esmeta.parser.{AstFrom, ESParser}
import esmeta.spec.Grammar
import esmeta.util.SystemUtils.readFile

/** ECMAScript parser that builds ASTs through ESTree
  *
  * It is a drop-in replacement of [[esmeta.parser.ESParser]] for the `Script`
  * and `Module` goal symbols, trading the ability to parse arbitrary
  * nonterminals for speed.
  *
  * An ESTree parser rejects more programs than the grammar of ECMA-262 alone
  * does, because it also checks the early errors of the specification -- ESMeta
  * instead evaluates those errors on the AST, so it has to parse such programs
  * first. Whenever the ESTree parser refuses a program, the reference parser is
  * therefore used as a fallback, which keeps the observable behavior the same
  * and confines the speedup to the programs that are accepted.
  *
  * One difference remains on purpose: the reference parser inserts the
  * semicolons of automatic semicolon insertion into the source and reports the
  * rewritten text, while this parser reports the text it was given. The ASTs
  * agree either way, and the locations of this one refer to the real source.
  *
  * That choice is why the locations here are for diagnostics only, and never
  * record the text they point into. The specification reads the source text of
  * a node back -- `[[SourceText]]`, and the `covered by` phrases that re-parse
  * it -- and the stringifier of ESMeta slices the recorded text whenever a node
  * has one. Slicing a source that was never rewritten is unsound: re-parsing
  * such a slice inserts the semicolons again and shifts the very offsets that
  * were used to cut it. Leaving the text unrecorded makes the specification
  * rebuild it from the tree instead, which no rewriting can disturb, and is
  * what the reference parser also does when it parses a file.
  *
  * @param fallback
  *   whether to fall back to [[esmeta.parser.ESParser]]; turning it off is
  *   useful to test the converter itself.
  */
case class FastParser(grammar: Grammar, fallback: Boolean = true) {

  private val converter = EsTreeConverter(grammar)
  private lazy val reference = ESParser(grammar)

  /** get a parser for a goal symbol */
  def apply(goal: String): AstFrom = new AstFrom {

    private val sourceType = if (goal == "Module") "module" else "script"
    private lazy val slow = reference(goal)

    // without Node.js there is nothing to fall back from, so the reference
    // parser is used directly instead of failing once per program
    private lazy val usable = !fallback || EsTreeParser.canUse

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
      converter(EsTreeParser.from(str, sourceType), str, goal)
  }

  /** record the file of every location of an AST */
  private def update(ast: Ast, filename: Option[String]): Unit =
    for (loc <- ast.loc) loc.filename = filename
    ast match
      case ast: Syntactic =>
        for { child <- ast.children.flatten } update(child, filename)
      case _ =>
}
