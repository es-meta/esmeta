package esmeta.es.util

import esmeta.es.*

/** structural differences between two ASTs
  *
  * ASTs compare equal only when their productions, arguments, alternatives, and
  * children all agree, so a plain `!=` says nothing about where they diverge.
  */
object AstDiff {

  /** the first difference between two ASTs, if any
    *
    * @param upToLineTerminators
    *   compare the text of lexical nodes with every line terminator sequence
    *   normalized to a line feed, which is how the specification reads them
    */
  def apply(
    expect: Ast,
    actual: Ast,
    upToLineTerminators: Boolean = false,
  ): Option[String] =
    diff(expect, actual, expect.name, upToLineTerminators)

  /** the first broken parent link of an AST, if any
    *
    * The parent of a node is not part of its equality, but the specification
    * walks it: `IsInTailPosition` climbs to the body that encloses a call. A
    * tree whose children agree can still have the links wrong.
    */
  def parentLinks(ast: Ast): Option[String] = links(ast, ast.name)

  private def links(ast: Ast, path: String): Option[String] = ast match
    case _: Lexical => None
    case syn: Syntactic =>
      syn.children.flatten.view.flatMap { child =>
        val at = s"$path/${child.name}"
        if (!child.parent.exists(_ eq syn)) Some(s"broken parent link @ $at")
        else links(child, at)
      }.headOption

  /** normalize `<CR><LF>` and `<CR>` to `<LF>`, as `TRV` and `SV` do */
  private def normalize(str: String): String =
    str.replace("\r\n", "\n").replace('\r', '\n')

  private def diff(
    expect: Ast,
    actual: Ast,
    path: String,
    upToLineTerminators: Boolean,
  ): Option[String] =
    def diff(expect: Ast, actual: Ast, path: String): Option[String] =
      AstDiff.diff(expect, actual, path, upToLineTerminators)
    (expect, actual) match
      case (expect: Lexical, actual: Lexical) =>
        val same =
          if (upToLineTerminators)
            normalize(expect.str) == normalize(actual.str)
          else expect.str == actual.str
        if (expect.name != actual.name)
          Some(s"lexical ${expect.name} != ${actual.name} @ $path")
        else if (!same)
          Some(s"text `${expect.str}` != `${actual.str}` @ $path")
        else None
      case (expect: Syntactic, actual: Syntactic) =>
        if (expect.name != actual.name)
          Some(s"${expect.name} != ${actual.name} @ $path")
        else if (expect.args != actual.args)
          Some(s"${expect.name} args ${expect.args} != ${actual.args} @ $path")
        else if (expect.rhsIdx != actual.rhsIdx)
          Some(
            s"${expect.name}[${expect.rhsIdx}] != [${actual.rhsIdx}] @ $path",
          )
        else if (expect.children.length != actual.children.length)
          Some(
            s"${expect.name} has ${expect.children.length} children" +
            s" != ${actual.children.length} @ $path",
          )
        else
          (expect.children zip actual.children).zipWithIndex.view.flatMap {
            case ((Some(expect), Some(actual)), idx) =>
              diff(expect, actual, s"$path/${expect.name}@$idx")
            case ((None, None), _) => None
            case ((expect, actual), idx) =>
              val shown = (ast: Option[Ast]) => ast.fold("-")(_.name)
              Some(s"child ${shown(expect)} != ${shown(actual)} @ $path@$idx")
          }.headOption
      case _ => Some(s"${kind(expect)} != ${kind(actual)} @ $path")

  private def kind(ast: Ast): String = ast match
    case _: Lexical   => s"lexical ${ast.name}"
    case _: Syntactic => s"syntactic ${ast.name}"
}
