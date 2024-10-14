package esmeta.es.util.fuzzer

import scala.util.*
import esmeta.cfg.CFG
import esmeta.es.Ast
import esmeta.parser.ESParser

class MinifyChecker(
  cfg: CFG,
  minify: String => Option[String], // minify function
  config: MinifyCheckerConfig = MinifyCheckerConfig(),
) {
  /*
   * Check if the given code is minified.
   */
  def check(code: String): Option[MinifyCheckResult] = minify(code).map {
    minified =>
      val originalAst = ESParser(cfg.grammar)("Script").from(code)
      val minifiedAst = ESParser(cfg.grammar)("Script").from(minified)
      val diff = checkAstDiff(originalAst, minifiedAst)
      MinifyCheckResult(
        diff = diff,
      )
  }

  /*
   * Check the structural difference between the given ASTs and return every
   * different part of the ASTs, if any. The returned list should have the
   * changed parts of the original AST.
   *
   * Directly checking whether the string value of the code is the same as the
   * minified code is not enough because the minified code can have different
   * source code string while preserving the same Ast structure. So, we need to
   * check the structural equivalence ASTs of the code and the minified code.
   *
   * It would be appropriate to ignore the differences in the name of the variables.
   * (or it would be perfect if we first rename all the variables of ast1 and ast2 and
   * then compare them)
   */
  def checkAstDiff(ast1: Ast, ast2: Ast): List[Ast] =
    checkAstDiffSuppl(ast1, ast2, Nil)

  /* Recursive helper function for checkAstDiff */
  def checkAstDiffSuppl(ast1: Ast, ast2: Ast, acc: List[Ast]): List[Ast] = ???
}

case class MinifyCheckerConfig(
  // TODO: add configurations if needed
)

case class MinifyCheckResult(
  // influentialOptions: List[String], // TODO: check options that affect minification (not right now)
  diff: List[Ast],
)
