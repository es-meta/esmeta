package esmeta.transpile

import esmeta.cfg.CFG
import esmeta.es.*
import esmeta.es.util.*
import esmeta.spec.Spec
import esmeta.transpile.util.*

object Transpiler:
  def apply(ast: Ast)(using CFG): Ast = {
    println(s"Transpiling ${ast.code}")
    val transpiledAst = TranspileWalker(summon[CFG]).walk(ast)
    val stringified = transpiledAst.toString
    assert(ast =:= ast)
    println("printing " ++ transpiledAst.code)
    for ((a, b) <- (ast >/< transpiledAst).toOption) do
      println(s"After transpileation, difference is:")
      println(s"As Code       : `${a.code}` != `${b.code}`")
      println(s"As Production : ${a.name} != ${b.name}")
    transpiledAst
  }

extension (ast: Ast) {
  def assertSyntactic: Syntactic = ast match
    case s: Syntactic => s
    case _ => throw new Exception(s"Expected Syntactic, but got $ast")

  def toCode(using CFG): String =
    ast.toString(grammar = Some(summon[CFG].grammar))
}

class TranspileWalker(cfg: CFG) extends Walker {
  given CFG = cfg

  // use esParser to reduce "manually" modelled transpilation rules
  lazy val esParser = cfg.esParser

  override def walk(ast: Syntactic): Syntactic = ast match {
    case Syntactic("CoalesceExpression", _, _, Some(e1) +: Some(e2) +: _) =>
      esParser("Expression")
        .from(s"(${e1.code} == null ? undefined : ${e2.code})")
        .assertSyntactic
    // .walkChildren

    case Syntactic("ExpressionStatement", args, rhsIdx, children) =>
      esParser("StatementList")
        .from(s"throw 42; ${ast.code}")
        .assertSyntactic
    // .walkChildren

    case _ => super.walk(ast)
  }

  extension (ast: Syntactic)
    def walkChildren: Syntactic =
      val newChildren = ast.children.map(walkOpt(_, walk))
      ast.copy(children = newChildren)
}

extension (ast: Ast) {
  def code(using CFG): String =
    ast.toString(grammar = Some(summon[CFG].grammar))
}
