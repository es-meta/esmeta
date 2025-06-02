package esmeta.transpile

import esmeta.cfg.CFG
import esmeta.es.*
import esmeta.es.util.*
import esmeta.spec.Spec
import esmeta.transpile.util.*

object Transpiler:
  def apply(ast: Ast)(using CFG): Ast = {
    val transpiledAst = TranspileWalker(summon[CFG]).walk(ast)
    val stringified = transpiledAst.toString
    assert(ast =:= ast)
    println("printing " ++ transpiledAst.toCode)
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
    case Syntactic("ExpressionStatement", args, rhsIdx, children) =>
      esParser("StatementList")
        .from(s"throw 42; ${ast.toCode}")
        .assertSyntactic

    case _ => super.walk(ast)
  }
}
