package esmeta.compiler

import esmeta.ir.{Type => IRType, Param => IRParam, *}
import esmeta.lang.*
import esmeta.peval.{SpecializedFuncs}
import esmeta.spec.*
import scala.collection.mutable.{ListBuffer, Stack}

/** function builder */
case class FuncBuilder(
  spec: Spec,
  kind: FuncKind,
  name: String,
  params: List[IRParam],
  retTy: IRType,
  algo: Algorithm,
  returnContext: Option[Ref],
  needReturnComp: Boolean,
) {
  import FuncKind.*

  /** get an IR function as the result of compilation of an algorithm */
  def getFunc(body: => Inst): Func = Func(
    name == "RunJobs",
    kind,
    name,
    params,
    retTy,
    body,
    SpecializedFuncs.EMPTY,
    Some(algo),
  )

  /** check whether it is builtin */
  lazy val isBuiltin: Boolean = kind == Builtin

  /** bindings for nonterminals */
  var ntBindings: List[(String, Expr, Option[Int])] = algo.head match
    case SyntaxDirectedOperationHead(Some(target), _, _, _, _) =>
      val rhs = grammar.nameMap(target.lhsName).rhsList(target.idx)
      val rhsNames = rhs.nts.map(_.name)
      val rhsBindings = rhsNames.zipWithIndex.map {
        case (name, idx) => (name, ENAME_THIS, Some(idx))
      }
      if (rhsNames contains target.lhsName) rhsBindings
      else (target.lhsName, ENAME_THIS, None) :: rhsBindings
    case _ => List()

  /** bindings for built-in function parameters */
  var builtinBindings: Set[String] = Set()

  /** create a new scope with a given procedure */
  def newScope(f: => Unit): Inst =
    scopes.push(ListBuffer()); f; ISeq(scopes.pop.toList)

  /** set backward edge from ir to lang */
  def withLang(lang: Syntax)(f: => Unit): Unit =
    langs.push(lang); val result = f; langs.pop
    result
  def withLang[T <: IRElem](lang: Syntax)(f: => T): T =
    langs.push(lang); val result = backEdgeWalker(f); langs.pop
    result

  /** add instructions to the current scope */
  def addInst(insts: Inst*): Unit = scopes.head ++= insts
    .flatMap {
      case ISeq(is) => is
      case i        => List(i)
    }
    .map(backEdgeWalker.apply)

  /** add return to resume instruction */
  def addReturnToResume(context: Ref, value: Expr): Unit =
    val (x, xExpr) = newTIdWithExpr
    addInst(
      IPop(x, toStrERef(context, "ReturnCont"), true),
      ICall(newTId, xExpr, List(value)),
    )

  /** get next temporal variable */
  def newTId: Temp = Temp(nextTId)

  /** get next temporal variable with expressions */
  def newTIdWithExpr: (Temp, Expr) = { val x = newTId; (x, ERef(x)) }

  /** get next temporal variable with expressions after expression assignment */
  def newTIdWithExpr(expr: Expr): (Temp, Expr) =
    val (x, xExpr) = newTIdWithExpr
    addInst(IAssign(x, expr))
    (x, xExpr)

  /** get closure name */
  def nextCloName: String = s"$name:clo${nextCId}"

  /** get continuation name */
  def nextContName: String = s"$name:cont${nextCId}"

  /** grammar */
  private def grammar: Grammar = spec.grammar

  /** scope stacks */
  private var scopes: Stack[ListBuffer[Inst]] = Stack()

  /** lang stacks */
  val langs: Stack[Syntax] = Stack()

  lazy val backEdgeWalker: BackEdgeWalker = BackEdgeWalker(this)

  lazy val returnModifier: ReturnModifier = ReturnModifier(this)

  // ---------------------------------------------------------------------------
  // Private Helpers
  // ---------------------------------------------------------------------------
  // temporal variable index counter
  private def nextTId: Int = { val tid = tidCount; tidCount += 1; tid }
  private var tidCount: Int = 0

  // closure id counter
  private def nextCId: Int = { val cid = cidCount; cidCount += 1; cid }
  private var cidCount: Int = 0
}
