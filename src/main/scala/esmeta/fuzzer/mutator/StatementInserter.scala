package esmeta.fuzzer.mutator

import esmeta.es.*
import esmeta.es.util.{Walker => AstWalker}
import esmeta.es.util.*
import esmeta.es.util.Coverage.*
import esmeta.spec.Grammar
import esmeta.fuzzer.synthesizer.*
import esmeta.util.BaseUtils.*
import esmeta.cfg.CFG

/** A mutator that inserts statements to ECMAScript AST */
class StatementInserter(using cfg: CFG)(
  val synBuilder: Synthesizer.Builder = RandomSynthesizer,
) extends Mutator
  with Util.MultiplicativeListWalker {
  import StatementInserter.*
  import Mutator.*

  val randomMutator = RandomMutator()

  val names = "StatementInserter" :: randomMutator.names

  val synthesizer = synBuilder(cfg.grammar)

  /** default weight for StatementInserter is 1 */
  def calculateWeight(ast: Ast): Int = 1

  /** mutate a program */
  def apply(
    ast: Ast,
    n: Int,
    _target: Option[(CondView, Coverage)],
  ): Seq[Result] = {
    // count the number of stmtLists
    val k = stmtListCounter(ast)

    if (k == 0) randomMutator(ast, n, _target)
    else if (n == 1)
      // Insert one statement with 80% probability
      k1 = k - 1
      c1 = 1
      k2 = 1
      c2 = 5

      sample(ast, n)
    else {
      // calculate the most efficient parameters
      val (kc1, kc2) = calcParam(n, k)
      k1 = kc1._1; c1 = kc1._2
      k2 = kc2._1; c2 = kc2._2
      sample(ast, n)
    }
  }

  /** parameter for sampler */
  private var (c1, c2, k1, k2) = (0, 0, 0, 0)

  private def sample(ast: Ast, n: Int): Seq[Result] =
    shuffle(walk(ast)).take(n).map(Result(name, _))

  private def decideGenNum =
    if k1 > 0 && randBool(k1 / (k1 + k2 + 0.0)) then
      k1 -= 1; c1
    else if k2 > 0 then
      k2 -= 1; c2
    else throw new Error("This is a bug in Stmt Inserter")

  /** generate a new statement list item, either randomly or manually */
  private def newStmtItem(args: List[Boolean]) = choose(
    synthesizer(STATEMENT_LIST_ITEM, args),
    choose(manualStmtItems(args)),
  )

  /** lift a single statementListItem to statementList */
  private def item2list(item: Syntactic) =
    Syntactic(STATEMENT_LIST, item.args, 0, Vector(Some(item)))

  /** ast walker */
  override def walk(ast: Syntactic): List[Syntactic] = ast match
    // singleton statement list
    case Syntactic(STATEMENT_LIST, args, 0, _) =>
      val genNum = decideGenNum
      val mutants = super.walk(ast)
      List
        .tabulate(genNum)(_ match
          case 0 => // do Nothing
            mutants
          case _ => // append a stmt either front or behind
            val newStmt = newStmtItem(args)
            mutants.map(mutant =>
              Syntactic(
                STATEMENT_LIST,
                args,
                1,
                if randBool then Vector(Some(mutant), Some(newStmt))
                else Vector(Some(item2list(newStmt)), mutant.children(0)),
              ),
            ),
        )
        .flatten

    // long statement list
    case Syntactic(STATEMENT_LIST, args, 1, _) =>
      val genNum = decideGenNum
      val mutants = super.walk(ast)
      List
        .tabulate(genNum)(_ match
          case 0 => // do Nothing
            mutants
          case _ => // append a stmt behind
            val newStmt = newStmtItem(args)
            mutants.map(mutant =>
              Syntactic(
                STATEMENT_LIST,
                args,
                1,
                Vector(Some(mutant), Some(newStmt)),
              ),
            ),
        )
        .flatten

    // ast who has an empty statement list as a child
    case _ if containsEmptyStatementList(ast) =>
      val Syntactic(name, args, rhsIdx, children) = ast
      val container = STATEMENT_LIST_OPTIONAL_CONTAINERS.find(_._1 == name).get

      // get args for new stmt to be added
      val rhsArgModifier = container._4.toList
      val newArgs = rhsArgModifier.zipWithIndex.map {
        case (-1, _) => false
        case (0, i)  => optional(args(i)).getOrElse(false)
        case (1, _)  => true
        case _       => false
      }

      // generate new stmts
      val genNum = decideGenNum
      val newStmts = List.tabulate(genNum)(_ match
        case 0 => None
        case _ => Some(item2list(newStmtItem(newArgs))),
      )

      // change children
      val childIdx = container._3
      val newChildrens =
        children.zipWithIndex.foldRight(List(Vector[Option[Ast]]())) {
          case ((child, i), childrens) => {
            for {
              child <- if (i == childIdx) newStmts else walkOpt(child)
              children <- childrens
            } yield (child +: children)
          }
        }
      newChildrens.map(newChildren =>
        Syntactic(name, args, rhsIdx, newChildren),
      )

    case _ => super.walk(ast)

  // TODO: generalize to case where length of args is not 3
  lazy val manualStmtItems: Map[List[Boolean], List[Syntactic]] = (
    for (
      a1 <- List(true, false);
      a2 <- List(true, false);
      a3 <- List(true, false);
      args = List(a1, a2, a3)
    )
      yield (
        args,
        manualStmts.flatMap(code =>
          optional(
            esParser(STATEMENT_LIST_ITEM, args)
              .from(code)
              .asInstanceOf[Syntactic],
          ),
        ),
      )
  ).toMap
}

object StatementInserter {
  val STATEMENT_LIST = "StatementList"
  val STATEMENT_LIST_ITEM = "StatementListItem"
  val STATEMENT_LIST_OPTIONAL_CONTAINERS = List(
    // (name, rhsIdx, childIdx of optional statementList, rhs arg modifier)
    // TODO: automatically generate this list
    ("Block", 0, 0, (0, 0, 0)),
    ("CaseClause", 0, 1, (0, 0, 0)),
    ("DefaultClause", 0, 0, (0, 0, 0)),
    ("FunctionStatementList", 0, 0, (0, 0, 1)),
    ("ClassStaticBlockStatementList", 0, 0, (-1, 1, -1)),
  )

  def containsEmptyStatementList(ast: Ast) = ast match {
    case Syntactic(name, args, rhsIdx, children) =>
      STATEMENT_LIST_OPTIONAL_CONTAINERS.exists {
        case (cName, cRhsIdx, cChildIdx, _) =>
          cName == name &&
          cRhsIdx == rhsIdx &&
          children(cChildIdx) == None
      }
    case _ => false
  }

  val manualStmts = List(
    "x ( ) ; ",
    "x ( 0 ) ; ",
    "return ; ",
    "return 0 ; ",
    "throw 0 ; ",
    "yield 0 ; ",
    "await x ( ) ; ",
    "yield * x ( ) ; ",
    "new x ( ) ; ",
    "break ; ",
    "break x ; ",
    "continue ; ",
    "continue x ; ",
  )

  // count the number of places where stmt can be inserted
  val stmtListCounter = Util.AstCounter(ast => {
    ast.name == STATEMENT_LIST || containsEmptyStatementList(ast)
  })
}
