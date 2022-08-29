package esmeta.es.util.mutator

import esmeta.util.BaseUtils
import esmeta.es.util.*
import esmeta.es.*

/* A simple random mutation */
case class RandomMutation(ast: Ast) extends Mutator with Walker {
  val name = "Random Mutation"

  def mutate = {
    RandomMutation.walk(ast)
  }
}

object RandomMutation extends Walker {
  // def apply(ast: Ast): Ast = {print("statement"); walk(ast)}

  def buildExpr(
    exprInfo: List[Either[(String, List[Boolean], Int), Ast]],
  ): Ast = exprInfo match
    case Left((name, args, rhsIdx)) :: tail =>
      Syntactic(name, args, rhsIdx, List(Some(buildExpr(tail))))
    case Right(ast) :: Nil => ast
    case _                 => Lexical("StringLiteral", "error")

  private val tff = List(true, false, false)
  private val fff = List(false, false, false)
  private val ff = List(false, false)

  val primExpr = buildExpr(
    List(
      Left("PrimaryExpression", ff, 2),
      Left("Literal", List(), 2),
      Right(Lexical("NumericLiteral", "41")),
    ),
  )

  val assignExpr = buildExpr(
    List(
      Left("AssignmentExpression", tff, 0),
      Left("ConditionalExpression", tff, 0),
      Left("ShortCircuitExpression", tff, 0),
      Left("LogicalORExpression", tff, 0),
      Left("LogicalANDExpression", tff, 0),
      Left("BitwiseORExpression", tff, 0),
      Left("BitwiseXORExpression", tff, 0),
      Left("BitwiseANDExpression", tff, 0),
      Left("EqualityExpression", tff, 0),
      Left("RelationalExpression", tff, 0),
      Left("ShiftExpression", ff, 0),
      Left("AdditiveExpression", ff, 0),
      Left("MultiplicativeExpression", ff, 0),
      Left("ExponentiationExpression", ff, 0),
      Left("UnaryExpression", ff, 0),
      Left("UpdateExpression", ff, 0),
      Left("LeftHandSideExpression", ff, 0),
      Left("NewExpression", ff, 0),
      Left("MemberExpression", ff, 0),
      Right(primExpr),
    ),
  )

  val bindingId = buildExpr(
    List(
      Left("Identifier", ff, 0),
      Right(Lexical("IdentifierName \\ (ReservedWord)", "subway")),
    ),
  )

  val initializer = buildExpr(
    List(
      Left("Initializer", tff, 0),
      Right(assignExpr),
    ),
  )

  val declExpr = Syntactic(
    "VariableDeclaration",
    tff,
    0,
    List(Some(bindingId), Some(initializer)),
  )

  val statement = buildExpr(
    List(
      Left("Statement", fff, 1),
      Left("VariableStatement", ff, 0),
      Right(declExpr),
    ),
  )

  val exprList = List(assignExpr)
  val primExprList = List(primExpr)
  val statementList = List(statement)
  val declarationList = List(declExpr)

  override def walk(ast: Syntactic): Syntactic = {
    if (BaseUtils.randBool) {
      val Syntactic(name, _, _, _) = ast
      name match
        case "AssignmentExpression" =>
          BaseUtils.choose(exprList).asInstanceOf[Syntactic]
        case "PrimaryExpression" =>
          BaseUtils.choose(primExprList).asInstanceOf[Syntactic]
        case "Statement" =>
          BaseUtils.choose(statementList).asInstanceOf[Syntactic]
        case "VariableDeclaration" =>
          BaseUtils.choose(declarationList).asInstanceOf[Syntactic]
        case _ => super.walk(ast)
    } else super.walk(ast)
  }
}
