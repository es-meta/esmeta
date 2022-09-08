package esmeta.es.util.mutator

import esmeta.util.BaseUtils
import esmeta.spec.*
import esmeta.es.util.*
import esmeta.es.*
import esmeta.es.util.mutator.RandomMutation.fff

/* A simple random mutation */
case class RandomMutation(grammar: Grammar) extends Mutator with Walker {
  val name = "Random Mutation"
  val synthesizer = RandomSynth(grammar)
  val generator = SimpleAstGenerator(grammar)

  def mutate(ast: Ast): Ast = {
//    println(generator.generate("Statement", fff))
//    println(synthesizer.synthesize("Statement", fff))
    walk(ast)
  }
  override def walk(ast: Syntactic): Syntactic = {
    if (BaseUtils.randBool) {
      val Syntactic(name, args, _, _) = ast
      name match
        case "AssignmentExpression" | "PrimaryExpression" | "Statement" |
            "VariableDeclaration" =>
          synthesizer
            .synthesize(name, args)
            .getOrElse(super.walk(ast))
            .asInstanceOf[Syntactic]
        case _ => super.walk(ast)
    } else super.walk(ast)
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

  val primExpr: Ast = buildExpr(
    List(
      Left("PrimaryExpression", ff, 2),
      Left("Literal", List(), 2),
      Right(Lexical("NumericLiteral", "41")),
    ),
  )

  val assignExpr: Ast = buildExpr(
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

  val bindingId: Ast = buildExpr(
    List(
      Left("Identifier", ff, 0),
      Right(Lexical("IdentifierName \\ (ReservedWord)", "subway")),
    ),
  )

  val initializer: Ast = buildExpr(
    List(
      Left("Initializer", tff, 0),
      Right(assignExpr),
    ),
  )

  val declExpr: Syntactic = Syntactic(
    "VariableDeclaration",
    tff,
    0,
    List(Some(bindingId), Some(initializer)),
  )

  val statement: Ast = buildExpr(
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
      val Syntactic(name, _, args, _) = ast
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
