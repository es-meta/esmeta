package esmeta.es.util.mutator

import esmeta.util.BaseUtils
import esmeta.spec.*
import esmeta.es.util.*
import esmeta.es.*

/* A simple random mutation */
case class RandomMutation(grammar: Grammar) extends Mutator with Walker {
  val name = "Random Mutation"
  val synthesizer: RandomSynth = RandomSynth(grammar)

  def mutate(ast: Ast): Ast = {
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
