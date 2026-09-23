package esmeta.es.util.polyfill.completion

import esmeta.es.util.polyfill.*
import esmeta.es.util.polyfill.completion.CompletionType.*
import esmeta.es.util.polyfill.util.Walker as PolyfillWalker
import esmeta.lang.*
import esmeta.lang.PredicateConditionOperator.*
import esmeta.util.BaseUtils.*

/** erase completion records from expressions, by unwrapping `[[Value]]`
  * accesses and splitting completion arguments into a flag and a value
  *
  * The traversal of the polyfill step language is inherited; only the
  * expression case carries the rewriting.
  */
class ValueAccessUnwrapper(env: TypeEnv) extends PolyfillWalker {

  /** the reified counterpart of the abstract-operation case below
    *
    * [[PolyfillExpr.lift]] turns every abstract-operation call into
    * [[PolyfillExpr.Invoke]], so a call at the top of a step never reaches the
    * metalanguage walker and would otherwise keep its completion arguments
    * unsplit.
    */
  override def walk(expr: PolyfillExpr): PolyfillExpr = expr match {
    case completionAO @ PolyfillExpr.Invoke(name, args, _)
        if name.contains("Completion") =>
      if (args.length > 1)
        raise(
          s"Completion AO Call should contain up to one argument:\n\t$completionAO",
        )
      args.head
    case PolyfillExpr.Invoke(name, args, tag) =>
      PolyfillExpr.Invoke(name, args.flatMap(splitCompletionArg), tag)
    case _ => super.walk(expr)
  }

  /** split an argument that names a completion into its flag and its value */
  private def splitCompletionArg(arg: PolyfillExpr): List[PolyfillExpr] =
    arg match {
      case PolyfillExpr.LangExpr(
            x @ ReferenceExpression(v @ Variable(targetVar, nt, _, _)),
          ) if nt.isEmpty =>
        env(targetVar) match {
          case MayAbrupt | MayNormal | MayCompletion =>
            List(
              PolyfillExpr.LangExpr(
                ReferenceExpression(Variable(s"${targetVar}_flag", None)),
              ),
              PolyfillExpr.LangExpr(x.copy(v.copy(nt = Some("comp_split")))),
            )
          case _ => List(walk(arg))
        }
      case PolyfillExpr.Invoke(innerCallName, innerArgs, _)
          if innerCallName.contains("Completion") =>
        innerCallName match {
          case "NormalCompletion" =>
            List(PolyfillExpr.LangExpr(EnumLiteral("normal"))) ++ innerArgs
          case "ThrowCompletion" | "AbruptCompletion" =>
            List(PolyfillExpr.LangExpr(EnumLiteral("abrupt"))) ++ innerArgs
          case "Completion" =>
            raise(s"Cannot unpack the raw completion object: $arg")
          case _ => List(walk(arg))
        }
      case x => List(walk(x))
    }

  override def walk(expr: Expression): Expression = expr match {
    case ReferenceExpression(
          Access(Variable(varName, _, _, _), "Value", _, _),
        ) =>
      env(varName) match {
        case NotCompletion => super.walk(expr)
        case _ =>
          ReferenceExpression(Variable(varName, Some("value_unwrapped")))
      }
    case completionAO @ InvokeAbstractOperationExpression(name, args, _)
        if name.contains("Completion") =>
      if (args.length > 1)
        raise(
          s"Completion AO Call should contain up to one argument:\n\t$completionAO",
        )
      args.head
    case aoExpr @ InvokeAbstractOperationExpression(name, args, _) =>
      val newArgs = args.flatMap {
        case x @ ReferenceExpression(v @ Variable(targetVar, nt, _, _))
            if nt.isEmpty =>
          env(targetVar) match {
            case MayAbrupt | MayNormal | MayCompletion =>
              List(
                ReferenceExpression(Variable(s"${targetVar}_flag", None)),
                x.copy(v.copy(nt = Some("comp_split"))),
              )
            case _ => Some(x)
          }
        case c @ InvokeAbstractOperationExpression(
              innerCallName,
              innerArgs,
              _,
            ) if innerCallName.contains("Completion") =>
          if (innerArgs.length > 1)
            raise(
              s"Completion AO Call should contain up to one argument:\n\t$c",
            )
          innerCallName match {
            case "NormalCompletion" =>
              List(EnumLiteral("normal"), innerArgs.head)
            case "ThrowCompletion" | "AbruptCompletion" =>
              List(EnumLiteral("abrupt"), innerArgs.head)
            case "Completion" =>
              raise(
                s"Cannot unpack the raw completion object: $c",
              )
            case _ => Some(c.copy(args = innerArgs.map(walk)))
          }
        case x => Some(walk(x))
      }
      aoExpr.copy(args = newArgs)
    case _ => super.walk(expr)
  }
}

/** an `if` step checking the completion type of a variable, e.g. `if x is an
  * abrupt completion, ...`
  *
  * It extracts the checked completion type and the name of the checked
  * variable.
  */
object CompletionCheckPattern {
  def unapply(cond: Condition): Option[(CompletionType, String)] =
    traverseCondition(cond)

  private def traverseCondition(
    cond: Condition,
  ): Option[(CompletionType, String)] =
    cond match {
      case PredicateCondition(expr, _, op) =>
        // TODO: Proper decision for negated case
        op match {
          // Abrupt, Throw, Normal are not pluralOp
          case Abrupt | Throw => Some((MayAbrupt, extractVarName(expr.head)))
          case Normal         => Some((MayNormal, extractVarName(expr.head)))
          case _              => None
        }
      case CompoundCondition(left, op, right) =>
        traverseCondition(left).orElse(traverseCondition(right))
      case _ => None
    }

  private def extractVarName(expr: Expression) = expr match {
    case ReferenceExpression(Variable(x, _, _, _)) => x
    case err =>
      raise(
        s"Expected Reference Expression for extractVarName, but got '${err.toString}'",
      )
  }
}
