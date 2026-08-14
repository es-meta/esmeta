package esmeta.es.util.polyfill.analysis

import esmeta.es.util.polyfill.*
import esmeta.es.util.polyfill.CompletionType.*
import esmeta.lang.*
import esmeta.lang.util.Walker as LangWalker
import esmeta.util.BaseUtils.*

/** erase completion records from expressions, by unwrapping `[[Value]]`
  * accesses and splitting completion arguments into a flag and a value
  */
class ValueAccessUnwrapper(env: TypeEnv) extends LangWalker {

  override def walk(step: Step): Step = step match {
    case WrappedTryCatchStep(tryBlock, catchVar, catchBlock) =>
      WrappedTryCatchStep(
        walk(tryBlock),
        walk(catchVar),
        walkOpt(catchBlock, walk),
      )
    case TaggedStep(innerStep, tag) =>
      TaggedStep(walk(innerStep), tag)
    case _ => super.walk(step)
  }

  override def walk(expr: Expression): Expression = expr match {
    // Unwrap .[[Value]] access on known completion types
    case ReferenceExpression(
          Access(Variable(varName, _, _, _), "Value", _, _),
        ) =>
      env(varName) match {
        case NotCompletion => super.walk(expr)
        case _ =>
          ReferenceExpression(Variable(varName, Some("value_unwrapped")))
      }
    // Unwrap Completion AO calls
    case completionAO @ InvokeAbstractOperationExpression(name, args, _)
        if name.contains("Completion") =>
      if (args.length > 1)
        raise(
          s"Completion AO Call should contain up to one argument:\n\t$completionAO",
        )
      args.head
    // AO calls with completion argument unpacking
    case aoExpr @ InvokeAbstractOperationExpression(name, args, _) =>
      val newArgs = args.flatMap {
        case x @ ReferenceExpression(v @ Variable(targetVar, nt, _, _))
            if nt.isEmpty =>
          env(targetVar) match {
            case MayAbrupt | MayNormal | MayCompletion =>
              // Plug in x_flag directly — no numeric conversion
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