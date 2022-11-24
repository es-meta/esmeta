package esmeta.analyzer

import esmeta.state.CONST_EMPTY
import esmeta.ir.{Func => _, *}
import esmeta.ty.*

trait Optimized { this: AbsTransfer =>

  /** loading monads */
  import AbsState.monad.*

  object OptimizedCall {
    def unapply(callInst: CallInst)(using
      cp: NodePoint[_],
    ): Option[Result[AbsValue]] = callInst match
      case ICall(_, EClo("Completion", Nil), List(expr)) =>
        Some(for {
          v <- transfer(expr)
          _ <- modify(prune(ETypeCheck(expr, EStr("CompletionRecord")), true))
        } yield v)
      case ICall(_, EClo("UpdateEmpty", Nil), List(compExpr, valueExpr)) =>
        AbsValue match
          case domain.value.BasicDomain =>
            Some(for {
              compValue <- transfer(compExpr)
              newValue <- transfer(valueExpr)
            } yield {
              val AbsComp(map) = compValue.comp
              val empty = AbsPureValue(CONST_EMPTY)
              val newMap = map.map {
                case (ty, res @ AbsComp.Result(value, target)) =>
                  ty -> (
                    if (empty !⊑ value) res
                    else res.copy(value = (value -- empty) ⊔ newValue.pureValue)
                  )
              }
              AbsValue(AbsComp(newMap))
            })
          case domain.value.TypeDomain =>
            Some(for {
              compValue <- transfer(compExpr)
              newValue <- transfer(valueExpr)
            } yield {
              val compTy = compValue.ty.comp
              val normalTy = compTy.normal
              val newValueTy = newValue.ty.pureValue
              val emptyTy = ConstT("empty").pureValue
              val updated =
                compTy.copy(normal = (normalTy -- emptyTy) ⊔ newValueTy)
              AbsValue(ValueTy(comp = updated))
            })
          case _ => None
      case _ => None
  }
}
