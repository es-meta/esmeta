package esmeta.peval

import esmeta.cfg.{Func => CfgFunc}
import esmeta.error.{ExpiredAllocator}
import esmeta.ir.{Func => IRFunc, *}
import esmeta.peval.domain.*
import esmeta.state.{RecordObj}
import scala.collection.mutable.{Set as MSet}

extension [A, B](a: A) inline def |>(f: A => B): B = f(a)

case class InstPevalResult(insts: List[Inst], guard: Option[PathCondition])
case class ExprPevalResult(result: PValue, unwrap: List[IAssign])
case class RefPevalResult(result: PRefTarget, unwrap: List[IAssign])

object PevalResult:
  def apply(insts: Inst*): InstPevalResult = InstPevalResult(insts.toList, None)
  def apply(pvalue: PValue): ExprPevalResult = ExprPevalResult(pvalue, Nil)
  def apply(preftgt: PRefTarget): RefPevalResult = RefPevalResult(preftgt, Nil)
  val emptyInst = InstPevalResult(Nil, None)

class TempAllocator(private val from: Int):
  def get: Temp =
    i += 1
    Temp(i)

  def addIAssign(inst: IAssign): Unit =
    if (expired) throw ExpiredAllocator()
    set += inst

  def flush: Inst =
    expired = true
    ISeq(set.toList)

  private var i = from
  private var set = MSet.empty[IAssign]
  private var expired = false

type OverloadsIRMap = Map[String, Set[OverloadedIRFunc]]
type OverloadsMap = Map[String, Set[OverloadedFunc]]
