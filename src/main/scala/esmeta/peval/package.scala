package esmeta.peval

import esmeta.cfg.{Func => CfgFunc}
import esmeta.error.{DynamicValue, ExpiredAllocator}
import esmeta.ir.{Func => IRFunc, *}
import esmeta.peval.domain.*
import esmeta.state.{RecordObj}
import scala.collection.mutable.{Set as MSet}
import scala.util.Try
import scala.util.Failure
import scala.util.Success

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

sealed trait Expect[+A]
case class Static[+A](a: A) extends Expect[A]
case object Dynamic extends Expect[Nothing]

def TryStatic[A](value: => A): Expect[A] = Try(value) match
  case Success(value)                   => Static(value)
  case Failure(exception: DynamicValue) => Dynamic
  case Failure(e)                       => throw e

extension [A](a: Expect[A])
  def unpack: A = a match
    case Static(value) => value
    case Dynamic       => throw DynamicValue()
