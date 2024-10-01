package esmeta.peval.simplifier

import esmeta.ir.*

object InstFlattener:
  def apply(inst: Inst): Inst =
    val insts = (inst match
      case IIf(cond, thenInst, elseInst) =>
        List(IIf(cond, apply(thenInst), apply(elseInst)).passCmt(from = inst))
      case IWhile(cond, body) =>
        List(IWhile(cond, apply(body)).passCmt(from = inst))
      case ISeq(insts) => flat(ISeq(insts.map(apply))).insts
      case INop()      => List.empty[Inst]
      case i           => List(i)
    )
    insts.size match
      case 1 => insts.head
      case _ => ISeq(insts)

  def flat(iseq: ISeq): ISeq = ISeq(iseq.insts.map {
    case ISeq(insts) => insts.map(apply)
    case i           => List(apply(i))
  }.flatten)
