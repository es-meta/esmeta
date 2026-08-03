package esmeta.rocq

import esmeta.ir.{Func, FuncKind, Local, Name, Param, Temp}

/** Names used by generated Rocq source files. */
private[rocq] object RocqNaming {

  /** Replace characters outside Rocq's simple identifier subset. */
  private def encode(name: String): String =
    name.codePoints.toArray.map {
      case cp
          if ('a' <= cp && cp <= 'z') ||
          ('A' <= cp && cp <= 'Z') ||
          ('0' <= cp && cp <= '9') =>
        cp.toChar.toString
      case cp => "_"
    }.mkString

  /** Prefix generated names with the semantic kind retained by the IR. */
  private def kindPrefix(kind: FuncKind): String = kind match {
    case FuncKind.AbsOp        => "AbsOp"
    case FuncKind.NumMeth      => "NumMeth"
    case FuncKind.SynDirOp     => "SynDirOp"
    case FuncKind.ConcMeth     => "ConcMeth"
    case FuncKind.InternalMeth => "InternalMeth"
    case FuncKind.Builtin      => "Builtin"
    case FuncKind.Clo          => "Clo"
    case FuncKind.Cont         => "Cont"
    case FuncKind.Aux          => "Aux"
  }

  private def functionPart(func: Func): String =
    s"${kindPrefix(func.kind)}_${encode(func.name)}"

  def function(func: Func): String = s"ir_${functionPart(func)}"

  def semantic(func: Func): String = s"${function(func)}_sem"

  def signature(func: Func): String = s"sig_${functionPart(func)}"

  /** CRIS call-event key, using the same normalization as Rocq names. */
  def signatureName(func: Func): String = functionPart(func)

  /** Keep IR binders disjoint from Rocq and imported global names. */
  def local(local: Local): String = local match {
    case Name(name)  => s"ir_${encode(name)}"
    case Temp(index) => s"temp_$index"
  }

  /** Parameters and references use the same context-free lexical name. */
  def parameter(param: Param, _index: Int): String = local(param.lhs)

  def module(func: Func): String = s"${functionPart(func)}"
}
