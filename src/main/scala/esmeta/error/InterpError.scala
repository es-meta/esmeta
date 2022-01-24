package esmeta.error

import esmeta.interp.*
import esmeta.cfg.*

sealed abstract class InterpError(msg: String)
  extends ESMetaError(s"[Interp Error] $msg")
case class InvalidNodeId(nid: Int) extends InterpError(s"invalid node id: $nid")
case object NoReturnValue extends InterpError(s"no return value")
case class NoBoolean(v: Value) extends InterpError(s"not a boolean: $v")
case class NoInteger(v: Value) extends InterpError(s"not an integer: $v")
case class UnknownId(x: Id) extends InterpError(s"unknown variable: $x")
case class UnknownAddr(addr: Addr)
  extends InterpError(s"unknown address: $addr")
case class UncheckedAbrupt(comp: Comp)
  extends InterpError(s"unchecked abrupt completion: $comp")
case class WrongStringRef(str: String, prop: PureValue)
  extends InterpError(s"wrong access of string reference: $str.$prop")
case class NotSupported(msg: String) extends InterpError(s"[YET] $msg")
case class InvalidObjProp(obj: Obj, prop: PureValue)
  extends InterpError(s"invalid object property: $prop for $obj")
case class InvalidRefBase(v: Value)
  extends InterpError(s"not a proper reference base: $v")
case class OutOfRange(list: ListObj, k: Int)
  extends InterpError(s"out of range: $k of $list")
