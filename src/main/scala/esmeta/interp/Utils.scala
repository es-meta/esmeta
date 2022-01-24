package esmeta.interp

import esmeta.cfg.*
import esmeta.cfg.Utils.*
import esmeta.error.*
import esmeta.util.BaseUtils.*
import esmeta.util.*
import scala.collection.mutable.{Map => MMap}
import scala.math.{BigInt => SBigInt}

/** Interp utilities */
object Utils {

  /** conversion number to string */
  def toStringHelper(m: Double, radix: Int = 10): String = {
    // get sign
    def getSign(n: Int): Char = if (n - 1 > 0) '+' else '-'

    // get string of number
    def getStr(number: Long, radix: Int): String =
      var str = ""
      var sLong = number
      while (sLong > 0) { str += getRadixString(sLong % radix); sLong /= radix }
      str.reverse

    // get radix string of number
    def getRadixString(d: Long): String =
      if (d < 10) d.toString else ('a' + (d - 10)).toChar.toString

    if (m.isNaN) "NaN"
    else if (m == 0) "0"
    else if (m < 0) "-" + toStringHelper(-m, radix)
    else if (m.isPosInfinity) "Infinity"
    else {
      var s = BigDecimal(m)
      var n = 0
      while (s % radix == BigDecimal(0) || s % 1 != BigDecimal(0)) {
        if (s % radix == BigDecimal(0)) { s /= radix; n += 1 }
        else { s *= radix; n -= 1 }
      }
      while (
        (((s - (s % radix)) / radix) * BigDecimal(radix).pow(
          n + 1,
        )).toDouble == m
      ) {
        s = (s - (s % radix)) / radix
        n = n + 1
      }
      var sLong = s.toLong
      var k = 0
      while (s >= BigDecimal(1)) { s /= radix; k += 1 }
      n += k
      if (k <= n && n <= 21) {
        getStr(sLong, radix) + ("0" * (n - k))
      } else if (0 < n && n <= 21) {
        val str = getStr(sLong, radix)
        str.substring(0, n) + '.' + str.substring(n)
      } else if (-6 < n && n <= 0) {
        "0." + ("0" * (-n)) + getStr(sLong, radix)
      } else if (k == 1) {
        getStr(sLong, radix) + "e" + getSign(n) + math.abs(n - 1).toString
      } else {
        val str = getStr(sLong, radix)
        str.substring(0, 1) + '.' + str
          .substring(1) + 'e' + getSign(n) + math.abs(n - 1).toString
      }
    }
  }

  /** extensions for Numeric */
  extension (n: Numeric) {
    def toMath: Math = n match {
      case math: Math     => math
      case Number(double) => Math(double)
      case BigInt(bigint) => Math(BigDecimal(bigint))
    }
  }

  /** extensions for Double */
  extension (l: Double) {
    def %%(r: Double): Double =
      val m = l % r
      if (m * r < 0.0) m + r else m
  }

  /** extensions for scala.math.BigInt */
  extension (l: SBigInt) {
    def %%(r: SBigInt): SBigInt =
      val m = l % r
      if (m * r < 0) m + r else m
  }

  /** extensions for BigDecimal */
  extension (l: BigDecimal) {
    def %%(r: BigDecimal): BigDecimal =
      val m = l % r
      if (m * r < 0) m + r else m
  }

  object NormalComp {
    def apply(value: PureValue): Comp =
      Comp(CONST_NORMAL, value, None)
    def unapply(comp: Comp): Option[PureValue] = comp match {
      case Comp(CONST_NORMAL, value, None) => Some(value)
      case _                               => None
    }
  }

  /** extensions for states */
  extension (st: State) {

    /** get the current function */
    def func: Func = st.cfg.funcOf(st.context.cur)

    /** return id and its value */
    def retId: Id = st.context.retId

    /** get local variable maps */
    def locals: MMap[Local, Value] = st.context.locals

    /** lookup variable directly */
    def directLookup(x: Id): Value = (x match {
      case x: Global => st.globals.get(x)
      case x: Local  => st.context.locals.get(x)
    }).getOrElse(throw UnknownId(x))

    /** getters */
    def apply(refV: RefValue): Value = refV match
      case IdValue(x)            => st(x)
      case PropValue(base, prop) => st(base, prop)
    def apply(x: Id): Value = directLookup(x) match
      case Absent if st.func.isBuiltin => Undef
      case v                           => v
    def apply(base: Value, prop: PureValue): Value = base match
      case comp: Comp =>
        prop match
          case Str("Type")   => comp.ty
          case Str("Value")  => comp.value
          case Str("Target") => comp.targetValue
          case _             => st(comp.escaped, prop)
      case addr: Addr => st.heap(addr, prop)
      case Str(str)   => st(str, prop)
      case v          => throw InvalidRefBase(v)
    def apply(str: String, prop: PureValue): PureValue = prop match
      case Str("length") => Math(str.length)
      case Math(k)       => Str(str(k.toInt).toString)
      case Number(k)     => Str(str(k.toInt).toString)
      case _             => throw WrongStringRef(str, prop)
    def apply(addr: Addr): Obj = st.heap(addr)
  }

  /** extensions for heaps */
  extension (heap: Heap) {

    /** getters */
    def apply(addr: Addr): Obj =
      heap.map.getOrElse(addr, throw UnknownAddr(addr))
    def apply(addr: Addr, key: PureValue): Value = heap(addr) match
      case (s: SymbolObj) => s(key)
      // TODO case (MapObj(ALGORITHM, _, _))  => getAlgorithm(key)
      // TODO case (MapObj(INTRINSICS, _, _)) => getIntrinsics(key)
      case (m: MapObj)    => m(key)
      case (l: ListObj)   => l(key)
      case YetObj(_, msg) => throw NotSupported(msg)
  }

  /** extensions for objects */
  extension (obj: Obj) {
    // getters
    def apply(prop: PureValue): Value = (obj, prop) match
      case (SymbolObj(desc), Str("Description")) => desc
      case (MapObj(_, props, _), prop) =>
        props.get(prop).fold[Value](Absent)(_.value)
      case (ListObj(values), Math(decimal)) =>
        val idx = decimal.toInt
        if (0 <= idx && idx < values.length) values(idx)
        else Absent
      case (ListObj(values), Str("length")) => Math(values.length)
      case _                                => throw InvalidObjProp(obj, prop)
  }

  /** extensions for list objects */
  extension (list: ListObj) {

    // appends
    def append(value: PureValue): list.type = { list.values :+= value; list }

    // prepends
    def prepend(value: PureValue): list.type = { list.values +:= value; list }

    // pops
    def pop(idx: PureValue): PureValue = idx match {
      case Math(decimal) =>
        val vs = list.values
        val k = decimal.toInt
        if (k < 0 || k >= vs.length) throw OutOfRange(list, k)
        val v = vs(k)
        list.values = vs.slice(0, k) ++ vs.slice(k + 1, vs.length)
        v
      case v => throw NoInteger(v)
    }
  }

  /** extensions for values */
  extension (v: Value) {

    /** escape completion */
    def escaped: PureValue = v match
      case NormalComp(value) => value
      case comp: Comp        => throw UncheckedAbrupt(comp)
      case pure: PureValue   => pure

    /** check abrupt completion */
    def isCompletion: Boolean = v match
      case comp: Comp => true
      case _          => false

    /** check abrupt completion */
    def isAbruptCompletion: Boolean = v match
      case comp: Comp => comp.ty != CONST_NORMAL
      case _          => false

    /** wrap completion */
    def wrapCompletion: Comp = wrapCompletion(CONST_NORMAL)
    def wrapCompletion(ty: Const): Comp = v match
      case comp: Comp      => comp
      case pure: PureValue => Comp(ty, pure, None)
  }

  /** extensions for completion values */
  extension (comp: Comp) {
    def targetValue: PureValue =
      comp.target.fold[PureValue](CONST_EMPTY)(Str.apply)
  }
}
