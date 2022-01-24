package esmeta.ir

import esmeta.error.NotSupported
import esmeta.util.BaseUtils.*
import scala.collection.mutable.{Map => MMap}

/** IR Utils */
object Utils {
  // -----------------------------------------------------------------------------
  // Helpers
  // -----------------------------------------------------------------------------

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

  /** modulo operation */
  def modulo(l: Double, r: Double): Double = {
    l % r
  }

  /** unsigned modulo operation */
  def unsigned_modulo(l: Double, r: Double): Double = {
    val m = l % r
    if (m * r < 0.0) m + r
    else m
  }

  /** modulo operation for bigints */
  def modulo(l: BigInt, r: BigInt): BigInt = {
    l % r
  }

  /** unsigned modulo operation for bigints */
  def unsigned_modulo(l: BigInt, r: BigInt): BigInt = {
    val m = l % r
    if (m * r < 0) m + r
    else m
  }

  /** get proper number value */
  def number(x: BigDecimal): Value = {
    if (x.toLong == x) INum(x.toLong)
    else Num(x.toDouble)
  }

  // -----------------------------------------------------------------------------
  // States
  // -----------------------------------------------------------------------------
  /** extension for state */
  extension (st: State) {

    /** move the cursor */
    def moveTo(program: Program): State = moveTo(program.insts)
    def moveTo(insts: List[Inst]): State = moveTo(ISeq(insts))
    def moveTo(inst: Inst): State =
      st.context.cursorOpt = st.cursorGen(inst)
      st

    /** move to the next cursor */
    def moveNext: Unit = st.context.moveNext

    /** get next step target */
    def nextTarget: Option[Cursor] = st.context.cursorOpt match
      case Some(cursor) => Some(cursor)
      case None =>
        st.ctxtStack match
          case Nil => None
          case _   => error(s"need explicit return")

    /** get local variable maps */
    def locals: MMap[Id, Value] = st.context.locals

    /** lookup variable directly */
    def directLookup(x: Id): Value =
      locals.getOrElse(
        x,
        st.globals.getOrElse(x, error(s"unknown variable: ${x.name}")),
      )

    /** getters */
    def apply(refV: RefValue): Value =
      refV match
        case RefValueId(x)            => st(x)
        case RefValueProp(base, prop) => st(base, prop)
    def apply(x: Id): Value =
      directLookup(x) match
        case Absent if st.context.isBuiltin => Undef
        case v                              => v
    def apply(base: Value, prop: PureValue): Value = base match
      case comp: CompValue =>
        prop match
          case Str("Type")   => comp.ty
          case Str("Value")  => comp.value
          case Str("Target") => comp.target
          case _             => st(comp.escaped, prop)
      case addr: Addr => st.heap(addr, prop)
      case Str(str)   => st(str, prop)
      case v          => error(s"not a proper reference base: $v")
    def apply(str: String, prop: Value): Value = prop match
      case Str("length") => INum(str.length)
      case INum(k)       => Str(k.toInt.toString)
      case Num(k)        => Str(k.toInt.toString)
      case v => error(s"wrong access of string reference: $str.$prop")
    def apply(addr: Addr): Obj = st.heap(addr)

    /** setters */
    def update(refV: RefValue, value: Value): Unit = refV match
      case RefValueId(x) => update(x, value)
      case RefValueProp(base, prop) =>
        base.escaped match
          case addr: Addr => update(addr, prop, value)
          case _          => error(s"illegal reference update: $refV = $value")
    def update(x: Id, value: Value): Unit =
      if locals contains x then locals += x -> value
      else if st.globals contains x then st.globals += x -> value
      else error(s"illegal variable update: $x = $value")
    def update(addr: Addr, prop: PureValue, value: Value): Unit =
      st.heap.update(addr, prop, value)

    /** existence checks */
    def exists(id: Id): Boolean =
      val defined = st.globals.contains(id) || locals.contains(id)
      defined && directLookup(id) != Absent
    def exists(ref: RefValue): Boolean = ref match
      case RefValueId(id)           => exists(id)
      case RefValueProp(base, prop) => st(base.escaped, prop) != Absent

    /** delete a property from a map */
    def delete(refV: RefValue): Unit = refV match
      case RefValueId(x) => error(s"cannot delete variable $x")
      case RefValueProp(base, prop) =>
        base.escaped match
          case addr: Addr => st.heap.delete(addr, prop)
          case _          => error(s"illegal reference delete: delete $refV")

    /** get string for a given address */
    def getString(value: Value): String = value match
      case comp: CompValue =>
        comp.toString + (comp.value match {
          case addr: Addr => " -> " + st.heap(addr).toString
          case _          => ""
        })
      case addr: Addr => addr.toString + " -> " + st.heap(addr).toString
      case _          => value.toString

    /** Return helper */
    def doReturn(value: Value): Unit =
      st.ctxtStack match
        case Nil =>
          // TODO save final result to somewhere
          st.context.cursorOpt = None
        case ctxt :: rest =>
          // TODO need type modeling
          // return wrapped values
          ctxt.locals += st.context.retId -> value.wrapCompletion
          st.context = ctxt
          st.ctxtStack = rest

    /** copied */
    def copied: State =
      State(
        st.cursorGen,
        st.context.copied,
        st.ctxtStack.map(_.copied),
        MMap.from(st.globals),
        st.heap.copied,
      )
  }

  // -----------------------------------------------------------------------------
  // Context
  // -----------------------------------------------------------------------------
  /** extension for context */
  extension (ctxt: Context) {
    // TODO handle absent value in builtin function
    def isBuiltin: Boolean = ???

    /** copied */
    def copied: Context = ctxt.copy(locals = MMap.from(ctxt.locals))

    /** move cursor */
    def moveNext: Unit =
      ctxt.cursorOpt = ctxt.cursorOpt.flatMap(_.next)
  }

  // -----------------------------------------------------------------------------
  // Cursor
  // -----------------------------------------------------------------------------
  /** extension for cursor */
  extension (cursor: Cursor) {

    /** get next cursor */
    def next: Option[Cursor] = cursor match
      case InstCursor(_, rest) => InstCursor.from(rest)

    /** get current instruction from cursor */
    def curr: Option[Inst] = cursor match
      case InstCursor(curr, _) => Some(curr)
  }

  // -----------------------------------------------------------------------------
  // Heap
  // -----------------------------------------------------------------------------
  /** extension for heap */
  extension (heap: Heap) {

    /** getters */
    def apply(addr: Addr): Obj =
      heap.map.getOrElse(addr, error(s"unknown address: $addr"))
    def apply(addr: Addr, key: PureValue): Value = heap(addr) match
      // TODO handle access to intrinsics, algorithms
      case (s: IRSymbol)          => s(key)
      case (m: IRMap)             => m(key)
      case (l: IRList)            => l(key)
      case IRNotSupported(_, msg) => throw NotSupported(msg)

    /** setters */
    def update(addr: Addr, prop: PureValue, value: Value): Unit =
      heap(addr) match
        case (m: IRMap) => m.update(prop, value)
        case v          => error(s"not a heap.map: $v")

    /** delete */
    def delete(addr: Addr, prop: PureValue): Unit = heap(addr) match
      case (m: IRMap) => m.delete(prop)
      case v          => error(s"not a heap.map: $v")

    /** object operators */
    def append(addr: Addr, value: PureValue): Unit = heap(addr) match
      case (l: IRList) => l.append(value)
      case v           => error(s"not a list: $v")
    def prepend(addr: Addr, value: PureValue): Unit = heap(addr) match
      case (l: IRList) => l.prepend(value)
      case v           => error(s"not a list: $v")
    def pop(addr: Addr, idx: PureValue): Value = heap(addr) match
      case (l: IRList) => l.pop(idx)
      case v           => error(s"not a list: $v")
    def keys(addr: Addr, intSorted: Boolean): Addr =
      alloc(IRList(heap(addr) match {
        case (m: IRMap) if !intSorted => m.keys
        case (m: IRMap)               => m.sortedKeys
        case obj                      => error(s"not a heap.map: $obj")
      }))
    def copyObj(addr: Addr): Addr = alloc(heap(addr).copied)

    /** object allocations */
    def allocMap(
      ty: Ty,
      m: Map[PureValue, Value] = Map(),
    ): Addr =
      // TODO create subMap if needed
      val irMap = IRMap(ty)
      for ((k, v) <- m) irMap.update(k, v)
      alloc(irMap)
    def allocList(list: List[PureValue]): Addr = alloc(IRList(list.toVector))
    def allocSymbol(desc: PureValue): Addr = alloc(IRSymbol(desc))

    /** allocation helper */
    private def alloc(obj: Obj): Addr =
      val newAddr = DynamicAddr(heap.size)
      heap.map += newAddr -> obj
      heap.size += 1
      newAddr

    /** copied */
    def copied: Heap =
      Heap(
        MMap.from(heap.map.toList.map {
          case (addr, obj) =>
            addr -> obj.copied
        }),
        heap.size,
      )
  }

  // -----------------------------------------------------------------------------
  // IR Object
  // -----------------------------------------------------------------------------
  /** extension for IR object */
  extension (obj: Obj) {

    /** copied */
    def copied: Obj = obj match
      case s: IRSymbol       => s.copied
      case m: IRMap          => m.copied
      case l: IRList         => l.copied
      case n: IRNotSupported => n.copied
  }

  // -----------------------------------------------------------------------------
  // IR Symbol
  // -----------------------------------------------------------------------------
  /** extension for IRSymbol */
  extension (irSym: IRSymbol) {

    /** getters */
    def apply(key: Value): Value = key match
      case Str("Description") => irSym.desc
      case v                  => error(s"an invalid symbol field access: $v")

    /** copied */
    def copied: IRSymbol = IRSymbol(irSym.desc)
  }

  // -----------------------------------------------------------------------------
  // IR Map
  // -----------------------------------------------------------------------------
  /** extension for IRMap */
  extension (irMap: IRMap) {

    /** get key, value pairs */
    def pairs: Map[Value, Value] =
      irMap.props.foldLeft(Map[Value, Value]()) {
        case (m, (k, mv)) =>
          m + (k -> mv.value)
      }

    /** getters */
    def apply(prop: PureValue): Value =
      (irMap.props.get(prop), irMap.ty, prop) match
        case (Some(mv), _, _) => mv.value
        case _                => Absent

    /** setters */
    def findOrUpdate(prop: PureValue, value: Value): Unit =
      irMap.props.get(prop) match
        case Some(_) =>
        case _       => update(prop, value)
    def update(prop: PureValue, value: Value): Unit =
      val creationTime = irMap.props
        .get(prop)
        .map { case IRMapValue(_, t) => t }
        .getOrElse({ irMap.size += 1; irMap.size })
      irMap.props += prop -> IRMapValue(value, creationTime)

    /** deletes */
    def delete(prop: PureValue): Unit = irMap.props -= prop

    /** object operators */
    def keys: Vector[PureValue] = irMap.props.map(_._1).toVector
    def sortedKeys: Vector[PureValue] = ??? // TODO sorted keys

    /** copied */
    def copied: IRMap =
      val newProps = MMap[PureValue, IRMapValue]()
      newProps ++= irMap.props
      IRMap(irMap.ty, newProps, irMap.size)
  }

  // -----------------------------------------------------------------------------
  // IR List
  // -----------------------------------------------------------------------------
  /** extension for IRList */
  extension (irList: IRList) {

    /** getters */
    def apply(key: PureValue): PureValue = key match
      case INum(long) =>
        val idx = long.toInt
        if (0 <= idx && idx < irList.values.length) irList.values(idx)
        else Absent
      case Str("length") => INum(irList.values.length)
      case v             => error(s"invalid key: $v")

    /** object operators */
    def append(value: PureValue): Unit = irList.values :+= value
    def prepend(value: PureValue): Unit = irList.values +:= value
    def pop(idx: PureValue): PureValue = idx match
      case INum(long) =>
        val k = long.toInt
        if (k < 0 || k >= irList.values.length)
          error(s"Out of range: $k of $irList")
        val v = irList.values(k)
        irList.values = irList.values
          .slice(0, k) ++ irList.values.slice(k + 1, irList.values.length)
        v
      case v => error(s"not an integer index: $irList[$v]")

    /** copied */
    def copied: IRList = IRList(irList.values)
  }

  // -----------------------------------------------------------------------------
  // IR Not Supported
  // -----------------------------------------------------------------------------
  /** extension for IRNotSupported */
  extension (irNotSup: IRNotSupported) {

    /** object operators */
    def copied: IRNotSupported = IRNotSupported(irNotSup.tyname, irNotSup.desc)
  }

  // -----------------------------------------------------------------------------
  // Value
  // -----------------------------------------------------------------------------
  /** extension for Value */
  extension (v: Value) {
    // escape completion
    def escaped: PureValue = v match
      case CompValue(CONST_NORMAL, value, _) => value
      case CompValue(_, _, _) =>
        error(s"unchecked abrupt completion: $v")
      case pure: PureValue => pure

    // check abrupt completion
    def isCompletion: Boolean = v match
      case comp: CompValue => true
      case _               => false

    // check abrupt completion
    def isAbruptCompletion: Boolean = v match
      case comp: CompValue => comp.ty != CONST_NORMAL
      case _               => false

    // wrap completion
    def wrapCompletion: CompValue = wrapCompletion(CONST_NORMAL)
    def wrapCompletion(ty: Const): CompValue = v match
      case comp: CompValue => comp
      case pure: PureValue => CompValue(ty, pure)
  }
}
