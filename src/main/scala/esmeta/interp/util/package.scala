package esmeta.interp.util

import esmeta.interp.*
import esmeta.ir.{Func => IRFunc, *}
import esmeta.cfg.*
import esmeta.cfg.util.*
import esmeta.error.*
import esmeta.util.BaseUtils.*
import esmeta.util.*
import scala.collection.mutable.{Map => MMap}
import scala.math.{BigInt => SBigInt}

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

/** normal completion */
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
  def func: Func = st.context.cursor match {
    case NodeCursor(node) => st.cfg.funcOf(node)
    case ExitCursor(func) => func
  }

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

  /** setters */
  def define(x: Id, value: Value): st.type = x match
    case x: Global => st.globals += x -> value; st
    case x: Local  => st.context.locals += x -> value; st
  def update(refV: RefValue, value: Value): st.type = refV match {
    case IdValue(x) => update(x, value); st
    case PropValue(base, prop) =>
      base.escaped match {
        case addr: Addr => update(addr, prop, value); st
        case _          => error(s"illegal reference update: $refV = $value")
      }
  }
  def update(x: Id, value: Value): st.type =
    x match
      case x: Global if exists(x) => st.globals += x -> value
      case x: Name if exists(x)   => st.context.locals += x -> value
      case x: Temp                => st.context.locals += x -> value
      case _ => error(s"illegal variable update: $x = $value")
    st
  def update(addr: Addr, prop: PureValue, value: Value): st.type =
    st.heap.update(addr, prop, value); st

  /** existence checks */
  def exists(x: Id): Boolean = (x match {
    case x: Global => st.globals contains x
    case x: Local  => st.context.locals contains x
  }) && directLookup(x) != Absent
  def exists(ref: RefValue): Boolean = ref match {
    case IdValue(id)           => exists(id)
    case PropValue(base, prop) => st(base.escaped, prop) != Absent
  }

  /** delete a property from a map */
  def delete(refV: RefValue): st.type = refV match {
    case IdValue(x) =>
      error(s"cannot delete variable $x")
    case PropValue(base, prop) =>
      base.escaped match {
        case addr: Addr =>
          st.heap.delete(addr, prop); st
        case _ =>
          error(s"illegal reference delete: delete $refV")
      }
  }

  /** object operators */
  def append(addr: Addr, value: PureValue): st.type =
    st.heap.append(addr, value); st
  def prepend(addr: Addr, value: PureValue): st.type =
    st.heap.prepend(addr, value); st
  def pop(addr: Addr, front: Boolean): PureValue =
    st.heap.pop(addr, front)
  def copyObj(addr: Addr): Addr =
    st.heap.copyObj(addr)
  def keys(addr: Addr, intSorted: Boolean): Addr =
    st.heap.keys(addr, intSorted)
  def allocMap(tname: String, map: Map[PureValue, PureValue] = Map()): Addr =
    st.heap.allocMap(tname, map)
  def allocList(list: List[PureValue]): Addr =
    st.heap.allocList(list)
  def allocSymbol(desc: PureValue): Addr =
    st.heap.allocSymbol(desc)
  def setType(addr: Addr, tname: String): st.type =
    st.heap.setType(addr, tname); st

  /** get string for a given address */
  def getString(value: Value): String = value match {
    case comp: Comp =>
      comp.toString + (comp.value match {
        case addr: Addr => " -> " + st.heap(addr).toString
        case _          => ""
      })
    case addr: Addr => addr.toString + " -> " + st.heap(addr).toString
    case _          => value.toString
  }
}

/** extensions for calling contexts */
extension (callCtxt: CallContext) {

  /** function name * */
  def name: String = callCtxt.context.func.head.name

  /** copy contexts */
  def copied: CallContext = callCtxt.copy(context = callCtxt.context.copied)
}

/** extensions for contexts */
extension (ctxt: Context) {

  /** copy contexts */
  def copied: Context = ctxt.copy(locals = MMap.from(ctxt.locals))

  /** name */
  def name: String = ctxt.func.head.name
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

  /** setters */
  def update(addr: Addr, prop: PureValue, value: Value): heap.type =
    heap(addr) match {
      case (m: MapObj) => m.update(prop, value); heap
      case v           => error(s"not a map: $v")
    }

  /** delete */
  def delete(addr: Addr, prop: PureValue): heap.type = heap(addr) match {
    case (m: MapObj) => m.delete(prop); heap
    case v           => error(s"not a map: $v")
  }

  /** appends */
  def append(addr: Addr, value: PureValue): heap.type = heap(addr) match {
    case (l: ListObj) =>
      l.append(value); heap
    case v => error(s"not a list: $v")
  }

  /** prepends */
  def prepend(addr: Addr, value: PureValue): heap.type = heap(addr) match {
    case (l: ListObj) =>
      l.prepend(value); heap
    case v => error(s"not a list: $v")
  }

  /** pops */
  def pop(addr: Addr, front: Boolean): PureValue = heap(addr) match {
    case (l: ListObj) => l.pop(front)
    case v            => error(s"not a list: $v")
  }

  /** copy objects */
  def copyObj(addr: Addr): Addr = alloc(heap(addr).copied)

  /** keys of map */
  def keys(addr: Addr, intSorted: Boolean): Addr = {
    alloc(ListObj(heap(addr) match {
      case (m: MapObj) => m.keys(intSorted)
      case obj         => error(s"not a map: $obj")
    }))
  }

  /** map allocations */
  def allocMap(
    tname: String,
    m: Map[PureValue, PureValue],
  ): Addr = {
    val irMap =
      if (tname == "Record") MapObj(tname, MMap(), 0) else MapObj(tname)
    for ((k, v) <- m) irMap.update(k, v)
    if (hasSubMap(tname))
      val subMap = MapObj("SubMap")
      irMap.update(Str("SubMap"), alloc(subMap))
    alloc(irMap)
  }

  private def hasSubMap(tname: String): Boolean =
    (tname endsWith "Object") || (tname endsWith "EnvironmentRecord")

  /** list allocations */
  def allocList(list: List[PureValue]): Addr = alloc(ListObj(list.toVector))

  /** symbol allocations */
  def allocSymbol(desc: PureValue): Addr = alloc(SymbolObj(desc))

  // allocation helper
  private def alloc(obj: Obj): Addr = {
    val newAddr = DynamicAddr(heap.size)
    heap.map += newAddr -> obj
    heap.size += 1
    newAddr
  }

  // property access helper
  private def getAddrValue(
    addr: Addr,
    propName: String,
  ): Addr = heap(addr, Str(propName)) match {
    case addr: Addr => addr
    case v          => error(s"not an address: $v")
  }

  // property value getter
  private def getPropValue(
    addr: Value,
    propName: String,
  ): Value = addr match {
    case addr: Addr =>
      val submap = getAddrValue(addr, "SubMap")
      val prop = getAddrValue(submap, propName)
      heap(prop, Str("Value"))
    case _ => error(s"not an address: $addr")
  }

  /** set type of objects */
  def setType(addr: Addr, tname: String): heap.type = heap(addr) match {
    case (irMap: MapObj) =>
      irMap.tname = tname; heap
    case _ => error(s"invalid type update: $addr")
  }

  /** copied */
  def copied: Heap =
    val newMap = MMap.from(heap.map.toList.map {
      case (addr, obj) => addr -> obj.copied
    })
    Heap(newMap, heap.size)
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

  /** copy of object */
  def copied: Obj = obj match {
    case MapObj(tname, props, size) => MapObj(tname, MMap.from(props), size)
    case ListObj(values)            => ListObj(Vector.from(values))
    case SymbolObj(desc)            => SymbolObj(desc)
    case _                          => obj
  }
}

/** extensions for list objects */
extension (list: ListObj) {

  // appends
  def append(value: PureValue): list.type = { list.values :+= value; list }

  // prepends
  def prepend(value: PureValue): list.type = { list.values +:= value; list }

  // pops
  def pop(front: Boolean): PureValue =
    val vs = list.values
    if (vs.isEmpty) throw OutOfRange(list, 0)
    val v = if (front) vs.head else vs.last
    list.values = if (front) vs.drop(1) else vs.dropRight(1)
    v
}

/** extensions for map objects */
extension (map: MapObj) {

  /** setters */
  def findOrUpdate(prop: PureValue, value: Value): map.type =
    map.props.get(prop) match
      case Some(_) => map
      case _       => update(prop, value)

  /** updates */
  def update(prop: PureValue, value: Value): map.type =
    val id = map.props
      .get(prop)
      .map(_.creationTime)
      .getOrElse({ map.size += 1; map.size })
    map.props += prop -> MapObj.Prop(value, id)
    map

  /** deletes */
  def delete(prop: PureValue): map.type = { map.props -= prop; map }

  /** pairs of map */
  def pairs: Map[PureValue, Value] = (map.props.map {
    case (k, (MapObj.Prop(v, _))) => k -> v
  }).toMap

  /** keys of map */
  def keys(intSorted: Boolean): Vector[PureValue] = {
    if (!intSorted) {
      if (map.tname == "SubMap")
        map.props.toVector
          .sortBy(_._2._2)
          .map(_._1)
      else map.props.toVector.map(_._1).sortBy(_.toString)
    } else
      (for {
        (Str(s), _) <- map.props.toVector
        d: Double = ??? // ESValueParser.str2num(s)
        if toStringHelper(d) == s
        i = d.toLong // should handle unsigned integer
        if d == i
      } yield (s, i)).sortBy(_._2).map { case (s, _) => Str(s) }
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

  /** type conversion */
  def toStr(e: Expr): String = v match
    case Str(s) => s
    case _      => throw NoString(e, v)
  def toInt(e: Expr): Int = v match
    case Number(n) if n.isValidInt => n.toInt
    case Math(n) if n.isValidInt   => n.toInt
    case _                         => throw NoInteger(e, v)
  def getList(e: Expr, st: State): ListObj = v match
    case addr: Addr =>
      st(addr) match
        case l: ListObj => l
        case obj        => throw NoList(e, obj)
    case _ => throw NoAddr(e, v)
}

/** extensions for completion values */
extension (comp: Comp) {
  def targetValue: PureValue =
    comp.target.fold[PureValue](CONST_EMPTY)(Str.apply)
}
