package esmeta.ir

import esmeta.error.NotSupported
import esmeta.util.BaseUtils.*
import scala.collection.mutable.{Map => MMap}

/** IR Utils */
object Utils {
  // ////////////////////////////////////////////////////////////////////////////
  // Syntax
  // ////////////////////////////////////////////////////////////////////////////
  // TODO Define Syntax extension
  /** extension for ty */
  extension (ty: Ty) {
    // check whether it has SubMap
    def hasSubMap: Boolean = {
      (ty.name endsWith "Object") || (ty.name endsWith "EnvironmentRecord")
    }
  }

  // ////////////////////////////////////////////////////////////////////////////
  // States
  // ////////////////////////////////////////////////////////////////////////////
  /** extension for state */
  extension (st: State) {

    /** move the cursor */
    def moveTo(program: Program): State = moveTo(program.insts)
    def moveTo(insts: List[Inst]): State = moveTo(ISeq(insts))
    def moveTo(inst: Inst): State = {
      st.context.cursorOpt = st.cursorGen(inst)
      st
    }

    /** return id and its value */
    def retId: Id = st.context.retId

    /** get local variable maps */
    def locals: MMap[Id, Value] = st.context.locals

    /** lookup variable directly */
    def directLookup(x: Id): Value =
      locals.getOrElse(
        x,
        st.globals.getOrElse(x, error(s"unknown variable: ${x.name}")),
      )

    // TODO
    /** getters */
    def apply(refV: RefValue): Value =
      refV match {
        case RefValueId(x)            => st(x)
        case RefValueProp(base, prop) => st(base, prop)
      }
    def apply(x: Id): Value =
      directLookup(x) match {
        case Absent if st.context.isBuiltin => Undef
        case v                              => v
      }
    // TODO PureValue issue
    def apply(base: Value, prop: Value): Value = base match {
      case comp: CompValue =>
        prop match {
          case Str("Type")   => comp.ty
          case Str("Value")  => comp.value
          case Str("Target") => comp.target
          case _             => st(comp.escaped, prop)
        }
      case addr: Addr => st.heap(addr, prop)
      case Str(str)   => st(str, prop)
      case v          => error(s"not a proper reference base: $v")
    }

    def apply(str: String, prop: Value): Value = prop match {
      case Str("length") => INum(str.length)
      case INum(k) =>
        Str(k.toInt.toString)
      case Num(k) => Str(k.toInt.toString)
      case v      => error(s"wrong access of string reference: $str.$prop")
    }

    def apply(addr: Addr): Obj = st.heap(addr)

    /** setters */
    def update(refV: RefValue, value: Value): State = refV match {
      case RefValueId(x) =>
        update(x, value); st
      case RefValueProp(base, prop) =>
        base.escaped match {
          case addr: Addr =>
            update(addr, prop, value); st
          case _ =>
            error(s"illegal reference update: $refV = $value")
        }
    }
    def update(x: Id, value: Value): State = {
      if (locals contains x) locals += x -> value
      else if (st.globals contains x) st.globals += x -> value
      else error(s"illegal variable update: $x = $value")
      st
    }
    def update(addr: Addr, prop: Value, value: Value): State = {
      st.heap.update(addr, prop, value); st
    }

    /** existence checks */
    def exists(id: Id): Boolean = {
      val defined = st.globals.contains(id) || locals.contains(id)
      defined && directLookup(id) != Absent
    }
    def exists(ref: RefValue): Boolean = ref match {
      case RefValueId(id)           => exists(id)
      case RefValueProp(base, prop) => st(base.escaped, prop) != Absent
    }

    /** delete a property from a map */
    def delete(refV: RefValue): State = refV match {
      case RefValueId(x) =>
        error(s"cannot delete variable $x")
      case RefValueProp(base, prop) =>
        base.escaped match {
          case addr: Addr =>
            st.heap.delete(addr, prop); st
          case _ =>
            error(s"illegal reference delete: delete $refV")
        }
    }

    /** object operators */
    def append(addr: Addr, value: Value): State = {
      st.heap.append(addr, value); st
    }
    def prepend(addr: Addr, value: Value): State = {
      st.heap.prepend(addr, value); st
    }
    def pop(addr: Addr, idx: Value): Value = st.heap.pop(addr, idx)
    // TODO copy method of Obj type cannot be defined
    // def copyObj(addr: Addr): Addr = st.heap.copyObj(addr)
    def keys(addr: Addr, intSorted: Boolean): Addr =
      st.heap.keys(addr, intSorted)
    def allocMap(ty: Ty, map: Map[Value, Value] = Map()): Addr =
      st.heap.allocMap(ty, map)
    def allocList(list: List[Value]): Addr = st.heap.allocList(list)
    def allocSymbol(desc: Value): Addr = st.heap.allocSymbol(desc)
    def setType(addr: Addr, ty: Ty): State = {
      st.heap.setType(addr, ty); st
    }

    // get string for a given address
    def getString(value: Value): String = value match {
      case comp: CompValue =>
        comp.toString + (comp.value match {
          case addr: Addr => " -> " + st.heap(addr).toString
          case _          => ""
        })
      case addr: Addr => addr.toString + " -> " + st.heap(addr).toString
      case _          => value.toString
    }

    /** copied */
    def copied: State = {
      val newContext = st.context.copied
      val newCtxtStack = st.ctxtStack.map(_.copied)
      val newGlobals = MMap.from(st.globals)
      // TODO copy method of Obj type cannot be defined
      val newHeap = ??? // st.heap.copied
      State(
        st.cursorGen,
        newContext,
        newCtxtStack,
        newGlobals,
        newHeap,
        st.fnameOpt,
      )
    }

    /** move to the next cursor */
    def moveNext: Unit = st.context.moveNext

    // TODO Not impl. {AST, Node}
    // /** get AST of topmost evaluation */
    // def currentAst: Option[AST] = (context :: ctxtStack)
    //  .flatMap(c => {
    //    if (c.isAstEvaluation) c.astOpt else None
    //  })
    //  .headOption

    // /** get current node */
    // def currentNode: Option[Node] = context.cursorOpt.flatMap {
    //  case NodeCursor(n) => Some(n)
    //  case _             => None
    // }

    // /** get position of AST of topmost evaluation */
    // // start line, end line, start index, end index
    // def getJsPos(): (Int, Int, Int, Int) =
    //  currentAst.fold((-1, -1, -1, -1))(ast => {
    //    val Span(start, end) = ast.span
    //    (start.line, end.line, start.index, end.index)
    //  })
  }

  /** extension for context */
  extension (ctxt: Context) {
    def copied: Context = ctxt.copy(locals = MMap.from(ctxt.locals))
    def isBuiltin: Boolean = ???
    // algo.fold(false)(_.isBuiltin)
    /** move cursor */
    def moveNext: Unit = {
      // prevCursorOpt = cursorOpt
      ctxt.cursorOpt = ctxt.cursorOpt.flatMap(_.next)
    }
    // TODO Not impl. algo
    // /** debugger info */
    // def getAlgoName: String = algo match {
    //  case Some(algo) => algo.name
    //  case None       => name
    // }
    // private def getLine(cur: Option[Cursor]): Int = cur.flatMap(_.inst) match {
    //  case Some(inst) => inst.line.getOrElse(-1)
    //  case None       => -1
    // }
    // def getInfo(
    //  fromPrev: Boolean = false,
    // ): (String, Int, List[(String, String)]) = (
    //  getAlgoName,
    //  getLine(if (fromPrev) prevCursorOpt else cursorOpt),
    //  locals.toList.map { case (Id(name), v) =>
    //    (name, v.toString)
    //  },
    // )
    // /** check if AST evaluation */
    // def isAstEvaluation: Boolean =
    //  astOpt.nonEmpty && algo.fold(false)(_.head match {
    //    case s: SyntaxDirectedHead =>
    //      s.methodName == "Evaluation" || s.methodName == "NamedEvaluation"
    //    case _ => false
    //  })
    // /** check if JS call */
    // def isJsCall = algo.fold(false)(_.name match {
    //  case "Call" | "Construct" => true
    //  case _                    => false
    // })
  }

  /** extension for heap */
  extension (heap: Heap) {

    /** getters */
    def apply(addr: Addr): Obj =
      heap.map.getOrElse(addr, error(s"unknown address: $addr"))
    def apply(addr: Addr, key: Value): Value = heap(addr) match {
      case (s: IRSymbol) => s(key)
      // TODO Algo, Intrinsics is not yet supported
      // case (IRMap(Ty(js.ALGORITHM), _, _))  => getAlgorithm(key)
      // case (IRMap(Ty(js.INTRINSICS), _, _)) => getIntrinsics(key)
      case (m: IRMap)             => m(key)
      case (l: IRList)            => l(key)
      case IRNotSupported(_, msg) => throw NotSupported(msg)
    }

    /** setters */
    def update(addr: Addr, prop: Value, value: Value): Heap =
      heap(addr) match {
        case (m: IRMap) =>
          m.update(prop, value); heap
        case v => error(s"not a heap.map: $v")
      }

    /** delete */
    def delete(addr: Addr, prop: Value): Heap = heap(addr) match {
      case (m: IRMap) =>
        m.delete(prop); heap
      case v => error(s"not a heap.map: $v")
    }

    /** appends */
    def append(addr: Addr, value: Value): Heap = heap(addr) match {
      case (l: IRList) =>
        l.append(value); heap
      case v => error(s"not a list: $v")
    }

    /** prepends */
    def prepend(addr: Addr, value: Value): Heap = heap(addr) match {
      case (l: IRList) =>
        l.prepend(value); heap
      case v => error(s"not a list: $v")
    }

    /** pops */
    def pop(addr: Addr, idx: Value): Value = heap(addr) match {
      case (l: IRList) => l.pop(idx)
      case v           => error(s"not a list: $v")
    }

    // TODO copy method of Obj type cannot be defined
    /** copy objects */
    // def copyObj(addr: Addr): Addr = alloc(heap(addr).copied)

    /** keys of map */
    def keys(addr: Addr, intSorted: Boolean): Addr = {
      alloc(IRList(heap(addr) match {
        case (m: IRMap) => m.keys(intSorted)
        case obj        => error(s"not a heap.map: $obj")
      }))
    }

    /** map allocations */
    def allocMap(
      ty: Ty,
      m: Map[Value, Value],
    ): Addr = {
      val irMap: IRMap =
        if (ty.name == "Record") IRMap(ty, MMap(), 0L) else ??? // IRMap(ty)
      for ((k, v) <- m) irMap.update(k, v)
      if (ty.hasSubMap) {
        val subMap = ??? // IRMap(Ty("SubMap"))
        irMap.update(Str("SubMap"), alloc(subMap))
      }
      alloc(irMap)
    }

    /** list allocations */
    def allocList(list: List[Value]): Addr = alloc(IRList(list.toVector))

    /** symbol allocations */
    def allocSymbol(desc: Value): Addr = alloc(IRSymbol(desc))

    /** allocation helper */
    private def alloc(obj: Obj): Addr = {
      val newAddr = DynamicAddr(heap.size)
      heap.map += newAddr -> obj
      heap.size += 1
      newAddr
    }

    /** property access helper */
    private def getAddrValue(
      addr: Addr,
      propName: String,
    ): Addr = heap(addr, Str(propName)) match {
      case addr: Addr => addr
      case v          => error(s"not an address: $v")
    }
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
    def setType(addr: Addr, ty: Ty): Heap = heap(addr) match {
      case (irmap: IRMap) =>
        irmap.ty = ty; heap
      case _ => error(s"invalid type update: $addr")
    }

    // TODO copy method of Obj type cannot be defined
    // copied
    // def copied: Heap = {
    //  val newmap = MMap.from(heap.map.toList.map { case (addr, obj) =>
    //    addr -> obj.copied
    //  })
    //  Heap(newmap, heap.size)
    // }

    // TODO not impl. js
    // // speical object access helper
    // def getAlgorithm(key: Value): Value = key match {
    //  case Str(str) => js.algoheap.map.get(str).map(Func).getOrElse(Absent)
    //  case _        => error(s"invalid algorithm: $key")
    // }
    // def getIntrinsics(key: Value): Value = key match {
    //  case Str(str) if js.intrinsicRegex.matches(str) =>
    //    // resolve %A.B.C%
    //    val js.intrinsicRegex(path) = str
    //    path.split("\\.").toList match {
    //      case base :: rest =>
    //        val baseAddr = js.intrinsicToAddr(base)
    //        rest.foldLeft(baseAddr: Value)(getPropValue)
    //      case Nil =>
    //        error(s"invalid intrinsics: $key")
    //    }
    //  case _ => error(s"invalid intrinsics: $key")
    // }
  }

  /** extension for IRSymbol */
  extension (irSym: IRSymbol) {
    // TODO val in extension => def?
    def ty: Ty = Ty("Symbol")

    /** getters */
    def apply(key: Value): Value = key match {
      case Str("Description") => irSym.desc
      case v                  => error(s"an invalid symbol field access: $v")
    }

    /** copy of object */
    def copied: IRSymbol = IRSymbol(irSym.desc)
  }

  /** extension for IRMap */
  extension (irMap: IRMap) {
    // get pairs
    def pairs: Map[Value, Value] =
      irMap.props.foldLeft(Map[Value, Value]()) { case (m, (k, mv)) =>
        m + (k -> mv.value)
      }

    // getters
    def apply(prop: Value): Value =
      (irMap.props.get(prop), irMap.ty, prop) match {
        case (Some(mv), _, _) => mv.value
        case _                => Absent
      }

    // setters
    def findOrUpdate(prop: Value, value: Value): IRMap = {
      irMap.props.get(prop) match {
        case Some(_) => irMap
        case _       => update(prop, value)
      }
    }

    // updates
    def update(prop: Value, value: Value): IRMap = {
      val creationTime = irMap.props
        .get(prop)
        .map { case IRMapValue(_, t) => t }
        .getOrElse({ irMap.size += 1; irMap.size })
      irMap.props += prop -> IRMapValue(value, creationTime)
      irMap
    }

    // deletes
    def delete(prop: Value): IRMap = { irMap.props -= prop; irMap }

    // copy of object
    def copied: IRMap = {
      val newProps = MMap[Value, IRMapValue]()
      newProps ++= irMap.props
      IRMap(irMap.ty, newProps, irMap.size)
    }

    // keys of map
    def keys(intSorted: Boolean): Vector[Value] = {
      if (!intSorted) {
        if (irMap.ty.name == "SubMap")
          irMap.props.toVector
            .sortBy(_._2._2)
            .map(_._1)
        else
          irMap.props.toVector
            .map(_._1)
            .sortBy(_.toString)
      } else ???
      // TODO consider the case when intSorted
      //  (for {
      //  (Str(s), _) <- irMap.props.toVector
      //  d = ESValueParser.str2num(s)
      //  if toStringHelper(d) == s
      //  i = d.toLong // should handle unsigned integer
      //  if d == i
      // } yield (s, i))
      //  .sortBy(_._2)
      //  .map { case (s, _) => Str(s) }
    }
    // XXX
    def apply(tyname: String)(pairs: Iterable[(Value, Value)]): IRMap = {
      val irMap = ??? // IRMap(Ty(tyname))
      for ((prop, value) <- pairs) irMap.update(prop, value)
      irMap
    }
    def apply(ty: Ty): IRMap =
      val props: MMap[Value, IRMapValue] = MMap()
      IRMap(ty, props, 0L)
  }

  /** extension for IRList */
  extension (irList: IRList) {
    // types
    def ty: Ty = Ty("List")

    // getters
    def apply(key: Value): Value = key match {
      case INum(long) =>
        val idx = long.toInt
        if (0 <= idx && idx < irList.values.length) irList.values(idx)
        else Absent
      case Str("length") => INum(irList.values.length)
      case v             => error(s"invalid key: $v")
    }

    // appends
    def append(value: Value): irList.type = { irList.values :+= value; irList }

    // prepends
    def prepend(value: Value): irList.type = { irList.values +:= value; irList }

    // pops
    def pop(idx: Value): Value = idx match {
      case INum(long) => {
        val k = long.toInt
        if (k < 0 || k >= irList.values.length)
          error(s"Out of range: $k of $irList")
        val v = irList.values(k)
        irList.values = irList.values
          .slice(0, k) ++ irList.values.slice(k + 1, irList.values.length)
        v
      }
      case v => error(s"not an integer index: $irList[$v]")
    }

    // copy of object
    def copied: IRList = IRList(irList.values)
  }

  /** extension for IRNotSupported */
  extension (irNotSup: IRNotSupported) {
    def ty: Ty = Ty(irNotSup.tyname)

    // copy of object
    def copied: IRNotSupported = IRNotSupported(irNotSup.tyname, irNotSup.desc)
  }

  /** extension for Value */
  extension (v: Value) {
    // escape completion
    def escaped: Value = v match {
      // TODO need to be revised(NormalComp)
      case CompValue(CONST_NORMAL, value, None) => value
      case CompValue(_, _, _) =>
        error(s"unchecked abrupt completion: $v")
      case pure: Value => pure
    }

    // check abrupt completion
    def isCompletion: Boolean = v match {
      case comp: CompValue => true
      case _               => false
    }

    // check abrupt completion
    def isAbruptCompletion: Boolean = v match {
      case comp: CompValue => comp.ty != CONST_NORMAL
      case _               => false
    }

    // wrap completion
    def wrapCompletion: CompValue = wrapCompletion(
      // TODO Type Definition
      CONST_NORMAL.asInstanceOf[Const],
    )
    def wrapCompletion(ty: Const): CompValue = v match {
      case comp: CompValue => comp
      // TODO erase pure
      case pure: Value => CompValue(ty, pure, None)
    }
  }

  /** extension for obj */
  extension (obj: Object) {
    def ty: Ty = obj match {
      case IRSymbol                 => Ty("Symbol")
      case IRMap(ty, _, _)          => ty
      case IRList                   => Ty("List")
      case IRNotSupported(tyStr, _) => Ty(tyStr)
    }
  }

  /** extension for CompValue */
  extension (comp: CompValue) {
    def target: Value = comp.targetOpt.fold[Value](CONST_EMPTY)(Str(_))
  }
}
