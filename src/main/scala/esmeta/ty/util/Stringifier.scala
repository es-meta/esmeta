package esmeta.ty.util

import esmeta.LINE_SEP
import esmeta.state.{Number, Math}
import esmeta.ty.*
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*

/** stringifier for types */
object Stringifier {

  /** type elements */
  given elemRule: Rule[TyElem] = (app, elem) =>
    elem match
      case elem: UnknownTy   => unknownTyRule(app, elem)
      case elem: ValueTy     => valueTyRule(app, elem)
      case elem: CompTy      => compTyRule(app, elem)
      case elem: PureValueTy => pureValueTyRule(app, elem)
      case elem: RecordTy    => recordTyRule(app, elem)
      case elem: ListTy      => listTyRule(app, elem)
      case elem: NameTy      => nameTyRule(app, elem)
      case elem: AstValueTy  => astValueTyRule(app, elem)
      case elem: MapTy       => mapTyRule(app, elem)
      case elem: MathTy      => mathTyRule(app, elem)
      case elem: InfinityTy  => infinityTyRule(app, elem)
      case elem: NumberTy    => numberTyRule(app, elem)
      case elem: BoolTy      => boolTyRule(app, elem)

  /** types */
  given tyRule: Rule[Ty] = (app, ty) =>
    ty match
      case ty: UnknownTy => unknownTyRule(app, ty)
      case ty: ValueTy   => valueTyRule(app, ty)

  /** unknown types */
  given unknownTyRule: Rule[UnknownTy] = (app, ty) =>
    app >> "Unknown"
    ty.msg.fold(app)(app >> "[\"" >> _ >> "\"]")

  /** value types */
  given valueTyRule: Rule[ValueTy] = (app, ty) =>
    if (ty.isTop) app >> "Any"
    else if (!ty.isBottom)
      FilterApp(app)
        .add(ty.comp, !ty.comp.isBottom)
        .add(ty.pureValue, !ty.pureValue.isBottom)
        .add(ty.map, !ty.map.isBottom)
        .app
    else app >> "Bot"

  /** completion record types */
  given compTyRule: Rule[CompTy] = (app, ty) =>
    given Rule[PureValueTy] = topRule(pureValueTyRule)
    if (ty.isTop) app >> "CompletionRecord"
    else
      FilterApp(app)
        .add(ty.normal, !ty.normal.isBottom, "Normal")
        .add(ty.abrupt, !ty.abrupt.isBottom, "Abrupt")
        .app

  /** list types */
  given listTyRule: Rule[ListTy] = (app, ty) =>
    ty.elem match
      case None => app
      case Some(elem) =>
        if (elem.isBottom) app >> "Nil"
        else if (elem.isTop) app >> "List"
        else app >> "List[" >> elem >> "]"

  // predefined types
  lazy val predTys: List[(PureValueTy, String)] = List(
    ESPureValueT -> "ESValue",
  )

  /** pure value types (non-completion record types) */
  given pureValueTyRule: Rule[PureValueTy] = (app, origTy) =>
    var ty: PureValueTy = origTy
    if (ty.isTop) app >> "PureValue"
    else
      predTys
        .foldLeft(FilterApp(app)) {
          case (app, (pred, name)) =>
            app.add({ ty --= pred; name }, pred <= ty)
        }
        .add(ty.clo.map(s => s"\"$s\""), !ty.clo.isBottom, "Clo")
        .add(ty.cont, !ty.cont.isBottom, "Cont")
        .add(ty.name, !ty.name.isBottom)
        .add(ty.record, !ty.record.isBottom)
        .add(ty.list, !ty.list.isBottom)
        .add("Symbol", !ty.symbol.isBottom)
        .add(ty.astValue, !ty.astValue.isBottom)
        .add(ty.nt.map(_.toString), !ty.nt.isBottom, "Nt")
        .add("CodeUnit", !ty.codeUnit.isBottom)
        .add(ty.enumv.map(s => s"~$s~"), !ty.enumv.isBottom, "Enum")
        .add(ty.math, !ty.math.isBottom)
        .add(ty.infinity, !ty.infinity.isBottom)
        .add(ty.number, !ty.number.isBottom)
        .add("BigInt", !ty.bigInt.isBottom)
        .add(ty.str.map(s => s"\"$s\""), !ty.str.isBottom, "String")
        .add(ty.bool, !ty.bool.isBottom)
        .add("Undefined", !ty.undef.isBottom)
        .add("Null", !ty.nullv.isBottom)
        .add("Absent", !ty.absent.isBottom)
        .app

  /** named record types */
  given nameTyRule: Rule[NameTy] = (app, ty) =>
    ty.set match
      case Inf => app >> "AnyName"
      case Fin(set) =>
        given Rule[Set[String]] = setRule("", OR, "")
        app >> set

  /** record types */
  given recordTyRule: Rule[RecordTy] = (app, ty) =>
    import RecordTy.*
    given Rule[(String, ValueTy)] = {
      case (app, (key, value)) =>
        app >> "[[" >> key >> "]]"
        if (!value.isTop) app >> ": " >> value
        else app
    }
    given Rule[List[(String, ValueTy)]] = iterableRule("{ ", ", ", " }")
    ty match
      case Top       => app >> "AnyRecord"
      case Elem(map) => app >> map.toList.sortBy(_._1)

  /** AST value types */
  given astValueTyRule: Rule[AstValueTy] = (app, ty) =>
    app >> "Ast"
    ty match
      case AstTopTy         => app
      case AstNameTy(names) => app >> names
      case AstSingleTy(x, i, j) =>
        app >> ":" >> x >> "[" >> i >> "," >> j >> "]"

  /** mathematical value types */
  given mathTyRule: Rule[MathTy] = (app, ty) =>
    ty match
      case MathTopTy      => app >> "Math"
      case IntTy          => app >> "Int"
      case NonPosIntTy    => app >> "NonPosInt"
      case NonNegIntTy    => app >> "NonNegInt"
      case NegIntTy       => app >> "NegInt"
      case PosIntTy       => app >> "PosInt"
      case MathSetTy(set) => if (set.isEmpty) app else app >> "Math" >> set

  /** infinity types */
  given infinityTyRule: Rule[InfinityTy] = (app, ty) =>
    ty.pos match
      case set if set.isEmpty   => app
      case set if set.size == 1 => app >> (if (set.head) "+INF" else "-INF")
      case _                    => app >> "INF"

  /** number types */
  given numberTyRule: Rule[NumberTy] = (app, ty) =>
    ty match
      case NumberTopTy      => app >> "Number"
      case NumberIntTy      => app >> "NumberInt"
      case NumberSetTy(set) => if (set.isEmpty) app else app >> "Number" >> set

  /** boolean types */
  given boolTyRule: Rule[BoolTy] = (app, ty) =>
    ty.set match
      case set if set.isEmpty   => app
      case set if set.size == 1 => app >> (if (set.head) "True" else "False")
      case _                    => app >> "Boolean"

  /** map types */
  given mapTyRule: Rule[MapTy] = (app, ty) =>
    app >> "Map[" >> ty.key >> " |-> " >> ty.value >> "]"

  // rule for bounded set lattice
  private given bsetRule[T: Ordering](using Rule[T]): Rule[BSet[T]] =
    (app, set) =>
      given Rule[List[T]] = iterableRule("[", ", ", "]")
      set match
        case Inf      => app
        case Fin(set) => app >> set.toList.sorted

  // rule for string set
  private given setRule[T: Ordering](using Rule[T]): Rule[Set[T]] =
    setRule("[", ", ", "]")
  private def setRule[T: Ordering](
    pre: String,
    sep: String,
    post: String,
  )(using Rule[T]): Rule[Set[T]] = (app, set) =>
    given Rule[List[T]] = iterableRule(pre, sep, post)
    app >> set.toList.sorted

  // rule for option type for top
  private def topRule[T <: Lattice[T]](
    tRule: Rule[T],
    pre: String = "[",
    post: String = "]",
  ): Rule[T] = (app, t) =>
    given Rule[T] = tRule
    if (!t.isTop) app >> pre >> t >> post
    else app

  // appender with filtering
  private class FilterApp(val app: Appender) {
    private var first = true
    def add[T](
      t: => T,
      valid: Boolean,
      pre: String = "",
      post: String = "",
    )(using tRule: Rule[T]): this.type =
      if (valid)
        if (!first) app >> OR
        else first = false
        app >> pre >> t >> post
      this
  }

  // rule for math
  private given mathRule: Rule[Math] = (app, math) => app >> math.toString
  given Ordering[Math] = Ordering.by(_.decimal)

  // rule for number
  private given numberRule: Rule[Number] = (app, number) =>
    number match
      case Number(Double.PositiveInfinity) => app >> "+INF"
      case Number(Double.NegativeInfinity) => app >> "-INF"
      case Number(n) if n.isNaN            => app >> "NaN"
      case Number(n)                       => app >> n
  given Ordering[Number] = Ordering.by(_.double)

  // separator for type disjuction
  private val OR = " | "
}
