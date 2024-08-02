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
      case elem: TyModel     => tyModelRule(app, elem)
      case elem: TyDecl      => tyDeclRule(app, elem)
      case elem: TyDecl.Elem => tyDeclElemRule(app, elem)
      case elem: FieldMap    => fieldMapRule(app, elem)
      case elem: OptValueTy  => optValueTyRule(app, elem)
      case elem: Ty          => tyRule(app, elem)
      case elem: RecordTy    => recordTyRule(app, elem)
      case elem: ListTy      => listTyRule(app, elem)
      case elem: AstTy       => astTyRule(app, elem)
      case elem: MapTy       => mapTyRule(app, elem)
      case elem: MathTy      => mathTyRule(app, elem)
      case elem: InfinityTy  => infinityTyRule(app, elem)
      case elem: NumberTy    => numberTyRule(app, elem)
      case elem: BoolTy      => boolTyRule(app, elem)

  /** type models */
  given tyModelRule: Rule[TyModel] = (app, model) =>
    given Rule[List[TyDecl]] = iterableRule(sep = LINE_SEP + LINE_SEP)
    app >> model.decls

  /** type declarations */
  given tyDeclRule: Rule[TyDecl] = (app, ty) =>
    val TyDecl(name, parent, elems) = ty
    app >> "type " >> name
    parent.fold(app)(app >> " extends " >> _)
    if (elems.nonEmpty) (app >> " ").wrap("{", "}") {
      elems.map(app :> _ >> ";")
    }
    app

  /** type declaration elements */
  given tyDeclElemRule: Rule[TyDecl.Elem] = (app, elem) =>
    import TyDecl.Elem.*
    elem match
      case Method(name, optional, target) =>
        app >> "def " >> name
        if (optional) app >> "?"
        target.fold(app)(app >> " = " >> _)
      case Field(name, optional, typeStr) =>
        app >> name
        if (optional) app >> "?"
        app >> " : " >> typeStr

  /** field map */
  given fieldMapRule: Rule[FieldMap] = (app, fieldMap) =>
    val FieldMap(map) = fieldMap
    given Rule[(String, OptValueTy)] = {
      case (app, (field, OptValueTy(ty, opt))) =>
        app >> field
        if (opt) app >> "?"
        if (!ty.isTop) app >> " : " >> ty
        app
    }
    given Rule[List[(String, OptValueTy)]] = iterableRule("{ ", ", ", " }")
    if (map.isEmpty) app >> "{}"
    else app >> map.toList.sortBy(_._1)

  /** optional value types */
  given optValueTyRule: Rule[OptValueTy] = (app, ty) =>
    val OptValueTy(value, optional) = ty
    app >> value
    if (optional) app >> "?"
    app

  /** types */
  given tyRule: Rule[Ty] = (app, ty) =>
    ty match
      case ty: UnknownTy => unknownTyRule(app, ty)
      case ty: ValueTy   => valueTyRule(app, ty)

  /** unknown types */
  given unknownTyRule: Rule[UnknownTy] = (app, ty) =>
    app >> "Unknown"
    ty.msg.fold(app)(app >> "[\"" >> _ >> "\"]")

  // predefined types
  private lazy val predTys: List[(ValueTy, String)] = List(
    ESValueT -> "ESValue",
  )

  /** value types */
  given valueTyRule: Rule[ValueTy] = (app, origTy) =>
    var ty: ValueTy = origTy
    if (ty.isTop) app >> "Any"
    else if (ty.isBottom) app >> "Bot"
    else
      predTys
        .foldLeft(FilterApp(app)) {
          case (app, (pred, name)) =>
            app.add({ ty --= pred; name }, pred <= ty)
        }
        .add(ty.clo.map(s => s"\"$s\""), !ty.clo.isBottom, "Clo")
        .add(ty.cont, !ty.cont.isBottom, "Cont")
        .add(ty.record, !ty.record.isBottom)
        .add(ty.map, !ty.map.isBottom)
        .add(ty.list, !ty.list.isBottom)
        .add(ty.ast, !ty.ast.isBottom)
        .add(
          ty.grammarSymbol.map(_.toString),
          !ty.grammarSymbol.isBottom,
          "GrammarSymbol",
        )
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
        .app

  /** list types */
  given listTyRule: Rule[ListTy] = (app, ty) =>
    import ListTy.*
    ty match
      case Top => app >> "List"
      case Bot => app >> ""
      case Elem(elem) =>
        if (elem.isBottom) app >> "Nil"
        else app >> "List[" >> elem >> "]"

  /** record types */
  given recordTyRule: Rule[RecordTy] = (app, ty) =>
    import RecordTy.*
    given Rule[(String, FieldMap)] = {
      case (app, (name, fm)) =>
        app >> name
        if (!fm.isTop)
          if (name.nonEmpty) app >> " "
          app >> fm
        app
    }
    given Rule[Iterable[String]] = iterableRule(sep = OR)
    given Rule[List[(String, FieldMap)]] = iterableRule(sep = OR)
    ty match
      case Top => app >> "Record"
      case Elem(map) =>
        var m = map
        var prevExists = false
        map.get("Normal").map { fm =>
          if (prevExists) app >> OR
          prevExists = true
          m -= "Normal"
          app >> "Normal"
          if (fm.map.keySet == Set("Value")) app >> "[" >> fm("Value") >> "]"
          else if (!fm.isTop) app >> " " >> fm
        }
        map.get("Abrupt").map { fm =>
          if (prevExists) app >> OR
          prevExists = true
          m -= "Abrupt"
          app >> "Abrupt"
          if (fm.map.keySet == Set("Type")) app >> fm("Type").value.enumv
          else if (!fm.isTop) app >> " " >> fm
        }
        if (m.nonEmpty)
          if (prevExists) app >> OR
          app >> "Record[" >> m.toList.sortBy(_._1) >> "]"
        else app

  /** AST value types */
  given astTyRule: Rule[AstTy] = (app, ty) =>
    import AstTy.*
    given Rule[Set[String]] = setRule("[", OR, "]")
    app >> "Ast"
    ty match
      case Top           => app
      case Simple(names) => app >> names
      case Detail(x, i)  => app >> "[" >> x >> "[" >> i >> "]" >> "]"

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
    import MapTy.*
    ty match
      case Top              => app >> "Map"
      case Bot              => app >> ""
      case Elem(key, value) => app >> "Map[" >> key >> " -> " >> value >> "]"

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
