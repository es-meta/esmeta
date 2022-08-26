package esmeta.analyzer.domain.obj

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.parser.ESValueParser
import esmeta.state.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.error

/** basic domain for objects */
object BasicDomain extends obj.Domain {

  /** elements */
  trait Elem extends Appendable

  /** top element */
  lazy val Top: Elem = exploded("top abstract object")

  /** bottom element */
  case object Bot extends Elem

  /** symbol elements */
  case class SymbolElem(desc: AbsValue) extends Elem

  /** map elements */
  sealed trait MapElem extends Elem:
    /** map types */
    val tname: String

    /** merged properties */
    def mergedProp: AbsValue = this match
      case MergedMap(_, prop, _) => prop
      case PropMap(_, map, _)    => AbsValue(map.keys)

    /** merged values */
    def mergedValue: AbsValue = this match
      case MergedMap(_, _, value) => value
      case PropMap(_, map, _)     => map.values.foldLeft(AbsValue.Bot)(_ ⊔ _)

  /** merged map elements */
  case class MergedMap(
    tname: String,
    prop: AbsValue,
    value: AbsValue,
  ) extends MapElem

  /** property map elements with optional property orders */
  case class PropMap(
    tname: String,
    map: Map[AValue, AbsValue],
    order: PropOrder,
  ) extends MapElem

  /** property orders */
  type PropOrder = Option[Vector[AValue]]
  extension (elem: PropOrder) {
    def ⊑(that: PropOrder): Boolean = (elem, that) match
      case (Some(l), Some(r)) if l == r => true
      case _                            => that == None
    def ⊔(that: PropOrder): PropOrder = (elem, that) match
      case (Some(l), Some(r)) if l == r => elem
      case _                            => None
  }

  /** lists */
  sealed trait ListElem extends Elem:
    /** merged value of all possible values */
    def mergedValue: AbsValue = this match
      case MergedList(value)   => value
      case KeyWiseList(values) => values.foldLeft(AbsValue.Bot)(_ ⊔ _)

  /** merged lists */
  case class MergedList(value: AbsValue) extends ListElem

  /** key-wise lists */
  case class KeyWiseList(values: Vector[AbsValue]) extends ListElem

  /** not supported objects */
  case class NotSupportedElem(tname: String, desc: String) extends Elem

  /** get list with abstact values */
  def getList(values: Iterable[AbsValue]): Elem = KeyWiseList(values.toVector)

  /** get list with a merged abstact value */
  def getMergedList(value: AbsValue): Elem = MergedList(value)

  /** get symbol with abstract description value */
  def getSymbol(desc: AbsValue): Elem = SymbolElem(desc)

  /** abstraction functions */
  def alpha(obj: Obj): Elem = obj match
    case SymbolObj(desc) => SymbolElem(AbsValue(desc))
    case m @ MapObj(tname, props, size) =>
      PropMap(
        tname = tname,
        map = (for {
          (k, propV) <- props
        } yield AValue.from(k) -> AbsValue(propV.value)).toMap,
        order = Some(m.keys.map(AValue.from)),
      )
    case ListObj(values)     => KeyWiseList(values.map(AbsValue(_)))
    case YetObj(tname, desc) => NotSupportedElem(tname, desc)

  /** abstraction functions for a single concrete object */
  def alpha(xs: Iterable[Obj]): Elem = xs.map(alpha).foldLeft[Elem](Bot)(_ ⊔ _)

  /** appender */
  given rule: Rule[Elem] = (app, elem) =>
    elem match
      case Bot              => app >> "⊥"
      case SymbolElem(desc) => app >> "'" >> desc.toString
      case MergedMap(tname, prop, value) =>
        app >> s"$tname "
        app >> "{{" >> prop.toString >> " -> " >> value.toString >> "}}"
      case PropMap(tname, map, order) =>
        app >> tname >> " "
        if (!map.isEmpty) app.wrap {
          order match
            case Some(order) =>
              for {
                (k, i) <- order.zipWithIndex
              } app :> s"[$i] $k -> " >> map(k)
            case None =>
              for {
                (k, v) <- map
              } app :> s"$k -> " >> v
        }
        else app >> "{}"
      case MergedList(value) =>
        app >> "[[" >> value.toString >> "]]"
      case KeyWiseList(values) =>
        app >> values.mkString("[", ", ", "]")
      case NotSupportedElem(tname, desc) =>
        app >> s"???[$tname](" >> desc >> ")"

  /** object element interfaces */
  extension (elem: Elem) {

    /** partial order */
    def ⊑(that: Elem): Boolean = (elem, that) match
      case (Bot, _)                               => true
      case (_, Bot)                               => false
      case (SymbolElem(ldesc), SymbolElem(rdesc)) => ldesc ⊑ rdesc
      case (PropMap(ltname, lmap, lorder), PropMap(rtname, rmap, rorder)) =>
        ltname == rtname &&
        lorder ⊑ rorder &&
        (lmap.keys ++ rmap.keys).forall(x => elem(x) ⊑ that(x))
      case (l: MapElem, r: MapElem) =>
        l.tname == r.tname &&
        l.mergedProp ⊑ r.mergedProp &&
        l.mergedValue ⊑ r.mergedValue
      case (KeyWiseList(lvs), KeyWiseList(rvs)) =>
        lvs.length == rvs.length &&
        (lvs zip rvs).forall { case (l, r) => l ⊑ r }
      case (l: ListElem, r: ListElem) =>
        l.mergedValue ⊑ r.mergedValue
      case (NotSupportedElem(ltname, ld), NotSupportedElem(rtname, rd)) =>
        ltname == rtname && ld == rd
      case _ => false

    /** join operator */
    def ⊔(that: Elem): Elem = (elem, that) match
      case (Bot, _)                               => that
      case (_, Bot)                               => elem
      case _ if elem ⊑ that                       => that
      case _ if that ⊑ elem                       => elem
      case (SymbolElem(ldesc), SymbolElem(rdesc)) => SymbolElem(ldesc ⊔ rdesc)
      case (
            PropMap(ltname, lmap, lorder),
            PropMap(rtname, rmap, rorder),
          ) if ltname == rtname =>
        PropMap(
          tname = ltname,
          map = (lmap.keys ++ rmap.keys).toList
            .map(x => x -> (elem(x) ⊔ that(x)))
            .toMap,
          order = lorder ⊔ rorder,
        )
      case (l: MapElem, r: MapElem) if l.tname == r.tname =>
        MergedMap(
          tname = l.tname,
          prop = l.mergedProp ⊔ r.mergedProp,
          value = l.mergedValue ⊔ r.mergedValue,
        )
      case (l @ KeyWiseList(lvs), r @ KeyWiseList(rvs)) =>
        if (lvs.length == rvs.length) {
          KeyWiseList((lvs zip rvs).map { case (l, r) => l ⊔ r })
        } else MergedList(l.mergedValue ⊔ r.mergedValue)
      case (l: ListElem, r: ListElem) =>
        MergedList(l.mergedValue ⊔ r.mergedValue)
      case (
            NotSupportedElem(lty, ld),
            NotSupportedElem(rty, rd),
          ) if lty == rty && ld == rd =>
        elem
      case _ =>
        exploded(s"cannot merge: ${elem.getTy} with ${that.getTy}")

    /** lookup */
    def apply(key: AValue): AbsValue = elem match
      case Bot => AbsValue.Bot
      case SymbolElem(desc) =>
        key match
          case Str("Description") => desc
          case _                  => AbsValue.Bot
      case MergedMap(_, prop, value) =>
        if (AbsValue(key) ⊑ prop) value
        else AbsValue.absentTop
      case m: PropMap        => m.map.getOrElse(key, AbsValue.absentTop)
      case MergedList(value) => value
      case KeyWiseList(values) =>
        key match
          case Math(math) =>
            val idx = math.toInt
            if (0 <= idx && idx < values.length) values(idx)
            else AbsValue.absentTop
          case Str("length") =>
            AbsValue(Math(BigDecimal.exact(values.length)))
          case _ => AbsValue.Bot
      case NotSupportedElem(_, desc) => AbsValue.Bot

    /** lookup */
    def get(akey: AbsValue): AbsValue = akey.getSingle match
      case Zero     => AbsValue.Bot
      case One(key) => elem(key)
      case Many =>
        elem match
          case Bot                       => AbsValue.Bot
          case SymbolElem(desc)          => desc
          case m: MapElem                => m.mergedValue
          case l: ListElem               => l.mergedValue
          case NotSupportedElem(_, desc) => AbsValue.Bot

    /** get list with abstact values */
    def getList: Option[Vector[AbsValue]] = elem match
      case KeyWiseList(vs) => Some(vs)
      case _               => None

    /** get type */
    def getTy: String = elem match
      case Bot                        => ""
      case SymbolElem(desc)           => "Symbol"
      case m: MapElem                 => m.tname
      case MergedList(value)          => "List"
      case KeyWiseList(values)        => "List"
      case NotSupportedElem(ty, desc) => ty

    /** singleton checks */
    def isSingle: Boolean = elem match
      case SymbolElem(desc)          => desc.isSingle
      case PropMap(_, map, Some(_))  => map.forall { case (_, v) => v.isSingle }
      case KeyWiseList(values)       => values.forall(_.isSingle)
      case NotSupportedElem(_, desc) => true
      case _                         => false

    /** get reachable address partitions */
    def reachableParts: Set[Part] = elem match
      case SymbolElem(desc) =>
        desc.reachableParts
      case MergedMap(_, prop, value) =>
        prop.reachableParts ++ value.reachableParts
      case m: PropMap =>
        m.map.keySet.collect { case p: Part => p }
        ++ m.map.values.flatMap(_.reachableParts).toSet
      case MergedList(value) =>
        value.reachableParts
      case KeyWiseList(values) =>
        values.foldLeft(Set[Part]())(_ ++ _.reachableParts)
      case _ =>
        Set()

    /** updates */
    def update(prop: AbsValue, value: AbsValue, weak: Boolean): Elem =
      def aux(key: AValue): MapUpdater = _ match {
        case MergedMap(t, p, v) => MergedMap(t, p ⊔ prop, v ⊔ value)
        case PropMap(ty, map, order) =>
          val newOrder = order match
            case Some(order) if !map.contains(key) => Some(order :+ key)
            case _                                 => order
          PropMap(ty, map + (key -> value), newOrder)
      }
      def mergedAux: MapUpdater = m =>
        MergedMap(
          m.tname,
          m.mergedProp ⊔ prop,
          m.mergedValue ⊔ value,
        )
      modifyMap(elem, prop, aux, mergedAux, aux, mergedAux, weak)

    /** delete */
    def delete(prop: AbsValue, weak: Boolean): Elem =
      def aux(key: AValue): MapUpdater = _ match {
        case PropMap(ty, map, order) =>
          val newOrder = order match
            case Some(order) if map contains key => Some(order.filter(_ != key))
            case _                               => order
          PropMap(ty, map - key, newOrder)
        case m => m
      }
      def mergedAux: MapUpdater = m =>
        MergedMap(
          m.tname,
          m.mergedProp,
          m.mergedValue,
        )
      modifyMap(elem, prop, aux, mergedAux, aux, mergedAux, weak)

    /** concat */
    def concat(list: AbsObj, weak: Boolean): Elem = list match
      case MergedList(value) =>
        modifyList(elem, x => x, _ ⊔ value, true)
      case list @ KeyWiseList(values) =>
        modifyList(elem, _ ++ values, _ ⊔ list.mergedValue, weak)
      case _ => Top

    /** duplicated element check */
    def duplicated: AbsBool = elem match
      case _: MergedList => AB
      case KeyWiseList(vs) if vs.forall(_.isSingle) =>
        val values = vs.map(_.getSingle).flatMap {
          case One(v) => Some(v)
          case _      => None
        }
        AbsBool(Bool(values.toSet.size != values.size))
      case _: KeyWiseList => AB
      case _              => AbsBool.Bot

    /** appends */
    def append(value: AbsValue, weak: Boolean): Elem =
      modifyList(elem, _ :+ value, _ ⊔ value, weak)

    /** prepends */
    def prepend(value: AbsValue, weak: Boolean): Elem =
      modifyList(elem, value +: _, _ ⊔ value, weak)

    /** remove */
    def remove(value: AbsValue, weak: Boolean): Elem =
      modifyList(elem, _.filter(v => v != value), _ ⊔ value, weak)

    /** pops */
    def pop(weak: Boolean, front: Boolean): (AbsValue, Elem) = elem match
      case l: ListElem =>
        var v: AbsValue = AbsValue.Bot
        val newObj =
          modifyList(
            elem,
            vs => {
              v = if (front) vs.head else vs.last
              if (front) vs.drop(1) else vs.dropRight(1)
            },
            mv => { v = mv; mv },
            weak,
          )
        (v, newObj)
      case _ => (AbsValue.Bot, Bot)

    /** keys of map */
    def keys(intSorted: Boolean): Elem = elem match
      case MergedMap(_, prop, _) => MergedList(prop)
      case PropMap(tname, map, Some(props)) =>
        KeyWiseList(if (intSorted) {
          (for {
            Str(s) <- props
            d = ESValueParser.str2Number(s)
            if toStringHelper(d) == s
            i = d.toLong
            if d == i
          } yield (s, i))
            .sortBy(_._2)
            .map { case (s, _) => AbsValue(Str(s)) }
        } else if (tname == "SubMap") {
          props.map(AbsValue(_))
        } else props.sortBy(_.toString).map(AbsValue(_)))
      case _ => Bot

    /** set type of objects */
    def setType(tname: String): Elem = elem match
      case MergedMap(_, prop, value) => MergedMap(tname, prop, value)
      case PropMap(_, map, props)    => PropMap(tname, map, props)
      case _ => error("cannot set type of non-map abstract objects.")

    /** check contains */
    def contains(value: AbsValue): AbsValue = (elem, value.getSingle) match
      case (Bot, _) | (_, Zero) => AbsValue.Bot
      case (KeyWiseList(values), One(_)) =>
        if (values contains value) AVT
        else if (values.forall(v => (v ⊓ value).isBottom)) AVF
        else AVB
      case (MergedList(mergedValue), _) =>
        if ((mergedValue ⊓ value).isBottom) AVF
        else AVB
      case _ => AbsValue.Bot

    /** find merged parts */
    def findMerged(
      part: Part,
      path: String,
      aux: (AbsValue, String, String) => Unit,
    ): Unit = elem match
      case Bot =>
      case SymbolElem(desc) =>
        aux(desc, s"$path.desc", s"$part.desc")
      case PropMap(_, map, Some(_)) =>
        for ((p, v) <- map) {
          aux(v, s"$path[$p]", s"$part[$p]")
        }
      case KeyWiseList(values) =>
        for ((v, k) <- values.zipWithIndex) {
          aux(v, s"$path[$k]", s"$part[$k]")
        }
      case NotSupportedElem(_, _) =>
      case obj => println(s"$path ($part) is merged object: $obj")
  }

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  // helper for map structures
  private type MapUpdater = MapElem => MapElem
  private def modifyMap(
    elem: Elem,
    prop: AbsValue,
    esF: AValue => MapUpdater,
    esMergedF: MapUpdater,
    f: AValue => MapUpdater,
    mergedF: MapUpdater,
    weak: Boolean,
  ): Elem = elem match
    // for ECMAScript
    case map @ MergedMap("SubMap", _, _) =>
      esMergedF(map)
    case map @ PropMap("SubMap", _, Some(_)) =>
      prop.keyValue.getSingle match
        case Zero              => elem
        case One(key) if !weak => esF(key)(map)
        case _                 => esMergedF(map)
    // for IR
    case map @ MergedMap(ty, _, _) =>
      mergedF(map)
    case map @ PropMap(ty, _, Some(_)) =>
      prop.keyValue.getSingle match
        case Zero              => elem
        case One(key) if !weak => f(key)(map)
        case _                 => mergedF(map)
    case _ => elem

  // helper for map structures
  private type ListUpdater = Vector[AbsValue] => Vector[AbsValue]
  private def modifyList(
    elem: Elem,
    f: ListUpdater,
    mergedF: AbsValue => AbsValue,
    weak: Boolean,
  ): Elem = elem match
    case l @ MergedList(value) => MergedList(mergedF(value))
    case l @ KeyWiseList(values) =>
      if (weak) MergedList(mergedF(l.mergedValue))
      else KeyWiseList(f(values))
    case _ => Bot
}
