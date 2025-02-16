package esmeta.analyzer.es

import esmeta.state.*
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*
import esmeta.domain.{*, given}, BSet.*, Flat.*
import scala.collection.immutable.VectorMap

/** abstract heaps */
trait AbsObjDecl { self: ESAnalyzer =>

  /** abstract objects */
  sealed trait AbsObj extends Printable[AbsObj] {
    import AbsObj.*, AbsValue.opt.Absent

    def apply(key: AbsValue): AbsValue = this match {
      case record: AbsRecord =>
        key.str.toFlat.to(record(_).value)
      case map: AbsMap =>
        key.str.toFlat.to(map(_)) ⊔ key.addr.toFlat.to(map(_))
      case AbsList(vs) =>
        ???
      case _ =>
        ???
    }

    def update(
      key: AbsValue,
      value: AbsValue,
      weak: Boolean,
    ): AbsObj =
      val v = if (weak) apply(key) ⊔ value else value
      this match {
        case AbsRecord(tname, fs) =>
          key.str.toFlat.to[AbsObj](s =>
            AbsRecord(tname, fs + (s -> Binding(v))),
          )
        case AbsMap(m)   => ???
        case AbsList(vs) => ???
        case _           => Bot
      }
  }

  /** abstract record */
  case class AbsRecord(
    baseTyName: String,
    fields: Map[String, Binding],
  ) extends AbsObj {
    def apply(key: String): Binding = fields.getOrElse(key, Binding.Bot)
  }

  /** abstract ordered map */
  case class AbsMap(
    map: VectorMap[AddrPart | String, AbsValue],
  ) extends AbsObj {
    def apply(key: String | AddrPart): AbsValue =
      map.getOrElse(key, AbsValue.Bot)
  }

  /** abstract list */
  case class AbsList(values: Vector[AbsValue]) extends AbsObj {
    def apply(key: Math): AbsValue =
      values.lift(key.decimal.toInt).getOrElse(AbsValue.Bot)
  }

  /** abstract object bottom */
  case object AbsObjBot extends AbsObj

  object AbsObj extends Lattice[AbsObj] with AbsDomain[Obj, AbsObj] {
    import ManualInfo.tyModel

    /** top element */
    lazy val Top: AbsObj = exploded("top abstract heap")

    /** bottom element */
    lazy val Bot: AbsObj = AbsObjBot

    /** abstraction */
    def alpha(elems: Iterable[Obj]): AbsObj = elems.foldLeft(Bot)(_ ⊔ alpha(_))

    /** abstraction */
    def alpha(obj: Obj): AbsObj = obj match {
      case RecordObj(tname, map) =>
        AbsRecord(
          tyModel.baseOf(tname),
          map.map {
            case (k, Uninit)   => k -> Binding.Uninit
            case (k, v: Value) => k -> Binding(AbsValue(v))
          }.toMap,
        )
      case MapObj(map) =>
        AbsMap(VectorMap.from(map.collect {
          case (Str(s), v)  => s -> AbsValue(v)
          case (a: Addr, v) => AddrPart(a) -> AbsValue(v)
        }))
      case ListObj(values)    => AbsList(values.map(AbsValue(_)))
      case YetObj(tname, msg) => Bot
    }
  }

  given Lattice[AbsObj] = AbsObj

  given Lattice.Ops[AbsObj] with
    import AbsObj.*, AbsValue.opt.Absent
    extension (x: AbsObj) {

      def isTop: Boolean = false

      def isBottom: Boolean = x == Bot

      def ⊑(y: AbsObj): Boolean = (x, y) match
        case (Bot, _) => true
        case (_, Bot) => false
        case (AbsRecord(lt, lfs), AbsRecord(rt, rfs)) =>
          lt == rt &&
          lfs.forall { (k, lv) => rfs.get(k).fold(false)(lv ⊑ _) }
        case _ => ???

      def ⊔(y: AbsObj): AbsObj = (x, y) match
        case (Bot, _) => y
        case (_, Bot) => x
        case (l @ AbsRecord(lt, lfs), r @ AbsRecord(rt, rfs)) =>
          if (lt == rt) {
            val fs = rfs.keys.foldLeft(lfs) {
              case (fs, k) => fs + (k -> l(k) ⊔ r(k))
            }
            AbsRecord(lt, fs)
          } else Top
        case _ => ???

      def ⊓(y: AbsObj): AbsObj = (x, y) match
        case (Bot, _) => Bot
        case (_, Bot) => Bot
        case (AbsRecord(lt, lfs), AbsRecord(rt, rfs)) =>
          if (lt == rt) {
            val fs = (for {
              k <- lfs.keySet intersect rfs.keySet
              lv = lfs(k) ⊓ rfs(k)
              if lv != AbsValue.opt.Absent
            } yield k -> lv).toMap
            AbsRecord(lt, fs)
          } else Top
        case _ => ???
    }

  given AbsDomain.GenericOps[Obj, AbsObj] with
    extension (x: AbsObj) {
      def contains(value: Obj): Boolean = ???
      def toBSet: BSet[Obj] = ???
      def toFlat: Flat[Obj] = ???
    }

  given rule: Rule[AbsObj] = (app, elem) => {
    elem match
      case AbsRecord(tname, fs) =>
        app >> "Record"
        if (tname != "") app >> "[" >> tname >> "]"
        given Rule[Map[String, Binding]] = sortedMapRule("{", " : ", "}")
        app >> " " >> fs
      case AbsMap(m) =>
        ???
      case AbsList(values) =>
        given Rule[Iterable[AbsValue]] = iterableRule("[", ", ", "]")
        app >> values
      case AbsObjBot =>
        app >> "⊥"
  }
}
