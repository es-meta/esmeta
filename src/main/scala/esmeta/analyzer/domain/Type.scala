package esmeta.analyzer.domain

import esmeta.analyzer.AnalyzerElem
import esmeta.interp.*
import esmeta.ir.Expr
import esmeta.util.BaseUtils.*
import esmeta.util.DoubleEquals
import scala.annotation.tailrec

/** types */
sealed trait Type extends AnalyzerElem {
  import Type.*

  /** get ancestor types */
  def ancestors: Set[Type] = parent.map(_.ancestors).getOrElse(Set()) + this
  def strictAncestors: Set[Type] = parent.map(_.ancestors).getOrElse(Set())

  /** get parent types */
  def parent: Option[Type] = optional(this match {
    case NormalT(t) =>
      t.parent match
        case Some(parent: PureType) => NormalT(parent)
        case _                      => error("no parent")
    // TODO type model
    // case NameT("Object") => ESValueT
    // case NameT(name) =>
    //   infoMap.get(name) match {
    //     case Some(Info(_, Some(parent), _)) => NameT(parent)
    //     case _                              => error("no parent")
    //   }
    case MathSingleT(_)   => MathT
    case PrimT            => ESValueT
    case ArithT           => PrimT
    case NumericT         => ArithT
    case NumberT          => NumericT
    case BigIntT          => NumericT
    case StrT             => ArithT
    case BoolT            => PrimT
    case SymbolT          => PrimT
    case NumberSingleT(n) => NumberT
    case BigIntSingleT(b) => BigIntT
    case StrSingleT(str)  => StrT
    case BoolSingleT(b)   => BoolT
    case UndefT           => PrimT
    case NullT            => PrimT
    case _                => error("no parent")
  })

  /** check sub typing */
  @tailrec
  final def <(that: Type): Boolean =
    if (this == that) true
    else
      parent match
        case Some(parent) => parent < that
        case None         => false

  /** remove types */
  def -(that: Type): Set[Type] =
    if (this < that) Set()
    else
      typeAliasMap.get(this) match
        case Some(set) if set contains that => set - that
        case _                              => Set(this)

  /** get base types */
  def bases: Set[Type] = baseMap.getOrElse(this, Set(this))

  /** get type names */
  def typeNameSet: Set[String] = for {
    x <- bases
    y <- x.typeName
  } yield y

  /* get name of base types */
  def typeName: Option[String] = this match
    case NameT(name) if cfg.typeModel.subType(name, "Object") => Some("Object")
    case SymbolT                                              => Some("Symbol")
    case NumberT | NumberSingleT(_)                           => Some("Number")
    case BigIntT | BigIntSingleT(_)                           => Some("BigInt")
    case StrT | StrSingleT(_)                                 => Some("String")
    case BoolT | BoolSingleT(_)                               => Some("Boolean")
    case UndefT => Some("Undefined")
    case NullT  => Some("Null")
    case _      => None

  /** wrap completion */
  def wrapCompletion: Type = this match
    case t: PureType => NormalT(t)
    case _           => this

  /** upcast */
  def upcast: Type = this match
    case NormalT(t)  => NormalT(t.upcast)
    case p: PureType => p.upcast
    case _           => this
}

/** completion types */
sealed trait CompType extends Type
case class NormalT(value: PureType) extends CompType
case object AbruptT extends CompType

/** pure types */
sealed trait PureType extends Type {

  /** upcast */
  override def upcast: PureType = this match
    case ListT(t)         => ListT(t.upcast)
    case MapT(t)          => MapT(t.upcast)
    case MathSingleT(_)   => MathT
    case NumberSingleT(_) => NumberT
    case BigIntSingleT(_) => BigIntT
    case StrSingleT(_)    => StrT
    case BoolSingleT(_)   => BoolT
    case _                => this
}

/** ECMAScript value types */
case object ESValueT extends PureType

/** norminal types */
case class NameT(name: String) extends PureType {
  // // lookup properties
  // def apply(prop: String): AbsType =
  //   Type.propMap.getOrElse(name, Map()).getOrElse(prop, Absent)
}

/** TODO record types */
// case class RecordT(props: Map[String, AbsType]) extends PureType
// // record types
// case class RecordT(props: Map[String, AbsType]) extends PureType {
//   // lookup properties
//   def apply(prop: String): AbsType = props.getOrElse(prop, Absent)
//
//   // merge record types
//   def ⊔(that: RecordT): RecordT = {
//     val keys = this.props.keySet ++ that.props.keySet
//     RecordT(keys.toList.map(k => k -> (this(k) ⊔ that(k))).toMap)
//   }
// }
// object RecordT {
//   // constructor
//   def apply(pairs: (String, AbsType)*): RecordT = RecordT(pairs.toMap)
// }

/** list types */
case object NilT extends PureType with SingleT
case class ListT(elem: PureType) extends PureType

/** sub mapping types */
case class MapT(elem: PureType) extends PureType

/** symbol types */
case object SymbolT extends PureType

// TODO
/** closure types */
// case class CloT(fid: Int, captured: Map[Name, AbsValue]) extends PureType with SingleT
/** continuation types */

/** AST types */
sealed trait AstTBase extends PureType { val name: String }
case class AstT(name: String) extends AstTBase
case class SyntacticT(name: String, idx: Int, subIdx: Int) extends AstTBase

/** grammar types */
case class GrammarT(name: String) extends PureType

/** code unit types */
case object CodeUnitT extends PureType

/** math types */
case object MathT extends PureType

/** singleton types */
sealed trait SingleT extends PureType
// case class MathSingleT(n: BigDecimal) extends SingleT
// case class CodeUnitSingleT(c: Char) extends PureType with SingleT
case class ConstT(name: String) extends PureType with SingleT
case class MathSingleT(n: BigDecimal) extends PureType with SingleT

/** primitive types */
case object PrimT extends PureType
case object ArithT extends PureType
case object NumericT extends PureType
case object NumberT extends PureType
case object BigIntT extends PureType
case object StrT extends PureType
case object BoolT extends PureType

/** singleton primitive types */
case class NumberSingleT(n: Double) extends SingleT with DoubleEquals(n)
case class BigIntSingleT(bigint: scala.math.BigInt) extends SingleT
case class StrSingleT(str: String) extends SingleT
case class BoolSingleT(bool: Boolean) extends SingleT
case object UndefT extends SingleT
case object NullT extends SingleT
case object AbsentT extends SingleT

/** modeling */
object Type {

  /** type aliases */
  val typeAlias: List[(Type, Set[Type])] = List(
    BoolT -> Set[Type](BoolSingleT(true), BoolSingleT(false)),
    NumericT -> Set[Type](NumberT, BigIntT),
    ArithT -> Set[Type](NumericT, StrT),
    PrimT -> Set[Type](NullT, UndefT, BoolT, ArithT, SymbolT),
    ESValueT -> Set[Type](NameT("Object"), PrimT),
  )
  val typeAliasMap: Map[Type, Set[Type]] = typeAlias.toMap
  val baseMap: Map[Type, Set[Type]] = {
    var map = Map[Type, Set[Type]]()
    for ((t, set) <- typeAlias) map += t -> set.flatMap(x => {
      map.get(x).getOrElse(Set(x))
    })
    map
  }

  /** conversion to type */
  def from(av: AValue): Type = av match
    case AComp(AConst("normal"), v, _) => NormalT(fromPure(v))
    case _: AComp                      => AbruptT
    case _                             => fromPure(av)
  def fromPure(av: AValue): PureType = av match
    case clo: AClo          => ??? // TODO
    case cont: ACont        => ??? // TODO
    case AAst(ast)          => AstT(ast.name)
    case AGrammar(name, _)  => GrammarT(name)
    case ACodeUnit(_)       => CodeUnitT
    case AConst(name)       => ConstT(name)
    case AMath(n)           => MathSingleT(n)
    case ASimple(Number(n)) => NumberSingleT(n)
    case ASimple(BigInt(n)) => BigIntSingleT(n)
    case ASimple(Str(n))    => StrSingleT(n)
    case ASimple(Bool(n))   => BoolSingleT(n)
    case ASimple(Undef)     => UndefT
    case ASimple(Null)      => NullT
    case ASimple(Absent)    => AbsentT
    case _ => error(s"impossible to convert to pure type ($av)")

  // abstraction
  // val abs: Type => AbsType = cached(AbsType(_))

  // // ////////////////////////////////////////////////////////////////////////////
  // // Type Information
  // // ////////////////////////////////////////////////////////////////////////////
  // case class Info(
  //   name: String,
  //   parent: Option[String],
  //   lazyProps: () => Map[String, AbsType],
  // ) { lazy val props: Map[String, AbsType] = lazyProps() }

  // // constructors
  // def I(name: String, parent: String, props: => Map[String, AbsType]): Info =
  //   Info(name, Some(parent), () => props)
  // def I(name: String, props: => Map[String, AbsType]): Info =
  //   Info(name, None, () => props)

  // // property map
  // type PropMap = Map[String, AbsType]

  // // get type information
  // lazy val infos: List[Info] = TypeModel.infos

  // // type info map
  // lazy val infoMap: Map[String, Info] =
  //   infos.map(info => info.name -> info).toMap

  // // direct subtypes
  // lazy val subTypes: Map[String, Set[String]] = {
  //   var children = Map[String, Set[String]]()
  //   for {
  //     info <- infos
  //     parent <- info.parent
  //     set = children.getOrElse(parent, Set())
  //   } children += parent -> (set + info.name)
  //   children
  // }

  // // recursive subtypes
  // lazy val recSubTypes: Map[String, Set[String]] = {
  //   var descs = Map[String, Set[String]]()
  //   def aux(name: String): Set[String] = descs.get(name) match {
  //     case Some(set) => set
  //     case None =>
  //       val set = (for {
  //         sub <- subTypes.getOrElse(name, Set())
  //         elem <- aux(sub)
  //       } yield elem) + name
  //       descs += name -> set
  //       set
  //   }
  //   infos.collect { case Info(name, None, _) => aux(name) }
  //   descs
  // }

  // // property map
  // lazy val propMap: Map[String, PropMap] =
  //   infos.map(info => info.name -> getPropMap(info.name)).toMap

  // // ////////////////////////////////////////////////////////////////////////////
  // // Private Helper Functions
  // // ////////////////////////////////////////////////////////////////////////////
  // // get property map
  // private def getPropMap(name: String): PropMap = {
  //   val upper = getUpperPropMap(name)
  //   val lower = getLowerPropMap(name)
  //   lower.foldLeft(upper) {
  //     case (map, (k, t)) =>
  //       val newT = t ⊔ map.getOrElse(k, AbsType.Bot)
  //       map + (k -> newT)
  //   }
  // }

  // // get property map from ancestors
  // private def getUpperPropMap(name: String): PropMap = infoMap.get(name) match {
  //   case Some(info) =>
  //     val parentProps = info.parent.map(getUpperPropMap).getOrElse(Map())
  //     val props = info.props
  //     weakMerge(parentProps, props)
  //   case None => Map()
  // }

  // // get property map of name
  // private def getSamePropMap(name: String): PropMap =
  //   infoMap.get(name).map(_.props).getOrElse(Map())

  // // get property map from ancestors
  // private def getLowerPropMap(name: String): PropMap =
  //   subTypes.get(name) match {
  //     case Some(children) =>
  //       children
  //         .map(child => {
  //           val lower = getLowerPropMap(child)
  //           val props = getSamePropMap(child)
  //           weakMerge(lower, props)
  //         })
  //         .reduce(parallelWeakMerge)
  //     case None => getSamePropMap(name)
  //   }

  // // weak merge
  // private def weakMerge(lmap: PropMap, rmap: PropMap): PropMap = {
  //   val keys = lmap.keySet ++ rmap.keySet
  //   keys.toList
  //     .map(k => {
  //       val lt = lmap.getOrElse(k, AbsType.Bot)
  //       val rt = rmap.getOrElse(k, AbsType.Bot)
  //       k -> (lt ⊔ rt)
  //     })
  //     .toMap
  // }

  // // parallel weak merge
  // private def parallelWeakMerge(lmap: PropMap, rmap: PropMap): PropMap = {
  //   val keys = lmap.keySet ++ rmap.keySet
  //   keys.toList
  //     .map(k => {
  //       k -> (lmap.getOrElse(k, Absent.abs) ⊔ rmap.getOrElse(k, Absent.abs))
  //     })
  //     .toMap
  // }
}
