package esmeta.analyzer.domain

import esmeta.analyzer.AnalyzerElem
import esmeta.cfg.Func
import esmeta.interp.*
import esmeta.ir.Expr
import esmeta.spec.TypeInfo
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
        case _                      => TopT
    case NameT("Object") => ESValueT
    case NameT(name) =>
      cfg.typeModel.infos.get(name) match
        case Some(TypeInfo(Some(parent), _, _)) => NameT(parent)
        case _                                  => TopT
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
    case TopT             => error("no parent")
    case _                => TopT
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
  def typeName: Option[String] = optional(this match
    case NameT(name) if cfg.typeModel.subType(name, "Object") => "Object"
    case SymbolT                                              => "Symbol"
    case NumberT | NumberSingleT(_)                           => "Number"
    case BigIntT | BigIntSingleT(_)                           => "BigInt"
    case StrT | StrSingleT(_)                                 => "String"
    case BoolT | BoolSingleT(_)                               => "Boolean"
    case UndefT                                               => "Undefined"
    case NullT                                                => "Null"
    case _ => error("no type name"),
  )

  /** wrap completion */
  def wrapCompletion: Type = this match
    case t: PureType => NormalT(t)
    case _           => this

  /** upcast */
  def upcast: Type = this match
    case NormalT(t)  => NormalT(t.upcast)
    case p: PureType => p.upcast
    case _           => this

  /** to pure type */
  def toPureType: PureType = this match
    case p: PureType => p
    case _           => error(s"not pure type: $this")

  /** various type conditions */
  def isNumeric = this match
    case NumberT | NumberSingleT(_) => true
    case MathT | MathSingleT(_)     => true
    case BigIntT | BigIntSingleT(_) => true
    case _                          => false
  def isStr = this match
    case StrT | StrSingleT(_) => true
    case _                    => false
  def isBool = this match
    case BoolT | BoolSingleT(_) => true
    case _                      => false
  def isUndef = this == UndefT
  def isMath = this match
    case MathT | MathSingleT(_) => true
    case _                      => false
  def isNumber = this match
    case NumberT | NumberSingleT(_) => true
    case _                          => false
  def isBigInt = this match
    case BigIntT | BigIntSingleT(_) => true
    case _                          => false
  def isCompletion = this match
    case NormalT(_) | AbruptT => true
    case _                    => false
  def isPure = this match
    case p: PureType => true
    case _           => false
}

/** top type */
case object TopT extends Type

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
case class NameT(name: String) extends PureType

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

/** closure types */
case class CloT(func: Func) extends PureType with SingleT

/** TODO continuation types */

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
    case AClo(func, _)      => CloT(func) // XXX captured?
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
