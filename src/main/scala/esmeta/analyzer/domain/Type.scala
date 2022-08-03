package esmeta.analyzer.domain

import esmeta.analyzer.{AnalyzerElem, NodePoint}
import esmeta.cfg.{Func, Node}
import esmeta.interp.*
import esmeta.ir.Expr
import esmeta.spec.{Type => SType, *}
import esmeta.util.BaseUtils.*
import esmeta.util.DoubleEquals
import scala.annotation.tailrec

/** types */
sealed trait Type extends AnalyzerElem {
  import Type.*

  /** get ancestor types */
  def ancestors: Set[Type] = parent.map(_.ancestors).getOrElse(Set()) + this
  def strictAncestors: Set[Type] = parent.map(_.ancestors).getOrElse(Set())
  def ancestorList: List[Type] =
    this :: parent.map(_.ancestorList).getOrElse(List())

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
    case MathSingleT(_)         => MathT
    case PrimT                  => ESValueT
    case ArithT                 => PrimT
    case NumericT               => ArithT
    case NumberT                => NumericT
    case BigIntT                => NumericT
    case StrT                   => ArithT
    case BoolT                  => PrimT
    case SymbolT                => PrimT
    case NumberSingleT(n)       => NumberT
    case BigIntSingleT(b)       => BigIntT
    case StrSingleT(str)        => StrT
    case BoolSingleT(b)         => BoolT
    case UndefT                 => PrimT
    case NullT                  => PrimT
    case SyntacticT(name, _, _) => AstT(name)
    case AstT(_)                => AstTopT
    case TopT                   => error("no parent")
    case CloT(_)                => CloTopT
    case _                      => TopT
  })

  /** check sub typing */
  @tailrec
  final def <(that: Type): Boolean =
    if (this == that) true
    else
      parent match
        case Some(parent) => parent < that
        case None         => false

  /** lowest common ancestor */
  def lca(that: Type): Type =
    @tailrec
    def aux(l0: List[Type], l1: List[Type]): Type = (l0, l1) match
      case (_, Nil) | (Nil, _) => error("lowest common ancestor")
      case (h0 :: t0, h1 :: t1) =>
        if (h0 == h1) h0
        else if (t1 contains h0) h0
        else if (t0 contains h1) h1
        else aux(t0, t1)
    aux(ancestorList, that.ancestorList)

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

  /** get name of base types */
  def typeName: Option[String] = optional(this match
    case NameT(name) if cfg.typeModel.isSubType(name, "Object") => "Object"
    case SymbolT                                                => "Symbol"
    case NumberT | NumberSingleT(_)                             => "Number"
    case BigIntT | BigIntSingleT(_)                             => "BigInt"
    case StrT | StrSingleT(_)                                   => "String"
    case BoolT | BoolSingleT(_)                                 => "Boolean"
    case UndefT                                                 => "Undefined"
    case NullT                                                  => "Null"
    case _ => error("no type name"),
  )

  /** instance name */
  def instanceNameSet: Set[String] = this match
    // case CloT(_)              => Set("AbstractClosure")
    // case AbruptT | NormalT(_) => Set("CompletionRecord")
    // case ConstT(_)            => Set("Constant")
    case NameT(name) =>
      cfg.typeModel.subTypes.getOrElse(name, Set(name)) ++
      ancestors.collect { case NameT(name) => name }
    case AstTopT => astChildMap.keySet ++ Set("ParseNode", "Nonterminal")
    case ast: AstTBase =>
      val astName = ast.name
      var set: Set[String] = Set("ParseNode")
      if (!cfg.grammar.lexicalNames.contains(astName)) set += "Nonterminal"
      set ++= astChildMap.getOrElse(astName, Set(astName))
      set
    case _ => Set("")

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

  /** various conditions for type */
  def isNumeric = this match
    case NumberT | NumberSingleT(_) => true
    case MathT | MathSingleT(_)     => true
    case BigIntT | BigIntSingleT(_) => true
    case _                          => false
  def isStr = this match
    case StrT | StrSingleT(_) | CodeUnitT => true
    case _                                => false
  def isBool = this match
    case BoolT | BoolSingleT(_) => true
    case _                      => false
  def isUndef = this == UndefT
  def isMath = this match
    case MathT | MathSingleT(_) | NumberSingleT(Double.PositiveInfinity) |
        NumberSingleT(Double.NegativeInfinity) =>
      true
    case _ => false
  def isNumber = this match
    case NumberT | NumberSingleT(_) => true
    case _                          => false
  def isBigInt = this match
    case BigIntT | BigIntSingleT(_) => true
    case _                          => false
  def isCompletion = this match
    case NormalT(_) | AbruptT => true
    case _                    => false
  def isObj = this match
    case NilT | ListT(_) | MapT(_) | SymbolT | NameT(_) => true
    case _                                              => false
  def isNamedObj = this match
    case NameT(_) => true
    case _        => false
  def isPure = this match
    case p: PureType => true
    case _           => false
  def isAst = this match
    case _: AstTBase | AstTopT => true
    case _                     => false
  def isList = this match
    case NilT | ListT(_) => true
    case _               => false
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

/** record types */
case class RecordT(props: Map[String, Set[Type]]) extends PureType {

  /** lookup properties */
  def apply(prop: String): Set[Type] = props.getOrElse(prop, Set(AbsentT))

  /** merge record types */
  def ⊔(that: RecordT): RecordT = {
    val keys = this.props.keySet ++ that.props.keySet
    RecordT(keys.toList.map(k => k -> (this(k) union that(k))).toMap)
  }
}
object RecordT {
  def apply(pairs: (String, Set[Type])*): RecordT = RecordT(pairs.toMap)
}

/** list types */
case object NilT extends PureType with SingleT
case class ListT(elem: Type) extends PureType

/** sub mapping types */
case class MapT(elem: PureType) extends PureType

/** symbol types */
case object SymbolT extends PureType

/** closure types */
case object CloTopT extends PureType
case class CloT(fname: String) extends PureType

/** continuation types */
case class ContT(target: NodePoint[Node]) extends PureType

/** AST types */
case object AstTopT extends PureType
sealed trait AstTBase extends PureType { val name: String }
case class AstT(name: String) extends AstTBase
case class SyntacticT(name: String, idx: Int, subIdx: Int) extends AstTBase

/** grammar types */
case class GrammarT(name: String) extends PureType with SingleT

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
    case AClo(func, _)      => CloT(func.name) // TODO captured
    case ACont(target, _)   => ContT(target) // TODO captured
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

  /** ast type check helper */
  lazy val astDirectChildMap: Map[String, Set[String]] =
    (cfg.grammar.prods.map {
      case Production(lhs, _, _, rhsList) =>
        val name = lhs.name
        val subs = rhsList.collect {
          case Rhs(_, List(Nonterminal(name, _, _)), _) => name
        }.toSet
        name -> subs
    }).toMap
  lazy val astChildMap: Map[String, Set[String]] = {
    var descs = Map[String, Set[String]]()
    def aux(name: String): Set[String] = descs.get(name) match {
      case Some(set) => set
      case None =>
        val set = (for {
          sub <- astDirectChildMap.getOrElse(name, Set())
          elem <- aux(sub)
        } yield elem) + name
        descs += name -> set
        set
    }
    cfg.grammar.prods.foreach(prod => aux(prod.name))
    descs
  }
}
