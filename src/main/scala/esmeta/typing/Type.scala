package esmeta.typing

import esmeta.cfg.{Func, Node}
import esmeta.state.*
import esmeta.ir.{Name, Expr}
import esmeta.spec.{Type => SType, *}
import esmeta.util.BaseUtils.*
import esmeta.util.DoubleEquals
import scala.annotation.tailrec

/** types */
sealed trait Type

/** top type */
case object TopT extends Type

/** completion types */
sealed trait CompType extends Type
case class NormalT(value: PureType) extends CompType
case object AbruptT extends CompType

/** pure types */
sealed trait PureType extends Type

/** ECMAScript value types */
case object ESValueT extends PureType

/** norminal types */
case class NameT(name: String) extends PureType

/** record types */
case class RecordT(props: Map[String, Set[Type]]) extends PureType
object RecordT:
  def apply(pairs: (String, Set[Type])*): RecordT = RecordT(pairs.toMap)

/** list types */
case object NilT extends PureType with SingleT
case class ListT(elem: Type) extends PureType

/** sub mapping types */
case class MapT(elem: PureType) extends PureType

/** symbol types */
case object SymbolT extends PureType

/** closure types */
case object CloTopT extends PureType
case class CloT(
  func: Func,
  caputred: Map[Name, Type],
) extends PureType

/** continuation types */
case class ContT(
  func: Func,
  caputred: Map[Name, Type],
  target: Node,
) extends PureType

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
case class BigIntSingleT(bigInt: scala.math.BigInt) extends SingleT
case class StrSingleT(str: String) extends SingleT
case class BoolSingleT(bool: Boolean) extends SingleT
case object UndefT extends SingleT
case object NullT extends SingleT
case object AbsentT extends SingleT
