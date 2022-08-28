package esmeta.ty

import esmeta.ty.util.Parser

/** types */
trait Ty extends TyElem {

  /** completion check */
  def isCompletion: Boolean
}
object Ty extends Parser.From[Ty](Parser.ty)

// -----------------------------------------------------------------------------
// legacy types
// -----------------------------------------------------------------------------
/** top type */
// case object TopT extends Ty
//
// /** completion types */
// sealed trait CompTy extends Ty
// case class NormalT(value: PureTy) extends CompTy
// case object AbruptT extends CompTy
//
// /** pure types */
// sealed trait PureTy extends Ty
//
// /** ECMAScript value types */
// case object ESValueT extends PureTy
//
// /** norminal types */
// case class NameT(override val name: String) extends PureTy
//
// /** record types */
// case class RecordT(props: Map[String, Set[Ty]]) extends PureTy
// object RecordT:
//   def apply(pairs: (String, Set[Ty])*): RecordT = RecordT(pairs.toMap)
//
// /** list types */
// case object NilT extends PureTy with SingleT
// case class ListT(elem: Ty) extends PureTy
//
// /** sub mapping types */
// case class MapT(elem: PureTy) extends PureTy
//
// /** symbol types */
// case object SymbolT extends PureTy
//
// /** closure types */
// case object CloTopT extends PureTy
// case class CloT(
//   func: Func,
//   caputred: Map[Name, Ty],
// ) extends PureTy
//
// /** continuation types */
// case class ContT(
//   func: Func,
//   caputred: Map[Name, Ty],
//   target: Node,
// ) extends PureTy
//
// /** AST types */
// case object AstTopT extends PureTy
// sealed trait AstTBase extends PureTy
// case class AstT(override val name: String) extends AstTBase
// case class SyntacticT(override val name: String, idx: Int, subIdx: Int)
//   extends AstTBase
//
// /** grammar types */
// case class GrammarT(override val name: String) extends PureTy with SingleT
//
// /** code unit types */
// case object CodeUnitT extends PureTy
//
// /** math types */
// case object MathT extends PureTy
//
// /** singleton types */
// sealed trait SingleT extends PureTy
// // case class MathSingleT(n: BigDecimal) extends SingleT
// // case class CodeUnitSingleT(c: Char) extends PureTy with SingleT
// case class ConstT(override val name: String) extends PureTy with SingleT
// case class MathSingleT(n: BigDecimal) extends PureTy with SingleT
//
// /** primitive types */
// case object PrimT extends PureTy
// case object ArithT extends PureTy
// case object NumericT extends PureTy
// case object NumberT extends PureTy
// case object BigIntT extends PureTy
// case object StrT extends PureTy
// case object BoolT extends PureTy
//
// /** singleton primitive types */
// case class NumberSingleT(n: Double) extends SingleT with DoubleEquals(n)
// case class BigIntSingleT(bigInt: scala.math.BigInt) extends SingleT
// case class StrSingleT(str: String) extends SingleT
// case class BoolSingleT(bool: Boolean) extends SingleT
// case object UndefT extends SingleT
// case object NullT extends SingleT
// case object AbsentT extends SingleT
// case object UndefT extends SingleT
// case object NullT extends SingleT
// case object AbsentT extends SingleT
