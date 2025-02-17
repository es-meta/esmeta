package esmeta.analyzer.es

import esmeta.domain.{*, given}, BSet.*, Flat.*
import esmeta.es.*
import esmeta.state.{Null => StNull, Undef => StUndef, *}
import esmeta.util.*
import esmeta.util.Appender.{*, given}
import esmeta.util.BaseUtils.*

/** abstract primitive values */
trait AbsPrimValueDecl { self: ESAnalyzer =>

  /** abstract primitive values */
  case class AbsPrimValue(
    ast: Flat[Ast] = Zero,
    grammarSymbol: Set[GrammarSymbol] = Set.empty,
    math: BSet[Math] = BSet.Bot,
    infinity: Flat[Boolean] = Zero,
    enumv: Set[String] = Set.empty,
    codeUnit: Flat[Char] = Zero,
    number: Flat[Double] = Zero,
    bigint: Flat[scala.BigInt] = Zero,
    str: BSet[String] = BSet.Bot,
    bool: Flat[Boolean] = Zero,
    undef: Boolean = false,
    nullv: Boolean = false,
  ) extends Printable[AbsPrimValue]

  object AbsPrimValue
    extends Lattice[AbsPrimValue]
    with AbsDomain[PrimValue, AbsPrimValue] {

    /** top element */
    lazy val Top: AbsPrimValue = exploded("top abstract primitive value")

    /** bottom element */
    lazy val Bot: AbsPrimValue = AbsPrimValue()

    /** useful abstract values */
    lazy val Null = AbsPrimValue(nullv = true)
    lazy val Undef = AbsPrimValue(undef = true)
    lazy val True = AbsPrimValue(bool = Flat(true))
    lazy val False = AbsPrimValue(bool = Flat(false))
    lazy val BoolTop = AbsPrimValue(bool = Many)
    lazy val StrTop = AbsPrimValue(str = Inf)
    lazy val NonNegInt = AbsPrimValue(math = Inf)
    lazy val MathTop = AbsPrimValue(math = Inf)
    lazy val NumberTop = AbsPrimValue(number = Many)
    lazy val BigIntTop = AbsPrimValue(bigint = Many)

    // abstraction functions
    override def alpha(vs: Iterable[PrimValue]) = {
      var _ast: Flat[Ast] = Zero
      var _grammarSymbol: Set[GrammarSymbol] = Set.empty
      var _math: BSet[Math] = BSet.Bot
      var _infinity: Flat[Boolean] = Zero
      var _enumv: Set[String] = Set.empty
      var _codeUnit: Flat[Char] = Zero
      var _number: Flat[Double] = Zero
      var _bigint: Flat[scala.BigInt] = Zero
      var _str: BSet[String] = BSet.Bot
      var _bool: Flat[Boolean] = Zero
      var _undef: Boolean = false
      var _nullv: Boolean = false
      vs.map {
        case AstValue(ast)    => _ast += ast
        case g: GrammarSymbol => _grammarSymbol += g
        case m: Math          => _math += m
        case Infinity(b)      => _infinity += b
        case Enum(name)       => _enumv += name
        case CodeUnit(c)      => _codeUnit += c
        case Number(n)        => _number += n
        case BigInt(n)        => _bigint += n
        case Str(s)           => _str += s
        case Bool(b)          => _bool += b
        case StUndef          => _undef = true
        case StNull           => _nullv = true
      }
      AbsPrimValue(
        ast = _ast,
        grammarSymbol = _grammarSymbol,
        math = _math,
        infinity = _infinity,
        enumv = _enumv,
        codeUnit = _codeUnit,
        number = _number,
        bigint = _bigint,
        str = _str,
        bool = _bool,
        undef = _undef,
        nullv = _nullv,
      )
    }

    inline def apply(ast: Ast): AbsPrimValue =
      AbsPrimValue(ast = Flat(ast))
    inline def apply(n: Double): AbsPrimValue =
      AbsPrimValue(number = Flat(n))
    inline def apply(s: String): AbsPrimValue =
      AbsPrimValue(str = BSet(s))
    inline def apply(b: Boolean): AbsPrimValue =
      AbsPrimValue(bool = Flat(b))
    inline def apply(d: BigDecimal): AbsPrimValue =
      AbsPrimValue(math = BSet(Math(d)))
    inline def apply(i: scala.BigInt): AbsPrimValue =
      AbsPrimValue(bigint = Flat(i))
  }

  given Lattice.Ops[AbsPrimValue] with
    import AbsPrimValue.*
    extension (x: AbsPrimValue) {
      def isTop: Boolean = false
      def isBottom: Boolean = x == Bot
      def ⊑(y: AbsPrimValue): Boolean =
        (x.ast ⊑ y.ast) &&
        (x.grammarSymbol ⊑ y.grammarSymbol) &&
        (x.math ⊑ y.math) &&
        (x.infinity ⊑ y.infinity) &&
        (x.enumv ⊑ y.enumv) &&
        (x.codeUnit ⊑ y.codeUnit) &&
        (x.number ⊑ y.number) &&
        (x.bigint ⊑ y.bigint) &&
        (x.str ⊑ y.str) &&
        (x.bool ⊑ y.bool) &&
        (x.undef ⊑ y.undef) &&
        (x.nullv ⊑ y.nullv)
      def ⊔(y: AbsPrimValue): AbsPrimValue = AbsPrimValue(
        ast = x.ast ⊔ y.ast,
        grammarSymbol = x.grammarSymbol ⊔ y.grammarSymbol,
        math = x.math ⊔ y.math,
        infinity = x.infinity ⊔ y.infinity,
        enumv = x.enumv ⊔ y.enumv,
        codeUnit = x.codeUnit ⊔ y.codeUnit,
        number = x.number ⊔ y.number,
        bigint = x.bigint ⊔ y.bigint,
        str = x.str ⊔ y.str,
        bool = x.bool ⊔ y.bool,
        undef = x.undef ⊔ y.undef,
        nullv = x.nullv ⊔ y.nullv,
      )
      def ⊓(y: AbsPrimValue): AbsPrimValue = AbsPrimValue(
        ast = x.ast ⊓ y.ast,
        grammarSymbol = x.grammarSymbol ⊓ y.grammarSymbol,
        math = x.math ⊓ y.math,
        infinity = x.infinity ⊓ y.infinity,
        enumv = x.enumv ⊓ y.enumv,
        codeUnit = x.codeUnit ⊓ y.codeUnit,
        number = x.number ⊓ y.number,
        bigint = x.bigint ⊓ y.bigint,
        str = x.str ⊓ y.str,
        bool = x.bool ⊓ y.bool,
        undef = x.undef ⊓ y.undef,
        nullv = x.nullv ⊓ y.nullv,
      )
    }

  given AbsDomain.GenericOps[PrimValue, AbsPrimValue] with
    extension (x: AbsPrimValue) {
      def contains(value: PrimValue): Boolean = ???
      def toBSet: BSet[PrimValue] = ???
      def toFlat: Flat[PrimValue] = ???
    }

  given Rule[AbsPrimValue] = (app, elem) => {
    import esStringifier.given, stateStringifier.given
    given Ordering[PrimValue] = Ordering.by(_.toString)
    if (elem.isBottom) app >> "⊥"
    else {
      val AbsPrimValue(
        ast,
        grammarSymbol,
        math,
        infinity,
        enumv,
        codeUnit,
        number,
        bigint,
        str,
        bool,
        undef,
        nullv,
      ) = elem
      var first = true
      def add[T: Rule: Lattice.Ops](
        x: T,
        name: String,
      ): Unit = if (x.nonBottom) {
        if (first) first = false else app >> " | "
        if (x.isTop) app >> name else app >> x
      }
      given [T: Rule]: Rule[Set[T]] = setRule("", " | ", "")
      given [T: Rule]: Rule[BSet[T]] = bsetRule("", " | ", "")
      add(ast, "Ast")
      add(grammarSymbol, "GrammarSymbol")
      add(math, "Math")
      add(infinity, "Infinity")
      add(enumv, "Enum")
      add(codeUnit, "CodeUnit")
      add(number, "Number")
      add(bigint, "BigInt")
      add(str.map("\"" + _ + "\""), "String")
      add(bool, "Boolean")
      add(undef, "undefined")
      add(nullv, "null")
      app
    }
  }
}
