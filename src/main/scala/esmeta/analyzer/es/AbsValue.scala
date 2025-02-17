package esmeta.analyzer.es

import esmeta.cfg.*
import esmeta.interpreter.Interpreter
import esmeta.ir.{Func => _, *}
import esmeta.error.*
import esmeta.es.*
import esmeta.state.*
import esmeta.ty.*
import esmeta.util.*
import esmeta.util.Appender.{*, given}
import esmeta.util.BaseUtils.*
import esmeta.domain.{*, given}

/** abstract values */
trait AbsValueDecl { self: ESAnalyzer =>

  /** abstract values */
  case class AbsValue(
    addr: AbsAddr = AbsAddr.Bot,
    clo: AbsClo = AbsClo.Bot,
    cont: AbsCont = AbsCont.Bot,
    prim: AbsPrimValue = AbsPrimValue.Bot,
  ) extends Printable[AbsValue] {
    import AbsValue.*

    /** convert to optional abstract value */
    inline def opt: AbsValue.opt = AbsOpt(this, false)

    /** getter of specific type */
    inline def ast: Flat[Ast] = prim.ast
    inline def grammarSymbol: Set[GrammarSymbol] = prim.grammarSymbol
    inline def math: BSet[Math] = prim.math
    inline def infinity: Flat[Boolean] = prim.infinity
    inline def enumv: Set[String] = prim.enumv
    inline def codeUnit: Flat[Char] = prim.codeUnit
    inline def number: Flat[Double] = prim.number
    inline def bigint: Flat[scala.BigInt] = prim.bigint
    inline def str: BSet[String] = prim.str
    inline def bool: Flat[Boolean] = prim.bool
    inline def undef: Boolean = prim.undef
    inline def nullv: Boolean = prim.nullv

    /** get reachable address partitions */
    def reachableParts: Set[AddrPart] = {
      var parts = addr
      for {
        AClo(_, captured) <- clo
        v <- captured.values
      } parts ++= v.reachableParts
      for {
        ACont(_, captured) <- cont
        v <- captured.values
      } parts ++= v.reachableParts
      parts
    }

    /** get lexical result */
    def getLexical(method: String): AbsValue = ???

    /** get syntactic SDO */
    def getSdo(method: String): List[(Func, AbsValue)] = ???

    /** parse strings with a rule */
    def parse(rule: AbsValue): AbsValue = ???

    /** substring operation */
    def substring(from: AbsValue): AbsValue = ???

    /** substring operation */
    def substring(from: AbsValue, to: AbsValue): AbsValue = ???

    /** trim operation */
    def trim(isStarting: Boolean): AbsValue = ???

    /** instanceof operation */
    def instanceOf(ty: AbsValue): AbsValue = ???

    /** sizeof operation */
    def sizeOf: AbsValue = ???

    /** helper functions for abstract transfer */
    def convertTo(cop: COp, radix: AbsValue): AbsValue = ???

    /** bitwise operations */
    def &(that: AbsValue)(using AbsState): AbsValue = ???
    def |(that: AbsValue)(using AbsState): AbsValue = ???
    def ^(that: AbsValue)(using AbsState): AbsValue = ???

    /** comparison operations */
    def =^=(that: AbsValue)(using AbsState): AbsValue = ???
    def ==^==(that: AbsValue)(using AbsState): AbsValue = ???
    def <(that: AbsValue)(using AbsState): AbsValue = ???

    /** logical operations */
    def &&(that: AbsValue)(using AbsState): AbsValue = ???
    def ||(that: AbsValue)(using AbsState): AbsValue = ???
    def ^^(that: AbsValue)(using AbsState): AbsValue = ???

    /** numeric operations */
    def +(that: AbsValue)(using AbsState): AbsValue = ???
    def sub(that: AbsValue)(using AbsState): AbsValue = ???
    def /(that: AbsValue)(using AbsState): AbsValue = ???
    def *(that: AbsValue)(using AbsState): AbsValue = ???
    def %(that: AbsValue)(using AbsState): AbsValue = ???
    def %%(that: AbsValue)(using AbsState): AbsValue = ???
    def **(that: AbsValue)(using AbsState): AbsValue = ???
    def <<(that: AbsValue)(using AbsState): AbsValue = ???
    def >>(that: AbsValue)(using AbsState): AbsValue = ???
    def >>>(that: AbsValue)(using AbsState): AbsValue = ???

    /** unary negation operation */
    def unary_-(using AbsState): AbsValue = AbsValue(prim =
      AbsPrimValue(
        math = math.map(-_),
        number = number.map(-_),
        bigint = bigint.map(-_),
        infinity = infinity.map(!_),
      ),
    )

    /** unary logical negation operation */
    def unary_!(using AbsState): AbsValue =
      AbsValue(prim = AbsPrimValue(bool = bool.map(!_)))

    /** unary bitwise negation operation */
    def unary_~(using AbsState): AbsValue = AbsValue(prim =
      AbsPrimValue(
        math = math.map(n => ~n),
        number = number.map(n => (~(n.toInt)).toDouble),
        bigint = bigint.map(~_),
      ),
    )

    /** absolute operation */
    def abs(using AbsState): AbsValue =
      AbsValue(prim = AbsPrimValue(math = math.map(Interpreter.abs)))

    /** floor operation */
    def floor(using AbsState): AbsValue =
      AbsValue(prim = AbsPrimValue(math = math.map(Interpreter.floor)))

    /** type operations */
    def typeOf(using AbsState): AbsValue = ???

  }
  object AbsValue
    extends ValueDomain
    with Lattice[AbsValue]
    with AbsDomain[Value, AbsValue] {

    /** abstract optional values */
    lazy val opt = OptDomain(AbsValue)
    type opt = AbsOpt[AbsValue]

    /** top element */
    lazy val Top: AbsValue = exploded("top abstract value")

    /** bottom element */
    lazy val Bot: AbsValue = AbsValue()

    /** useful abstract values */
    lazy val Null = AbsValue(prim = AbsPrimValue.Null)
    lazy val Undef = AbsValue(prim = AbsPrimValue.Undef)
    lazy val True = AbsValue(prim = AbsPrimValue.True)
    lazy val False = AbsValue(prim = AbsPrimValue.False)
    lazy val BoolTop = AbsValue(prim = AbsPrimValue.BoolTop)
    lazy val StrTop = AbsValue(prim = AbsPrimValue.StrTop)
    lazy val NonNegInt = AbsValue(prim = AbsPrimValue.NonNegInt)
    lazy val MathTop = AbsValue(prim = AbsPrimValue.MathTop)
    lazy val NumberTop = AbsValue(prim = AbsPrimValue.NumberTop)
    lazy val BigIntTop = AbsValue(prim = AbsPrimValue.BigIntTop)

    // abstraction functions
    def alpha(vs: Iterable[Value]): AbsValue = {
      var addrs = Set[Addr]()
      var clos = Set[AClo]()
      var conts = Set[ACont]()
      var prims = Set[PrimValue]()
      vs.map {
        case addr: NamedAddr => addrs += addr
        case clo: Clo        => clos += AClo(clo)
        case prim: PrimValue => prims += prim
        case v               => throw InvalidAbstraction(v)
      }
      AbsValue(
        addr = AbsAddr(addrs.map(AddrPart(_))),
        clo = clos,
        cont = conts,
        prim = AbsPrimValue(prims),
      )
    }
    inline def apply(part: AddrPart): AbsValue = AbsValue(addr = AbsAddr(part))
    inline def apply(clo: AClo): AbsValue = AbsValue(clo = AbsClo(clo))
    inline def apply(cont: ACont): AbsValue = AbsValue(cont = AbsCont(cont))
    inline def apply(ast: Ast): AbsValue = AbsValue(prim = AbsPrimValue(ast))
    inline def apply(n: Double): AbsValue = AbsValue(prim = AbsPrimValue(n))
    inline def apply(n: scala.BigInt): AbsValue =
      AbsValue(prim = AbsPrimValue(n))
    inline def apply(s: String): AbsValue = AbsValue(prim = AbsPrimValue(s))
    inline def apply(b: Boolean): AbsValue = AbsValue(prim = AbsPrimValue(b))
    inline def apply(d: BigDecimal): AbsValue = AbsValue(prim = AbsPrimValue(d))

    extension (value: AbsValue) {
      def getString(st: AbsState): String =
        val app = new Appender
        app >> value
        val parts = value.reachableParts
        if (parts.nonEmpty)
          (app >> " @ ").wrap(for (part <- parts.toList.sorted) {
            app :> part >> " -> " >> st.heap(part)
          })
        app.toString
    }
  }

  given Lattice[AbsValue] = AbsValue

  given Rule[AbsValue] = (app, elem) => {
    if (elem.isBottom) app >> "⊥"
    else {
      val AbsValue(addr, clo, cont, prim) = elem
      var first = true
      def add[T: Rule: Lattice.Ops](x: T): Unit = if (x.nonBottom) {
        if (first) { first = false; app >> x }
        else app >> " | " >> x
      }
      add(addr); add(clo); add(cont); add(prim)
      app
    }
  }

  given Lattice.Ops[AbsValue] with {
    import AbsValue.*
    extension (x: AbsValue) {
      def isTop: Boolean = false
      def isBottom: Boolean = x == Bot
      def ⊑(y: AbsValue): Boolean =
        (x.addr ⊑ y.addr) &&
        (x.clo ⊑ y.clo) &&
        (x.cont ⊑ y.cont) &&
        (x.prim ⊑ y.prim)
      def ⊔(y: AbsValue): AbsValue = AbsValue(
        x.addr ⊔ y.addr,
        x.clo ⊔ y.clo,
        x.cont ⊔ y.cont,
        x.prim ⊔ y.prim,
      )
      def ⊓(y: AbsValue): AbsValue = AbsValue(
        x.addr ⊓ y.addr,
        x.clo ⊓ y.clo,
        x.cont ⊓ y.cont,
        x.prim ⊓ y.prim,
      )
    }
  }

  given AbsDomain.GenericOps[Value, AbsValue] with {
    extension (x: AbsValue) {
      def contains(value: Value): Boolean = ???
      def toBSet: BSet[Value] = ???
      def toFlat: Flat[Value] = ???
    }
  }
}
