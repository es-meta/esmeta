package esmeta.analyzer.domain

import esmeta.interp.*
import esmeta.js.Ast
import esmeta.util.Appender
import esmeta.util.Appender.*

/** basic abstract values */
object BasicValueDomain extends Domain {
  val Bot = Elem(
    comp = AbsComp.Bot,
    clo = AbsClo.Bot,
    cont = AbsCont.Bot,
    loc = AbsLoc.Bot,
    ast = AbsAst.Bot,
    grammar = AbsGrammar.Bot,
    codeunit = AbsCodeUnit.Bot,
    const = AbsConst.Bot,
    math = AbsMath.Bot,
    simple = AbsSimple.Bot,
  )

  // abstraction functions
  def apply(ast: Ast): Elem = Bot.copy(ast = AbsAst(AAst(ast)))
  def apply(num: Number): Elem = Bot.copy(simple = AbsSimple(num))
  def apply(num: Double): Elem = Bot.copy(simple = AbsSimple(num))
  def apply(bigint: BigInt): Elem = Bot.copy(simple = AbsSimple(bigint))
  def apply(str: String): Elem = Bot.copy(simple = AbsSimple(str))
  def apply(bool: Boolean): Elem = Bot.copy(simple = AbsSimple(bool))
  def apply(d: BigDecimal): Elem = Bot.copy(math = AbsMath(AMath(d)))
  lazy val num: Elem = Bot.copy(simple = AbsSimple.num)
  lazy val bigint: Elem = Bot.copy(simple = AbsSimple.bigint)
  lazy val str: Elem = Bot.copy(simple = AbsSimple.str)
  lazy val bool: Elem = Bot.copy(simple = AbsSimple.bool)
  lazy val undef: Elem = Bot.copy(simple = AbsSimple.undef)
  lazy val nullv: Elem = Bot.copy(simple = AbsSimple.nullv)
  lazy val absent: Elem = Bot.copy(simple = AbsSimple.absent)
  def apply(value: Value): Elem = this(AValue(value))
  def apply(value: AValue): Elem = value match
    case (comp: AComp)       => Bot.copy(comp = AbsComp(comp))
    case (clo: AClo)         => Bot.copy(clo = AbsClo(clo))
    case (cont: ACont)       => Bot.copy(cont = AbsCont(cont))
    case (loc: Loc)          => Bot.copy(loc = AbsLoc(loc))
    case (ast: AAst)         => Bot.copy(ast = AbsAst(ast))
    case (grammar: AGrammar) => Bot.copy(grammar = AbsGrammar(grammar))
    case (cu: ACodeUnit)     => Bot.copy(codeunit = AbsCodeUnit(cu))
    case (const: AConst)     => Bot.copy(const = AbsConst(const))
    case (math: AMath)       => Bot.copy(math = AbsMath(math))
    case (simple: ASimple)   => Bot.copy(simple = AbsSimple(simple))

  // constructors
  def apply(
    comp: AbsComp = AbsComp.Bot,
    clo: AbsClo = AbsClo.Bot,
    cont: AbsCont = AbsCont.Bot,
    loc: AbsLoc = AbsLoc.Bot,
    ast: AbsAst = AbsAst.Bot,
    grammar: AbsGrammar = AbsGrammar.Bot,
    codeunit: AbsCodeUnit = AbsCodeUnit.Bot,
    const: AbsConst = AbsConst.Bot,
    math: AbsMath = AbsMath.Bot,
    simple: AbsSimple = AbsSimple.Bot,
    num: AbsNum = AbsNum.Bot,
    bigint: AbsBigInt = AbsBigInt.Bot,
    str: AbsStr = AbsStr.Bot,
    bool: AbsBool = AbsBool.Bot,
    undef: AbsUndef = AbsUndef.Bot,
    nullv: AbsNull = AbsNull.Bot,
    absent: AbsAbsent = AbsAbsent.Bot,
  ): Elem = {
    val newSimple = AbsSimple(num, bigint, str, bool, undef, nullv, absent)
    Elem(
      comp,
      clo,
      cont,
      loc,
      ast,
      grammar,
      codeunit,
      const,
      math,
      simple ⊔ newSimple,
    )
  }

  // extractors
  def unapply(elem: Elem) = Some(
    (
      elem.comp,
      elem.clo,
      elem.cont,
      elem.loc,
      elem.ast,
      elem.grammar,
      elem.codeunit,
      elem.const,
      elem.math,
      elem.simple,
    ),
  )

  // appender
  given rule: Rule[Elem] = (app, elem) => {
    if (elem.isBottom) app >> "⊥"
    else {
      val Elem(
        comp,
        clo,
        cont,
        loc,
        ast,
        grammar,
        codeunit,
        const,
        math,
        simple,
      ) = elem
      var strs = Vector[String]()
      if (!comp.isBottom) strs :+= comp.toString
      if (!clo.isBottom) strs :+= clo.toString
      if (!cont.isBottom) strs :+= cont.toString
      if (!ast.isBottom) strs :+= ast.toString
      if (!grammar.isBottom) strs :+= grammar.toString
      if (!codeunit.isBottom) strs :+= codeunit.toString
      if (!const.isBottom) strs :+= const.toString
      if (!math.isBottom) strs :+= math.toString
      if (!simple.isBottom) strs :+= simple.toString
      app >> strs.mkString(", ")
    }
  }

  // elements
  case class Elem(
    comp: AbsComp,
    clo: AbsClo,
    cont: AbsCont,
    loc: AbsLoc,
    ast: AbsAst,
    grammar: AbsGrammar,
    codeunit: AbsCodeUnit,
    const: AbsConst,
    math: AbsMath,
    simple: AbsSimple,
  ) extends ElemTrait {
    // getters
    def num: AbsNum = simple.num
    def bigint: AbsBigInt = simple.bigint
    def str: AbsStr = simple.str
    def bool: AbsBool = simple.bool
    def undef: AbsUndef = simple.undef
    def nullv: AbsNull = simple.nullv
    def absent: AbsAbsent = simple.absent
    def pure: AbsValue = copy(comp = AbsComp.Bot)

    // partial order
    def ⊑(that: Elem): Boolean = (
      this.comp ⊑ that.comp &&
        this.clo ⊑ that.clo &&
        this.cont ⊑ that.cont &&
        this.loc ⊑ that.loc &&
        this.ast ⊑ that.ast &&
        this.grammar ⊑ that.grammar &&
        this.codeunit ⊑ that.codeunit &&
        this.const ⊑ that.const &&
        this.math ⊑ that.math &&
        this.simple ⊑ that.simple
    )

    // join operator
    def ⊔(that: Elem): Elem = Elem(
      this.comp ⊔ that.comp,
      this.clo ⊔ that.clo,
      this.cont ⊔ that.cont,
      this.loc ⊔ that.loc,
      this.ast ⊔ that.ast,
      this.grammar ⊔ that.grammar,
      this.codeunit ⊔ that.codeunit,
      this.const ⊔ that.const,
      this.math ⊔ that.math,
      this.simple ⊔ that.simple,
    )

    // meet operator
    def ⊓(that: Elem): Elem = Elem(
      this.comp ⊓ that.comp,
      this.clo ⊓ that.clo,
      this.cont ⊓ that.cont,
      this.loc ⊓ that.loc,
      this.ast ⊓ that.ast,
      this.grammar ⊓ that.grammar,
      this.codeunit ⊓ that.codeunit,
      this.const ⊓ that.const,
      this.math ⊓ that.math,
      this.simple ⊓ that.simple,
    )

    // get single value
    def getSingle: Flat[AValue] = (
      this.comp.getSingle ⊔
        this.clo.getSingle ⊔
        this.cont.getSingle ⊔
        this.loc.getSingle ⊔
        this.ast.getSingle ⊔
        this.grammar.getSingle ⊔
        this.codeunit.getSingle ⊔
        this.const.getSingle ⊔
        this.math.getSingle ⊔
        this.simple.getSingle
    )

    // get reachable locations
    def reachableLocs: Set[Loc] = {
      var locs = loc.toSet
      for ((_, AbsComp.Result(value, target)) <- comp.map) {
        locs ++= value.reachableLocs
        locs ++= target.reachableLocs
      }
      for {
        AClo(_, captured) <- clo
        (_, value) <- captured
      } locs ++= value.reachableLocs
      for {
        ACont(_, captured) <- cont
        (_, value) <- captured
      } locs ++= value.reachableLocs
      locs
    }

    // remove absent values
    def removeAbsent: Elem = copy(simple = simple.removeAbsent)

    // escape completion
    def escaped: Elem = comp.normal.value ⊔ copy(comp = AbsComp.Bot)

    // only values usable as keys
    def keyValue: AbsValue = AbsValue(loc = loc, str = str)

    // singleton checks
    def isSingle: Boolean = getSingle match
      case FlatElem(_) => true
      case _           => false

    // check completion
    def isCompletion: AbsBool =
      var b: AbsBool = AbsBool.Bot
      if (!comp.isBottom) b ⊔= AT
      if (!pure.isBottom) b ⊔= AF
      b

    // abstract equality
    def =^=(that: AbsValue): AbsBool = (this.getSingle, that.getSingle) match
      case (FlatBot, _) | (_, FlatBot) => AbsBool.Bot
      case (FlatElem(l), FlatElem(r))  => AbsBool(Bool(l == r))
      case _                           => if ((this ⊓ that).isBottom) AF else AB

    // check abrupt completion
    def isAbruptCompletion: AbsBool =
      var b: AbsBool = AbsBool.Bot
      if (!comp.removeNormal.isBottom) b ⊔= AT
      if (!comp.normal.isBottom || !pure.isBottom) b ⊔= AF
      b

    // wrap completion
    def wrapCompletion: Elem = wrapCompletion("normal")
    def wrapCompletion(ty: String): Elem = AbsValue(comp = {
      if (pure.isBottom) comp
      else comp ⊔ AbsComp(ty -> AbsComp.Result(pure, AbsValue(CONST_EMPTY)))
    })

    // check absents
    def isAbsent: AbsBool =
      var b: AbsBool = AbsBool.Bot
      if (!absent.isBottom) b ⊔= AT
      if (!removeAbsent.isBottom) b ⊔= AF
      b
  }
}
