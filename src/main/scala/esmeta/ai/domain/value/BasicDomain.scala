package esmeta.ai.domain.value

import esmeta.ai.*
import esmeta.ai.domain.*
// import esmeta.ai.util.*
// import esmeta.cfg.Func
import esmeta.state.*
// import esmeta.ir.{COp, Name, VOp}
// import esmeta.es.*
// import esmeta.parser.ESValueParser
import esmeta.util.Appender.*

/** TODO basic domain for values */
class BasicDomain(val config: Config) extends value.Domain {
  import config.*

  // // TODO remove unsafe type casting
  // given Conversion[AbsValue, Elem] = _.asInstanceOf[Elem]
  // given Conversion[Elem, AbsValue] = _.asInstanceOf[AbsValue]

  /** TODO elements */
  case class Elem(
    // comp: AbsComp,
    // clo: AbsClo,
    // cont: AbsCont,
    // loc: AbsLoc,
    // ast: AbsAst,
    // grammar: AbsGrammar,
    // codeUnit: AbsCodeUnit,
    // const: AbsConst,
    // math: AbsMath,
    // simple: AbsSimple,
  )

  /** top element */
  lazy val Top: Elem = exploded("top abstract value")

  /** bottom element */
  val Bot: Elem = ???
  // Elem(
  //   comp = AbsComp.Bot,
  //   clo = AbsClo.Bot,
  //   cont = AbsCont.Bot,
  //   loc = AbsLoc.Bot,
  //   ast = AbsAst.Bot,
  //   grammar = AbsGrammar.Bot,
  //   codeUnit = AbsCodeUnit.Bot,
  //   const = AbsConst.Bot,
  //   math = AbsMath.Bot,
  //   simple = AbsSimple.Bot,
  // )

  /** abstraction functions */
  def alpha(iter: Iterable[Value]): Elem = ???

  // /** abstraction functions */
  // def apply(ast: Ast): Elem = Bot.copy(ast = AbsAst(AAst(ast)))
  // def apply(num: Number): Elem = Bot.copy(simple = AbsSimple(num))
  // def apply(num: Double): Elem = Bot.copy(simple = AbsSimple(num))
  // def apply(bigInt: BigInt): Elem = Bot.copy(simple = AbsSimple(bigInt))
  // def apply(str: String): Elem = Bot.copy(simple = AbsSimple(str))
  // def apply(bool: Boolean): Elem = Bot.copy(simple = AbsSimple(bool))
  // def apply(d: BigDecimal): Elem = Bot.copy(math = AbsMath(AMath(d)))
  // lazy val codeUnit: Elem = Bot.copy(codeUnit = AbsCodeUnit.Top)
  // lazy val math: Elem = Bot.copy(math = AbsMath.Top)
  // lazy val num: Elem = Bot.copy(simple = AbsSimple.num)
  // lazy val bigInt: Elem = Bot.copy(simple = AbsSimple.bigInt)
  // lazy val str: Elem = Bot.copy(simple = AbsSimple.str)
  // lazy val bool: Elem = Bot.copy(simple = AbsSimple.bool)
  // lazy val undef: Elem = Bot.copy(simple = AbsSimple.undef)
  // lazy val nullv: Elem = Bot.copy(simple = AbsSimple.nullv)
  // lazy val absent: Elem = Bot.copy(simple = AbsSimple.absent)
  // def apply(value: AValue): Elem = value match
  //   case (comp: AComp)       => Bot.copy(comp = AbsComp(comp))
  //   case (clo: AClo)         => Bot.copy(clo = AbsClo(clo))
  //   case (cont: ACont)       => Bot.copy(cont = AbsCont(cont))
  //   case (loc: Loc)          => Bot.copy(loc = AbsLoc(loc))
  //   case (ast: AAst)         => Bot.copy(ast = AbsAst(ast))
  //   case (grammar: AGrammar) => Bot.copy(grammar = AbsGrammar(grammar))
  //   case (cu: ACodeUnit)     => Bot.copy(codeUnit = AbsCodeUnit(cu))
  //   case (const: AConst)     => Bot.copy(const = AbsConst(const))
  //   case (math: AMath)       => Bot.copy(math = AbsMath(math))
  //   case (simple: ASimple)   => Bot.copy(simple = AbsSimple(simple))
  // def apply(tys: Type*): Elem = ??? // TODO

  // /** constructors */
  // def apply(
  //   comp: AbsComp = AbsComp.Bot,
  //   clo: AbsClo = AbsClo.Bot,
  //   cont: AbsCont = AbsCont.Bot,
  //   loc: AbsLoc = AbsLoc.Bot,
  //   ast: AbsAst = AbsAst.Bot,
  //   grammar: AbsGrammar = AbsGrammar.Bot,
  //   codeUnit: AbsCodeUnit = AbsCodeUnit.Bot,
  //   const: AbsConst = AbsConst.Bot,
  //   math: AbsMath = AbsMath.Bot,
  //   simple: AbsSimple = AbsSimple.Bot,
  //   num: AbsNum = AbsNum.Bot,
  //   bigInt: AbsBigInt = AbsBigInt.Bot,
  //   str: AbsStr = AbsStr.Bot,
  //   bool: AbsBool = AbsBool.Bot,
  //   undef: AbsUndef = AbsUndef.Bot,
  //   nullv: AbsNull = AbsNull.Bot,
  //   absent: AbsAbsent = AbsAbsent.Bot,
  // ): Elem = {
  //   val newSimple = AbsSimple(num, bigInt, str, bool, undef, nullv, absent)
  //   Elem(
  //     comp,
  //     clo,
  //     cont,
  //     loc,
  //     ast,
  //     grammar,
  //     codeUnit,
  //     const,
  //     math,
  //     simple ⊔ newSimple,
  //   )
  // }

  // /** make completion */
  // def mkCompletion(ty: Elem, value: Elem, target: Elem): Elem = {
  //   val t = apply(str = target.str, const = target.const)
  //   apply(comp = AbsComp((for {
  //     AConst(name) <- ty.const.toList
  //   } yield name -> AbsComp.Result(value, t)).toMap))
  // }

  // /** extractors */
  // def unapply(elem: Elem) = Some(
  //   (
  //     elem.comp,
  //     elem.clo,
  //     elem.cont,
  //     elem.loc,
  //     elem.ast,
  //     elem.grammar,
  //     elem.codeUnit,
  //     elem.const,
  //     elem.math,
  //     elem.simple,
  //   ),
  // )

  /** appender */
  given rule: Rule[Elem] = ??? // (app, elem) => {
  //   if (elem.isBottom) app >> "⊥"
  //   else {
  //     val Elem(
  //       comp,
  //       clo,
  //       cont,
  //       loc,
  //       ast,
  //       grammar,
  //       codeUnit,
  //       const,
  //       math,
  //       simple,
  //     ) = elem
  //     var strs = Vector[String]()
  //     if (!comp.isBottom) strs :+= comp.toString
  //     if (!clo.isBottom) strs :+= clo.toString
  //     if (!cont.isBottom) strs :+= cont.toString
  //     if (!loc.isBottom) strs :+= loc.toString
  //     if (!ast.isBottom) strs :+= ast.toString
  //     if (!grammar.isBottom) strs :+= grammar.toString
  //     if (!codeUnit.isBottom) strs :+= codeUnit.toString
  //     if (!const.isBottom) strs :+= const.toString
  //     if (!math.isBottom) strs :+= math.toString
  //     if (!simple.isBottom) strs :+= simple.toString
  //     app >> strs.mkString(", ")
  //   }
  // }

  // /** transfer for variadic operation */
  // def vopTransfer(vop: VOp, vs: List[Elem]): Elem = {
  //   import VOp.*

  //   // helpers
  //   def asMath(av: Elem): Option[BigDecimal] = av.getSingle match
  //     case FlatTop            => exploded("vop transfer")
  //     case FlatElem(AMath(n)) => Some(n)
  //     case _                  => None
  //   def asStr(av: Elem): Option[String] = av.getSingle match
  //     case FlatTop                   => exploded("vop transfer")
  //     case FlatElem(ASimple(Str(s))) => Some(s)
  //     case FlatElem(ACodeUnit(cu))   => Some(cu.toString)
  //     case _                         => None

  //   // transfer body
  //   if (vs.exists(_.isBottom)) Bot
  //   else
  //     vop match
  //       case Min =>
  //         val set = scala.collection.mutable.Set[Elem]()
  //         if (vs.exists(apply(NEG_INF) ⊑ _)) set += apply(NEG_INF)
  //         val filtered = vs.filter((v) => !(apply(POS_INF) ⊑ v))
  //         if (filtered.isEmpty) set += apply(POS_INF)
  //         set += doVopTransfer(asMath, _ min _, apply, filtered)
  //         set.foldLeft(Bot)(_ ⊔ _)
  //       case Max =>
  //         val set = scala.collection.mutable.Set[Elem]()
  //         if (vs.exists(apply(POS_INF) ⊑ _)) set += apply(POS_INF)
  //         val filtered = vs.filter((v) => !(apply(NEG_INF) ⊑ v))
  //         if (filtered.isEmpty) set += apply(NEG_INF)
  //         set += doVopTransfer(asMath, _ min _, apply, filtered)
  //         set.foldLeft(Bot)(_ ⊔ _)
  //       case Concat => doVopTransfer[String](asStr, _ + _, apply, vs)
  // }

  /** element interfaces */
  extension (elem: Elem) {
    // /** getters */
    // def num: AbsNum = simple.num
    // def bigInt: AbsBigInt = simple.bigInt
    // def str: AbsStr = simple.str
    // def bool: AbsBool = simple.bool
    // def undef: AbsUndef = simple.undef
    // def nullv: AbsNull = simple.nullv
    // def absent: AbsAbsent = simple.absent
    // def pure: Elem = copy(comp = AbsComp.Bot)
    def getKeyValue: Elem = ??? // apply(loc = loc, str = str)
    // def getClos: List[(Func, Map[Name, Elem])] =
    //   clo.toList.map { c =>
    //     // TODO remove unsafe type casting
    //     val captured =
    //       (for { (k, v) <- c.captured } yield k -> v.asInstanceOf[Elem]).toMap
    //     (c.func, captured)
    //   }
    // def getConts: List[ACont] = cont.toList
    // def getSDO(method: String): List[(Func, Elem)] = ??? // TODO
    // def getLexical(method: String): Elem = ??? // TODO
    // def getTypedArguments: List[(Elem, Type)] = List((elem, TopT))

    /** partial order */
    def ⊑(that: Elem): Boolean = ??? // (
    //   elem.comp ⊑ that.comp &&
    //     elem.clo ⊑ that.clo &&
    //     elem.cont ⊑ that.cont &&
    //     elem.loc ⊑ that.loc &&
    //     elem.ast ⊑ that.ast &&
    //     elem.grammar ⊑ that.grammar &&
    //     elem.codeUnit ⊑ that.codeUnit &&
    //     elem.const ⊑ that.const &&
    //     elem.math ⊑ that.math &&
    //     elem.simple ⊑ that.simple
    // )

    /** join operator */
    def ⊔(that: Elem): Elem = ??? // Elem(
    //   elem.comp ⊔ that.comp,
    //   elem.clo ⊔ that.clo,
    //   elem.cont ⊔ that.cont,
    //   elem.loc ⊔ that.loc,
    //   elem.ast ⊔ that.ast,
    //   elem.grammar ⊔ that.grammar,
    //   elem.codeUnit ⊔ that.codeUnit,
    //   elem.const ⊔ that.const,
    //   elem.math ⊔ that.math,
    //   elem.simple ⊔ that.simple,
    // )

    /** meet operator */
    override def ⊓(that: Elem): Elem = ??? // Elem(
    //   elem.comp ⊓ that.comp,
    //   elem.clo ⊓ that.clo,
    //   elem.cont ⊓ that.cont,
    //   elem.loc ⊓ that.loc,
    //   elem.ast ⊓ that.ast,
    //   elem.grammar ⊓ that.grammar,
    //   elem.codeUnit ⊓ that.codeUnit,
    //   elem.const ⊓ that.const,
    //   elem.math ⊓ that.math,
    //   elem.simple ⊓ that.simple,
    // )

    /** prune operator */
    override def -(that: Elem): Elem = ??? // Elem(
    //   elem.comp - that.comp,
    //   elem.clo - that.clo,
    //   elem.cont - that.cont,
    //   elem.loc - that.loc,
    //   elem.ast - that.ast,
    //   elem.grammar - that.grammar,
    //   elem.codeUnit - that.codeUnit,
    //   elem.const - that.const,
    //   elem.math - that.math,
    //   elem.simple - that.simple,
    // )

    // /** get single value */
    // def getSingle: Flat[AValue] = (
    //   elem.comp.getSingle ⊔
    //     elem.clo.getSingle ⊔
    //     elem.cont.getSingle ⊔
    //     elem.loc.getSingle ⊔
    //     elem.ast.getSingle ⊔
    //     elem.grammar.getSingle ⊔
    //     elem.codeUnit.getSingle ⊔
    //     elem.const.getSingle ⊔
    //     elem.math.getSingle ⊔
    //     elem.simple.getSingle
    // )

    // /** get reachable locations */
    // def reachableLocs: Set[Loc] = {
    //   var locs = loc.toSet
    //   for ((_, AbsComp.Result(value, target)) <- comp.map) {
    //     locs ++= value.reachableLocs
    //     locs ++= target.reachableLocs
    //   }
    //   for {
    //     AClo(_, captured) <- clo
    //     (_, value) <- captured
    //   } locs ++= value.reachableLocs
    //   for {
    //     ACont(_, captured) <- cont
    //     (_, value) <- captured
    //   } locs ++= value.reachableLocs
    //   locs
    // }

    // /** bitwise operations */
    // def &(that: Elem): Elem = ???
    // def |(that: Elem): Elem = ???
    // def ^(that: Elem): Elem = ???

    // /** comparison operations */
    // def =^=(that: Elem): Elem =
    //   apply(bool = (elem.getSingle, that.getSingle) match
    //     case (FlatBot, _) | (_, FlatBot) => AbsBool.Bot
    //     case (FlatElem(l), FlatElem(r))  => AbsBool(Bool(l == r))
    //     case _ => if ((elem ⊓ that).isBottom) AF else AB,
    //   )
    // def ==^==(that: Elem): Elem = ???
    // def <(that: Elem): Elem = ???

    // /** logical operations */
    // def &&(that: Elem): Elem = ???
    // def ||(that: Elem): Elem = ???
    // def ^^(that: Elem): Elem = ???

    // /** numeric operations */
    // def +(that: Elem): Elem = ???
    // // TODO
    // // AbsValue(
    // //   str = (
    // //     (left.str plus right.str) ⊔
    // //       (left.str plusNum right.num)
    // //   ),
    // //   num = (
    // //     (left.num plus right.num) ⊔
    // //       (right.num plusInt left.int) ⊔
    // //       (left.num plusInt right.int)
    // //   ),
    // //   int = left.int plus right.int,
    // //   bigInt = left.bigInt plus right.bigInt,
    // // )
    // def sub(that: Elem): Elem = ???
    // def /(that: Elem): Elem = ???
    // def *(that: Elem): Elem = ???
    // // TODO
    // // AbsValue(
    // //   num = (
    // //     (left.num mul right.num) ⊔
    // //       (right.num mulInt left.int) ⊔
    // //       (left.num mulInt right.int)
    // //   ),
    // //   int = left.int mul right.int,
    // //   bigInt = left.bigInt mul right.bigInt,
    // // )
    // def %(that: Elem): Elem = ???
    // def %%(that: Elem): Elem = ???
    // def **(that: Elem): Elem = ???
    // def <<(that: Elem): Elem = ???
    // def >>>(that: Elem): Elem = ???
    // def >>(that: Elem): Elem = ???

    // /** unary operations */
    // def unary_- : Elem = ???
    // def unary_! : Elem = apply(bool = !elem.bool)
    // def unary_~ : Elem = ???
    // def abs: Elem = ???
    // def floor: Elem = ???

    // /** type operations */
    // def typeOf(st: AbsState): Elem = {
    //   var set = Set[String]()
    //   if (!elem.num.isBottom) set += "Number"
    //   if (!elem.bigInt.isBottom) set += "BigInt"
    //   if (!elem.str.isBottom) set += "String"
    //   if (!elem.bool.isBottom) set += "Boolean"
    //   if (!elem.undef.isBottom) set += "Undefined"
    //   if (!elem.nullv.isBottom) set += "Null"
    //   if (!elem.loc.isBottom) for (loc <- elem.loc) {
    //     val tname = st(loc).getTy match
    //       case tname if cfg.typeModel.isSubType(tname, "Object") =>
    //         "Object"
    //       case tname => tname
    //     set += tname
    //   }
    //   apply(str = AbsStr(set.map(Str.apply)))
    // }
    // def typeCheck(tname: String, st: AbsState): Elem = {
    //   var bv: AbsBool = AbsBool.Bot
    //   if (!elem.num.isBottom) bv ⊔= AbsBool(Bool(tname == "Number"))
    //   if (!elem.bigInt.isBottom) bv ⊔= AbsBool(Bool(tname == "BigInt"))
    //   if (!elem.str.isBottom) bv ⊔= AbsBool(Bool(tname == "String"))
    //   if (!elem.bool.isBottom) bv ⊔= AbsBool(Bool(tname == "Boolean"))
    //   if (!elem.const.isBottom)
    //     bv ⊔= AbsBool(Bool(tname == "Constant"))
    //   if (!elem.comp.isBottom)
    //     bv ⊔= AbsBool(Bool(tname == "CompletionRecord"))
    //   if (!elem.undef.isBottom)
    //     bv ⊔= AbsBool(Bool(tname == "Undefined"))
    //   if (!elem.nullv.isBottom) bv ⊔= AbsBool(Bool(tname == "Null"))
    //   if (!elem.clo.isBottom)
    //     bv ⊔= AbsBool(Bool(tname == "AbstractClosure"))
    //   elem.ast.getSingle match
    //     case FlatBot => /* do nothing */
    //     case FlatTop => bv = AB
    //     case FlatElem(AAst(ast)) =>
    //       bv ⊔= AbsBool(
    //         Bool(tname == "ParseNode" || (ast.types contains tname)),
    //       )
    //   for (loc <- elem.loc) {
    //     val tname0 = st(loc).getTy
    //     bv ⊔= AbsBool(
    //       Bool(
    //         tname0 == tname || cfg.typeModel.isSubType(tname0, tname),
    //       ),
    //     )
    //   }
    //   apply(bool = bv)
    // }

    // /** helper functions for abstract transfer */
    // def convert(cop: COp, radix: Elem): Elem = {
    //   import COp.*
    //   var newV = Bot
    //   for (Str(s) <- elem.str) newV ⊔= (cop match
    //     case ToNumber => apply(Number(ESValueParser.str2Number(s)))
    //     case ToBigInt => apply(ESValueParser.str2bigInt(s))
    //     case _        => Bot
    //   )
    //   for (AMath(n) <- elem.math) newV ⊔= (cop match
    //     case ToNumber => apply(Number(n.toDouble))
    //     case ToBigInt => apply(BigInt(n.toBigInt))
    //     case _        => Bot
    //   )
    //   for (BigInt(b) <- elem.bigInt) newV ⊔= (cop match
    //     case ToMath => apply(Math(BigDecimal.exact(b)))
    //     case _      => Bot
    //   )
    //   for (ACodeUnit(cu) <- elem.codeUnit) newV ⊔= (cop match
    //     case ToMath => apply(Math(BigDecimal.exact(cu.toInt)))
    //     case _      => Bot
    //   )
    //   for (Number(d) <- elem.num)
    //     newV ⊔= (cop match
    //       case ToNumber | ToMath if d.isInfinity => apply(d)
    //       case ToMath => apply(Math(BigDecimal.exact(d)))
    //       case _: ToStr =>
    //         var newV0 = Bot
    //         for (AMath(n) <- radix.math if n.isValidInt) {
    //           newV0 ⊔= apply(toStringHelper(d, n.toInt))
    //         }
    //         for (Number(n) <- radix.num if n.isValidInt) {
    //           newV0 ⊔= apply(toStringHelper(d, n.toInt))
    //         }
    //         newV0
    //       case _ => Bot
    //     )
    //   newV
    // }
    // def sourceText: Elem = apply(str =
    //   AbsStr(
    //     elem.ast.toList.map(x =>
    //       Str(x.ast.toString(grammar = Some(cfg.grammar)).trim),
    //     ),
    //   ),
    // )
    // def parse(rule: Elem): Elem = {
    //   var newV: Elem = Bot

    //   // codes
    //   var codes: Set[(String, List[Boolean])] = Set()
    //   for (Str(s) <- elem.str) codes += (s, List())
    //   for (AAst(ast) <- elem.ast) {
    //     val code = ast.toString(grammar = Some(cfg.grammar))
    //     val args = ast match
    //       case syn: Syntactic => syn.args
    //       case _              => List()
    //     codes += (code, args)
    //   }

    //   // parse
    //   for {
    //     AGrammar(name, params) <- rule.grammar
    //     (str, args) <- codes
    //     parseArgs = if (params.isEmpty) args else params
    //   } newV ⊔= apply(cfg.esParser(name, parseArgs).from(str))

    //   // result
    //   newV
    // }
    // def duplicated(st: AbsState): Elem =
    //   apply(bool = elem.loc.foldLeft(AbsBool.Bot: AbsBool) {
    //     case (avb, loc) =>
    //       avb ⊔ (st(loc) match {
    //         case _: AbsObj.MergedList => AT
    //         case AbsObj.KeyWiseList(vs) if vs.forall(_.isSingle) =>
    //           val values = vs.map(_.getSingle).flatMap {
    //             case FlatElem(v) => Some(v)
    //             case _           => None
    //           }
    //           AbsBool(Bool(values.toSet.size != values.size))
    //         case _: AbsObj.KeyWiseList => AT
    //         case _                     => AbsBool.Bot
    //       })
    //   })
    // def substring(from: Elem, to: Elem): Elem =
    //   (elem.getSingle, from.getSingle, to.getSingle) match
    //     case (FlatBot, _, _) | (_, FlatBot, _) | (_, _, FlatBot) =>
    //       AbsValue.Bot
    //     case (FlatTop, _, _) | (_, FlatTop, _) | (_, _, FlatTop) =>
    //       exploded("ESubstring")
    //     case (
    //           FlatElem(ASimple(Str(s))),
    //           FlatElem(AMath(f)),
    //           FlatElem(AMath(t)),
    //         ) if f.isValidInt =>
    //       if (s.length < t) apply(s.substring(f.toInt))
    //       else if (t.isValidInt) apply(s.substring(f.toInt, t.toInt))
    //       else Bot
    //     case _ => Bot
    // def isArrayIndex: Elem = elem.getSingle match
    //   case FlatBot => Bot
    //   case FlatElem(ASimple(Str(s))) =>
    //     val d = ESValueParser.str2Number(s)
    //     val ds = toStringHelper(d)
    //     val UPPER = (1L << 32) - 1
    //     val l = d.toLong
    //     apply(ds == s && 0 <= l && d == l && l < UPPER)
    //   case FlatElem(_) => AVF
    //   case FlatTop     => exploded("EIsArrayIndex")

    // /** prune abstract values */
    // def pruneType(r: Elem, positive: Boolean): Elem = elem
    // def pruneTypeCheck(tname: String, positive: Boolean): Elem = elem
    // def pruneValue(r: Elem, positive: Boolean): Elem = elem

    // /** completion helpers */
    // def wrapCompletion: Elem = wrapCompletion("normal")
    // def wrapCompletion(ty: String): Elem = apply(comp = {
    //   if (pure.isBottom) comp
    //   else comp ⊔ AbsComp(ty -> AbsComp.Result(pure, AbsValue(CONST_EMPTY)))
    // })
    // def unwrapCompletion: Elem = comp.normal.value ⊔ elem.pure
    // def isCompletion: Elem =
    //   var b: AbsBool = AbsBool.Bot
    //   if (!comp.isBottom) b ⊔= AT
    //   if (!pure.isBottom) b ⊔= AF
    //   apply(bool = b)
    // def abruptCompletion: Elem = apply(comp = comp.removeNormal)

    // /** absent helpers */
    // def removeAbsent: Elem = copy(simple = simple.removeAbsent)
    // def isAbsent: Elem =
    //   var b: AbsBool = AbsBool.Bot
    //   if (!absent.isBottom) b ⊔= AT
    //   if (!removeAbsent.isBottom) b ⊔= AF
    //   apply(bool = b)

    // /** refine receiver object */
    // def refineelem(func: Func): Elem = elem
  }
}
