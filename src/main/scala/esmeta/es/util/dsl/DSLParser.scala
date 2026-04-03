package esmeta.es.util.dsl

import esmeta.lang.*
import esmeta.lang.util.Parser as LangParser
import esmeta.lang.util.Parsers
import esmeta.util.Locational

import scala.collection.mutable

/** DSL parser that extends the standard metalanguage parser with support for
  * meta-variables like `{{ name : type }}`. Each instance carries a mutable
  * context of known meta-variable definitions, which is populated during
  * parsing (when a `{{ name : type }}` declaration is encountered) and queried
  * when a bare `{{ name }}` reference is used.
  */
class DSLParser(initialDefs: Map[String, LangElem] = Map.empty)
  extends DSLParsers {

  // Seed the mutable context with any inherited definitions
  defs ++= initialDefs

  def parseStep(input: String): Step = parseBy(step)(input)
  def parseExpr(input: String): Expression = parseBy(expr)(input)
  def parseCond(input: String): Condition = parseBy(cond)(input)
  def parseRef(input: String): Reference = parseBy(ref)(input)
}

/** Mixin trait that adds meta-variable parsing to the standard language
  * parsers. Meta-variables use the `{{ name : type }}` syntax for declarations
  * and `{{ name }}` for references.
  */
trait DSLParsers extends Parsers {

  // ---------------------------------------------------------------------------
  // Mutable context for meta-variable definitions
  // ---------------------------------------------------------------------------

  protected val defs: mutable.Map[String, LangElem] = mutable.Map.empty

  def getContext: Parser[Map[String, LangElem]] = Parser { in =>
    Success(defs.toMap, in)
  }

  def addDef(name: String, elem: LangElem): Parser[Unit] = Parser { in =>
    defs += (name -> elem)
    Success((), in)
  }

  // ---------------------------------------------------------------------------
  // Backtracking combinator
  //
  // Standard parser combinators don't backtrack after consuming input.
  // `{{ name }}` is syntactically identical across all meta-variable types,
  // but dispatched to separate grammar extension points (extraExpr, extraRef,
  // extraVariable, etc.). When one extension point consumes `{{ name }}` and
  // discovers a type mismatch, the grammar must still be able to try other
  // paths (e.g. refExpr when metaExpr rejects a MetaReference).
  //
  // `backtrack` wraps any parser to reset to the original input position on
  // failure, giving PEG-style backtracking for that specific parse attempt.
  // ---------------------------------------------------------------------------

  private def backtrack[T <: Locational](p: Parser[T]): PL[T] =
    new LocationalParser[T] {
      def apply(in: Input): ParseResult[T] = p(in) match {
        case s: Success[_] => s.asInstanceOf[ParseResult[T]]
        case _             => Failure("backtrack", in)
      }
    }

  // ---------------------------------------------------------------------------
  // Meta-variable helpers
  // ---------------------------------------------------------------------------

  lazy val metaName: Parser[String] = "[a-zA-Z_][a-zA-Z0-9_]*".r

  /** Parse a meta-variable name with optional variant suffix: `name'N`. Returns
    * (baseName, variant). Unspecified variant defaults to 0.
    */
  lazy val metaNameWithVariant: Parser[(String, Int)] =
    "[a-zA-Z_][a-zA-Z0-9_]*".r ~ opt("'" ~> "[0-9]+".r) ^^ {
      case name ~ Some(v) => (name, v.toInt)
      case name ~ None    => (name, 0)
    }

  /** {{ 𝑥 : 𝜏 }} acts as a definition that matches any concrete AST fragment
    * of the specified syntactic category and captures it for later pattern
    * matching and substitution.
    */
  private def metaDef[T <: LangElem with Locational](
    typeTag: Parser[_],
    mkElem: (String, Int) => T,
  ): PL[T] =
    ("{{" ~> metaNameWithVariant <~ ":" <~ typeTag <~ "}}") >> {
      case (n, v) =>
        val elem = mkElem(n, v); addDef(n, elem) ^^^ elem
    }

  /** {{ 𝑥 }} performs a consistency check against a previously captured AST
    * based on their inferred symbolic paths, ensuring that they refer to the
    * same underlying value.
    */
  private def metaUse[T <: Locational](
    check: PartialFunction[LangElem, T],
  ): PL[T] =
    backtrack(("{{" ~> metaNameWithVariant <~ "}}") >> {
      case (n, v) =>
        getContext >> { ctx =>
          ctx.get(n).collect(check) match {
            case Some(t) => success(withVariant(t, v))
            case None    => failure("meta type mismatch")
          }
        }
    })

  /** Apply a variant number to a meta-node. */
  private def withVariant[T <: Locational](elem: T, variant: Int): T =
    (elem match {
      case mr: MetaReference  => mr.copy(variant = variant)
      case me: MetaExpression => me.copy(variant = variant)
      case ms: MetaStep       => ms.copy(variant = variant)
      case mc: MetaCondition  => mc.copy(variant = variant)
      case v: Variable        => v.copy(variant = variant)
      case other              => other
    }).asInstanceOf[T]

  // ---------------------------------------------------------------------------
  // Meta-variable parsers
  // ---------------------------------------------------------------------------

  lazy val metaStep: PL[Step] =
    metaDef("step\\*".r, (n, v) => MetaStep(n, true, v)) |
    metaDef("step", (n, v) => MetaStep(n, false, v)) |
    metaUse { case ms: MetaStep => ms }

  lazy val metaExpr: PL[Expression] =
    metaDef("expr", (n, v) => MetaExpression(n, v)) |
    metaUse { case me: MetaExpression => me }

  lazy val metaCond: PL[Condition] =
    metaDef("cond", (n, v) => MetaCondition(n, v)) |
    metaUse { case me: MetaCondition => me }

  lazy val metaRef: PL[Reference] =
    metaDef("ref", (n, v) => MetaReference(n, v)) |
    metaUse { case me: MetaReference => me; case v: Variable if v.meta => v }

  lazy val metaVar: PL[Variable] =
    metaDef("var", (n, v) => Variable(n, meta = true, variant = v)) |
    metaUse { case v: Variable if v.meta => v }

  // ---------------------------------------------------------------------------
  // Extension point overrides
  // ---------------------------------------------------------------------------

  override def extraStep: PL[Step] =
    metaStep | "do nothing." ^^^ BlockStep(StepBlock(Nil))

  override def extraExpr: PL[Expression] = metaExpr |
    "closure" ~> ("(" ~> repsep(
      variable,
      ",",
    ) <~ ")") ~ ("{" ~> step <~ "}") ^^ {
      case ps ~ body =>
        AbstractClosureExpression(ps, Nil, body)
    }

  override def extraCond: PL[Condition] = metaCond

  /** Meta-references with optional access continuations: `{{ base }}[expr]` →
    * IndexLookup, `{{ base }}.[[X]]` → Access
    */
  override def extraRef: PL[Reference] = metaRef ~ rep(
    ".[[" ~> "[a-zA-Z]+".r <~ "]]" ^^ { n => (r: Reference) =>
      Access(r, n, AccessKind.Field, AccessForm.Dot)
    } |
    "[" ~> expr <~ "]" ^^ { i => (r: Reference) => IndexLookup(r, i) },
  ) ^^ { case b ~ cs => cs.foldLeft(b: Reference) { case (r, c) => c(r) } }

  override def extraVariable: PL[Variable] = metaVar
}
