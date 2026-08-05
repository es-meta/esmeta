package esmeta.fuzzer.mutator

import esmeta.es.*
import esmeta.es.util.*
import esmeta.util.BaseUtils.*
import esmeta.cfg.CFG
import esmeta.parser.{ESParser, AstFrom}

/** ECMAScript AST mutator */
trait Mutator(using val cfg: CFG) {
  import Mutator.*, Coverage.*

  /** ECMAScript parser */
  lazy val esParser: ESParser = cfg.esParser
  lazy val scriptParser: AstFrom = esParser("Script")
  lazy val assignExprParser: AstFrom =
    esParser("AssignmentExpression", List(true, false, false))

  /** mutate code */
  def apply(code: String, target: Option[(CondView, Coverage)]): Result =
    apply(code, 1, target).head
  def apply(
    code: String,
    n: Int,
    target: Option[(CondView, Coverage)],
  ): Seq[Result] =
    val ast = scriptParser.from(code)
    apply(ast, n, target).map { mutatedAst =>
      val s = mutatedAst.toString(grammar = Some(cfg.grammar))
      Result(name, Code.Simple(s))
    }

  /** mutate structured Code */
  def apply(code: Code, target: Option[(CondView, Coverage)]): Result =
    apply(code, 1, target).head
  def apply(
    code: Code,
    n: Int,
    target: Option[(CondView, Coverage)],
  ): Seq[Result] = code match
    case Code.Simple(s)  => apply(s, n, target)
    case Code.Builtin(f) => mutateBuiltinOperands(f, n, target)

  /** mutate AST */
  def apply(ast: Ast, target: Option[(CondView, Coverage)]): Ast =
    apply(ast, 1, None).head
  def apply(ast: Ast, n: Int, target: Option[(CondView, Coverage)]): Seq[Ast]

  /** parse manually crafted snippets at given production level */
  def manuals(ast: Syntactic): List[Syntactic] =
    Mutator.manualExprs.flatMap { s =>
      try { Some(esParser(ast.name, ast.args).from(s).asInstanceOf[Syntactic]) }
      catch { case _: Exception => None }
    }

  /** Possible names of underlying mutators */
  val names: List[String]
  lazy val name: String = names.head

  /** mutate a builtin call, leaving its path untouched */
  private def mutateBuiltinOperands(
    form: BuiltinForm,
    n: Int,
    target: Option[(CondView, Coverage)],
  ): Seq[Result] =
    val ops = form.operands
    if (ops.isEmpty) return Seq(Result(name, Code.Builtin(form)))
    val perOp = math.max(1, (n + ops.size - 1) / ops.size)
    val results = for {
      (op, i) <- ops.zipWithIndex
      // an unparsable operand has no assignment-expression mutants to offer
      mutated <- optional(assignExprParser.from(op)).toList.flatMap { ast =>
        apply(ast, perOp, target).map(_.toString(grammar = Some(cfg.grammar)))
      }
      newForm = form.withOperands(ops.updated(i, mutated))
    } yield Result(name, Code.Builtin(newForm))
    if (results.isEmpty) Seq(Result(name, Code.Builtin(form)))
    else shuffle(results).take(n)
}

object Mutator {

  /** Result of mutation with mutator name and code */
  case class Result(name: String, codeObj: Code):
    def code: String = codeObj.toString

  /** Manually crafted expression strings for Syntactic node mutation */
  val manualExprs: List[String] =
    val nullish = List("null", "undefined")
    val booleans = List("true", "false")
    val numbers =
      List("0", "1", "-0", "-1", "0.1", "-0.1", "NaN", "Infinity", "-Infinity")
    val numberBounds = List(
      "Number.MAX_SAFE_INTEGER",
      "Number.MIN_SAFE_INTEGER",
      "9007199254740992", // 2^53, Number.MAX_SAFE_INTEGER + 1
      "Number.MAX_VALUE",
      "-Number.MAX_VALUE",
      "2147483647", // 2^31 - 1, Int32 max
      "-2147483648", // -2^31, Int32 min
      "4294967295", // 2^32 - 1, Uint32 max
    )
    val bigints = List("0n", "1n", "-1n")
    val bigintBounds = List(
      "9223372036854775807n", // 2^63 - 1, BigInt64 max
      "-9223372036854775808n", // -2^63, BigInt64 min
      "18446744073709551615n", // 2^64 - 1, BigUint64 max
    )
    val symbols = List("Symbol()", "Symbol.iterator")
    val strings = List("\"\"", "\"0\"")
    val objects = List("[]", "[,]", "{}", "function(){}")
    val frozenObjects = List(
      "Object.freeze ( { x : 1 } )",
      "Object.seal ( { x : 1 } )",
      "Object.preventExtensions ( { } )",
      "Object.create ( null )",
    )
    nullish ++ booleans ++ numbers ++ numberBounds ++
    bigints ++ bigintBounds ++ symbols ++ strings ++
    objects ++ frozenObjects

  /** Manually crafted token values for Lexical node mutation */
  val manualLexicalMap: Map[String, List[String]] = Map(
    "NullLiteral" -> List("null"),
    "BooleanLiteral" -> List("true", "false"),
    "NumericLiteral" -> List(
      "0",
      "1",
      "0.1",
      "0n",
      "1n",
      "0x0",
      "0o0",
      "0b0",
      "2147483647", // 2^31 - 1, Int32 max
      "4294967295", // 2^32 - 1, Uint32 max
      "9007199254740991", // 2^53 - 1, Number.MAX_SAFE_INTEGER
      "9007199254740992", // 2^53, Number.MAX_SAFE_INTEGER + 1
    ),
    "StringLiteral" -> List(
      "\"\"",
      "\"0\"",
      "\" \"",
      "\"__proto__\"",
      "\"constructor\"",
      "\"toString\"",
      "\"valueOf\"",
      "\"length\"",
      "\"prototype\"",
    ),
    "StringNumericLiteral" -> List("Infinity", "-Infinity", "0", "-0"),
    "RegularExpressionLiteral" -> List("/x/", "/x/g", "/x/u"),
  )
}
