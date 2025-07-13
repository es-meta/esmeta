package esmeta.fuzzer.mutator

import esmeta.es.*
import esmeta.cfg.CFG
import esmeta.spec.Grammar
import esmeta.fuzzer.synthesizer.*
import esmeta.es.util.*
import esmeta.es.util.Coverage.*
import esmeta.util.BaseUtils.*

/** A mutator that generates based on strings in spec literals */
class SpecStringMutator(using cfg: CFG)(
  val synBuilder: Synthesizer.Builder = RandomSynthesizer,
) extends Mutator
  with Walker {
  import SpecStringMutator.*
  import Mutator.*

  val randomMutator = RandomMutator()

  val names = "SpecStringMutator" :: randomMutator.names

  val synthesizer = synBuilder(cfg.grammar)

  /** mutate a program */
  def apply(
    ast: Ast,
    n: Int,
    target: Option[(CondView, Coverage)],
  ): Seq[Result] = {
    // count the number of primary expressions
    val k = primaryCounter(ast)
    if (k == 0) randomMutator(ast, n, target)
    else
      targetCondStr = target.flatMap { (condView, _) =>
        findCondStr(condView.cond.branch.cond)
      }
      sample(ast, n)
  }

  /** string in target branch */
  private var targetCondStr: Option[String] = None

  /** sample n distinct asts using spec-strings */
  private def sample(ast: Ast, n: Int): Seq[Result] =
    Set.tabulate[Ast](n)(_ => walk(ast)).map(Result(name, _)).toSeq

  /** ast walker */
  override def walk(syn: Syntactic): Syntactic =
    if (isPrimary(syn))
      val candidates = List(generateObjectWithWeight(syn.args), syn -> 1)
      if (targetCondStr.isDefined)
        val candidate = (generateString(targetCondStr.get, syn.args) -> 1)
        weightedChoose(candidate :: candidates)
      else weightedChoose(candidates)
    else super.walk(syn)

  // convert the given string to primary expression
  def generateString(str: String, args: List[Boolean]): Syntactic =
    cfg
      .esParser(PRIMARY_EXPRESSION, args)
      .from(s"\'$str\'")
      .asInstanceOf[Syntactic]

  // Properties appearing in specification
  private var _specProps: Set[String] = Set()
  lazy val specProps: Vector[String] = {
    import esmeta.ir.*
    object PropFinder extends util.UnitWalker {
      def addIfProp(e: Expr): Unit = e match {
        case EStr(str) =>
          _specProps += str
        case ERef(Field(Global("SYMBOL"), EStr(sym))) =>
          _specProps += s"[ Symbol . $sym ]"
        case _ =>
      }
      override def walk(inst: Inst) = inst match {
        case ICall(_, EClo(name, _), as) if propReadingAlgos.contains(name) =>
          as.foreach(addIfProp)
        case _ => super.walk(inst)
      }
    }
    _specProps = Set()
    PropFinder.walk(cfg.program)
    _specProps.toVector
  }

  // generate a random object, whose property is read in specification
  def generateObjectWithWeight(args: List[Boolean]): (Syntactic, Int) =
    val k = choose(specProps)
    val v = choose(defaultValues)
    val raw = s"{ $k : $v }"
    cfg.esParser(PRIMARY_EXPRESSION, args).from(raw).asInstanceOf[Syntactic] ->
    (specProps.size * defaultValues.size) // total search space of object generation
}

object SpecStringMutator {
  // macro
  val PRIMARY_EXPRESSION = "PrimaryExpression"

  // count the number of primaryExpressions
  def isPrimary(ast: Ast): Boolean = ast match {
    case Syntactic(PRIMARY_EXPRESSION, _, _, _) => true
    case _                                      => false
  }
  val primaryCounter = Util.AstCounter(isPrimary)

  // manually selected algorithms,
  // whoose purposes is reading property
  val propReadingAlgos = Set(
    "HasProperty",
    "GetMethod",
    "Get",
    "OrdinaryGetOwnProperty",
  )

  // default value of property
  val defaultValues: List[String] = List(
    "true",
    "false",
    "''",
    "function ( x ) { }",
    "function * ( x ) { }",
    "async function ( x ) { }",
    "async function * ( x ) { }",
    "0",
    "null",
    "( ) => { throw 0 ; }",
  )

  // find string literal in condition
  def findCondStr(e: esmeta.ir.Expr): Option[String] = {
    import esmeta.ir.*
    e match {
      case EBinary(BOp.Eq, EStr(str), _) => Some(str)
      case EBinary(BOp.Eq, _, EStr(str)) => Some(str)
      case _                             => None
    }
  }

}
