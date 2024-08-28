package esmeta.mutator

import esmeta.es.*
import esmeta.cfg.CFG
import esmeta.spec.Grammar
import esmeta.synthesizer.*
import esmeta.es.util.*
import esmeta.es.util.Coverage.*
import esmeta.util.BaseUtils.*

/** A mutator that generates based on stirngs in spec literals */
class SpecStringMutator(using cfg: CFG)(
  val synBuilder: Synthesizer.Builder = RandomSynthesizer,
) extends Mutator
  with Util.MultiplicativeListWalker {
  import SpecStringMutator.*

  val names = "SpecStringMutator" :: randomMutator.names

  val synthesizer = synBuilder(cfg.grammar)

  val randomMutator = RandomMutator()

  /** default weight for SpecStringMutator is 1 */
  def calculateWeight(ast: Ast): Int = 1

  /** mutate a program */
  def apply(
    ast: Ast,
    n: Int,
    target: Option[(CondView, Coverage)],
  ): Seq[(String, Ast)] = {
    // count the number of primary expressions
    val k = primaryCounter(ast)
    if (k == 0) randomMutator(ast, n, target)
    else
      c = (n - 1) / k + 1
      targetCondStr = target.flatMap(_._1.cond.elem match {
        case esmeta.cfg.Branch(_, _, e, _, _) => findCondStr(e)
        case _                                => None
      })
      sample(ast, n)
  }

  /** parameter for sampler */
  private var c = 0

  /** string in target branch */
  private var targetCondStr: Option[String] = None

  /** sample n distinct asts using spec-strings */
  private def sample(ast: Ast, n: Int) =
    shuffle(walk(ast)).take(n).map((name, _))

  /** ast walker */
  override def walk(syn: Syntactic): List[Syntactic] =
    if (isPrimary(syn))
      List.tabulate(c)(i => {
        if (targetCondStr.isDefined && i == 0)
          generateString(targetCondStr.get, syn.args)
        else
          generateObject(syn.args)
      }) ++ super.walk(syn)
    else
      super.walk(syn)

  // convert the given string to primary expression
  def generateString(str: String, args: List[Boolean]): Syntactic =
    cfg
      .esParser(PRIMARY_EXPRESSION, args)
      .from(s"\'str\'")
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
  def generateObject(args: List[Boolean]): Syntactic =
    val k = choose(specProps)
    val v = choose(defaultValues)
    val raw = s"{ $k : $v }"
    cfg.esParser(PRIMARY_EXPRESSION, args).from(raw).asInstanceOf[Syntactic]
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
