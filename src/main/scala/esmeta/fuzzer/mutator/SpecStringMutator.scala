package esmeta.fuzzer.mutator

import esmeta.cfg.{CFG, Func}
import esmeta.es.*
import esmeta.es.util.*
import esmeta.es.util.Coverage.*
import esmeta.fuzzer.synthesizer.*
import esmeta.ir.*
import esmeta.util.BaseUtils.*

/** A mutator that generates based on strings in spec literals */
class SpecStringMutator(using cfg: CFG)(
  val synBuilder: Synthesizer.Builder = RandomSynthesizer,
) extends Mutator
  with Walker {
  import Mutator.*, SpecStringMutator.*, Code.*

  val randomMutator = RandomMutator()

  val names = "SpecStringMutator" :: randomMutator.names

  val synthesizer = synBuilder(cfg.grammar)

  /** mutate code */
  def apply(
    code: Code,
    n: Int,
    target: Option[(CondView, Coverage)],
  ): Seq[Result] = code match
    case Normal(str) =>
      apply(str, n, target).map(str => Result(name, Normal(str)))
    case builtin @ Builtin(_, _, _, preStmts, postStmts) =>
      if ((preStmts.isDefined || postStmts.isDefined) && randBool) {
        // mutate statements
        (preStmts, postStmts) match
          case (Some(_), Some(_)) =>
            if randBool then builtin.mutatePreStmts(n, target)
            else builtin.mutatePostStmts(n, target)
          case (Some(_), None) => builtin.mutatePreStmts(n, target)
          case (None, Some(_)) => builtin.mutatePostStmts(n, target)
          case (None, None)    => raise("unreachable")
      } else {
        // mutate builtin call arguments
        builtin.mutateArgStr(n, target)
      }

  /** mutate ASTs */
  def apply(ast: Ast, n: Int, target: Option[(CondView, Coverage)]): Seq[Ast] =
    // count the number of primary expressions
    val k = primaryCounter(ast)
    if (k > 0) {
      for ((cv, _) <- target)
        targetFunc = cfg.funcOf.get(cv.cond.branch)
        targetCondStr = findCondStr(cv.cond.branch.cond)
      Set.tabulate(n)(_ => walk(ast)).toSeq
    } else randomMutator(ast, n, target)

  /** function where target branch is in */
  private var targetFunc: Option[Func] = None

  /** string in target branch */
  private var targetCondStr: Option[String] = None

  /** ast walker */
  override def walk(syn: Syntactic): Syntactic =
    if (isPrimary(syn))
      val candidates = List(
        generateObjectWithWeight(syn.args),
        generateGetterWithWeight(syn.args),
        generateSetterWithWeight(syn.args),
        syn -> 1,
      )
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
  private var _specProps: Map[Func, Set[String]] = Map()
  lazy val specProps: Map[Func, Vector[String]] = {
    import esmeta.ir.*
    object PropFinder extends util.UnitWalker {
      var currentCfgFunc: Option[esmeta.cfg.Func] = None
      def addIfProp(e: Expr): Unit = e match {
        case EStr(str) =>
          currentCfgFunc.foreach { cfgFunc =>
            val currentSet = _specProps.getOrElse(cfgFunc, Set())
            _specProps += (cfgFunc -> (currentSet + str))
          }
        case ERef(Field(Global("SYMBOL"), EStr(sym))) =>
          currentCfgFunc.foreach { cfgFunc =>
            val currentSet = _specProps.getOrElse(cfgFunc, Set())
            _specProps += (cfgFunc -> (currentSet + s"[ Symbol . $sym ]"))
          }
        case _ =>
      }
      override def walk(inst: Inst) = inst match {
        case ICall(_, EClo(name, _), as) if propReadingAlgos.contains(name) =>
          as.foreach(addIfProp)
        case _ => super.walk(inst)
      }
    }
    _specProps = Map()
    for (cfgFunc <- cfg.funcs) {
      PropFinder.currentCfgFunc = Some(cfgFunc)
      PropFinder.walk(cfgFunc.irFunc.body)
      PropFinder.currentCfgFunc = None
    }
    _specProps.view.mapValues(_.toVector).toMap
  }

  // generate a random object, whose property is read in specification
  def generateObjectWithWeight(args: List[Boolean]): (Syntactic, Int) =
    val props = targetFunc match
      case Some(func) =>
        val targetProps = specProps.getOrElse(func, Vector.empty)
        val allProps = specProps.values.flatten.toVector.distinct
        if (targetProps.isEmpty) allProps
        else { if randBool then targetProps else allProps }
      case None => specProps.values.flatten.toVector.distinct
    val k = choose(props)
    val v = choose(defaultValues)
    val raw = s"{ $k : $v }"
    cfg.esParser(PRIMARY_EXPRESSION, args).from(raw).asInstanceOf[Syntactic] ->
    (props.size * defaultValues.size) // total search space of object generation

  // generate a random getter/setter, whose property is read in specification
  def generateGetterWithWeight(args: List[Boolean]): (Syntactic, Int) =
    val props = targetFunc match
      case Some(func) =>
        val targetProps = specProps.getOrElse(func, Vector.empty)
        val allProps = specProps.values.flatten.toVector.distinct
        if (targetProps.isEmpty) allProps
        else { if randBool then targetProps else allProps }
      case None => specProps.values.flatten.toVector.distinct
    val k = choose(props)
    val getter = s"{ get $k () {} }"
    val throwingGetter = s"{ get $k () { throw 42; } }"
    cfg
      .esParser(PRIMARY_EXPRESSION, args)
      .from(choose(List(getter, throwingGetter)))
      .asInstanceOf[Syntactic] -> props.size
  def generateSetterWithWeight(args: List[Boolean]): (Syntactic, Int) =
    val props = targetFunc match
      case Some(func) =>
        val targetProps = specProps.getOrElse(func, Vector.empty)
        val allProps = specProps.values.flatten.toVector.distinct
        if (targetProps.isEmpty) allProps
        else { if randBool then targetProps else allProps }
      case None => specProps.values.flatten.toVector.distinct
    val k = choose(props)
    val setter = s"{ set $k (_) {} }"
    val throwingSetter = s"{ set $k (_) { throw 42; } }"
    cfg
      .esParser(PRIMARY_EXPRESSION, args)
      .from(choose(List(setter, throwingSetter)))
      .asInstanceOf[Syntactic] -> props.size
}

object SpecStringMutator {
  // macro
  val PRIMARY_EXPRESSION = "PrimaryExpression"

  // count the number of primaryExpressions
  def isPrimary(ast: Ast): Boolean = ast match
    case Syntactic(PRIMARY_EXPRESSION, _, _, _) => true
    case _                                      => false

  val primaryCounter = Util.AstCounter(isPrimary)

  // manually selected algorithms, whose purpose is reading property
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
  def findCondStr(e: Expr): Option[String] = e match
    case EBinary(BOp.Eq, EStr(str), _) => Some(str)
    case EBinary(BOp.Eq, _, EStr(str)) => Some(str)
    case _                             => None
}
