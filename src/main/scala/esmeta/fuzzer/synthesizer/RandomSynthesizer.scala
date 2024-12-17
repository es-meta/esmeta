package esmeta.fuzzer.synthesizer

import esmeta.es.*
import esmeta.spec.*
import esmeta.util.BaseUtils.*

/** A random ECMAScript AST synthesizer */
class RandomSynthesizer(
  val grammar: Grammar,
) extends Synthesizer {
  import grammar.*

  /** synthesizer name */
  def name: String = "RandomSynthesizer"

  /** for syntactic production */
  def apply(name: String, args: List[Boolean]): Syntactic =
    val prod @ Production(lhs, _, _, rhsVec) = nameMap(name)
    val argsMap = (lhs.params zip args).toMap
    val pairs = for {
      (rhs, rhsIdx) <- rhsVec.zipWithIndex
      if rhs.available(argsMap)
    } yield (rhs, rhsIdx)
    val (rhs, rhsIdx) = choose(pairs)
    val children = rhs.symbols.map(synSymbol(argsMap)).toVector
    Syntactic(name, args, rhsIdx, children)

  /** for lexical production */
  def apply(name: String): Lexical = simpleSyn(name)

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  private val simpleSyn = SimpleSynthesizer(grammar)

  private def synSymbol(argsMap: Map[String, Boolean])(
    symbol: Symbol,
  ): Option[Ast] = symbol match
    case ButNot(nt, _) => synSymbol(argsMap)(nt)
    case Optional(symbol) =>
      if (randBool) None else synSymbol(argsMap)(symbol)
    case Nonterminal(name, args) =>
      if (simpleSyn.reservedLexicals contains name)
        Some(Lexical(name, simpleSyn.reservedLexicals(name)))
      else {
        import NonterminalArgumentKind.*
        val newArgs = for (arg <- args) yield arg.kind match
          case True  => true
          case False => false
          case Pass  => argsMap(arg.name)
        val syn =
          if (randBool) simpleSyn(name, newArgs)
          else apply(name, newArgs)
        Some(syn)
      }
    case _ => None
}
object RandomSynthesizer extends Synthesizer.Builder {
  def apply(grammar: Grammar) = new RandomSynthesizer(grammar)
}
