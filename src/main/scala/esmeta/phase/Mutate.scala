package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.interpreter.*
import esmeta.es.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import esmeta.es.*
import esmeta.parser.ESParser
import esmeta.es.util.mutator.*

/** `mutate` phase */
case object Mutate extends Phase[CFG, String] {
  val name = "mutate"
  val help = "mutates a ECMAScript program."
  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): String =
    val filename = getFirstFilename(cmdConfig, this.name)
    val ast = ESParser(cfg.spec.grammar)("Script").fromFile(filename)
    val mutator: Mutator = RandomMutation(cfg.grammar)
    val generator = SimpleAstGenerator(cfg.grammar)
    val synthesizer = RandomSynth(cfg.grammar)
    val temp = List(
//      ("Punctuator", Nil),
      ("CallExpression", List(true, false)),
//      ("CoverCallExpressionAndAsyncArrowHead", List(true, false)),
//      ("Arguments", List(true, false)),
//      ("AssignmentExpression", List(true, true, true)),
    )
//    temp.foreach(x => println(generator.generate(x._1, x._2)))
////    generator.debug()
//    temp.foreach { x =>
//      val temp = synthesizer.synthesize(x._1, x._2);
//      println(temp);
//      println(temp.foreach(_.toString(grammar = Some(cfg.grammar))))
//    }
    for (_ <- Range(0, 20)) {
      println("==============================================================")
      val ret = mutator.mutate(ast)
//      println(ret.toString)
      println(ret.toString(grammar = Some(cfg.grammar)))
      println("==============================================================")
    }
//    ret.toString(grammar = Some(cfg.grammar))
    "whoa"

//
//    RandomMutation.exprList.foreach(expr =>
//      println(expr.toString(grammar = Some(cfg.grammar))),
//    )
//    println(mutator.mutate(ast).toString(grammar = Some(cfg.grammar)))
//    mutator.mutate(ast).toString(grammar = Some(cfg.grammar))
//    "whoa"

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List()
  case class Config()
}
