package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.es.util.JsonProtocol
import esmeta.es.util.Coverage.Cond
import esmeta.solver.Reducer
import esmeta.util.*
import esmeta.util.SystemUtils.*
import io.circe.Decoder

/** `reduce` phase */
case object Reduce extends Phase[CFG, Unit] {
  val name = "reduce"
  val help = "reduces programs, keeping the branch sides they cover."

  def apply(cfg: CFG, cmdConfig: CommandConfig, config: Config): Unit =
    val dir = getFirstFilename(cmdConfig, name)
    val jsonProtocol = JsonProtocol(cfg)
    import jsonProtocol.given

    // TODO: views are left out for now (should extend to fs views)
    given Decoder[(Cond, String)] = c =>
      for {
        cond <- c.downField("condView").downField("cond").as[Cond]
        script <- c.downField("script").as[String]
      } yield (cond, script)
    val infos = readJson[List[(Cond, String)]](s"$dir/branch-coverage.json")
    // the solver names programs from its log directory, the fuzzer from minimal
    def read(script: String): String =
      val path = s"$dir/$script"
      readFile(if (exists(path)) path else s"$dir/minimal/$script")
    val conds = infos.map((c, _) => (c.branch.id, c.cond) -> c)
    // the shortest program of each branch side
    val witnesses = infos
      .groupMap((c, _) => (c.branch.id, c.cond))(_._2)
      .map((key, scripts) => key -> scripts.distinct.map(read).minBy(_.length))
    val reduced = Reducer(cfg)(witnesses)
    val out = config.out.getOrElse(s"$dir/reduced")
    mkdir(out)
    val results = conds.toMap.toList.sortBy(_._1).map((k, c) => c -> reduced(k))
    Solve.dumpWitnesses(cfg, out, results)

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "out",
      StrOption((c, s) => c.out = Some(s)),
      "output directory (default: reduced in the input directory).",
    ),
  )
  case class Config(var out: Option[String] = None)
}
