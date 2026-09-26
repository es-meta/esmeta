package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.es.util.JsonProtocol
import esmeta.es.util.Coverage.Cond
import esmeta.solver.Amplifier
import esmeta.util.*
import esmeta.util.SystemUtils.*
import io.circe.Decoder

/** `amplify` phase */
case object Amplify extends Phase[CFG, Unit] {
  val name = "amplify"
  val help = "amplifies programs, keeping the branch sides they cover."

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
    val amplified = Amplifier(cfg)(witnesses)
    val out = config.out.getOrElse(s"$dir/amplified")
    mkdir(out)
    val results = conds.toMap.toList.sortBy(_._1).flatMap { (k, c) =>
      amplified.getOrElse(k, List(witnesses(k))).map(c -> _)
    }
    Solve.dumpWitnesses(cfg, out, results)

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "out",
      StrOption((c, s) => c.out = Some(s)),
      "output directory (default: amplified in the input directory).",
    ),
  )
  case class Config(var out: Option[String] = None)
}
