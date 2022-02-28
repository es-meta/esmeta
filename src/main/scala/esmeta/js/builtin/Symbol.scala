package esmeta.js.builtin

import esmeta.cfg.CFG
import esmeta.interp.*

/** model for symbols */
case class Symbols(cfg: CFG) {
  private def spec = cfg.program.spec
  given CFG = cfg

  private lazy val symbols: List[String] = (for {
    row <- spec.tables(WELL_KNOWN_SYMBOLS).rows
    symbolKey <- row.headOption.map(_.stripPrefix("@@"))
  } yield symbolKey)

  /** get symbol record */
  def obj: MapObj = MapObj("Record")(
    (for { symKey <- symbols } yield Str(symKey) -> symbolAddr(symKey)): _*,
  )

  /** get map for heap */
  def map: Map[Addr, Obj] = (for { symKey <- symbols } yield symbolAddr(
    symKey,
  ) -> SymbolObj(Str(symbolName(symKey)))).toMap
}
