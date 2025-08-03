package esmeta.es.builtin

import esmeta.es.*
import esmeta.cfg.CFG
import esmeta.state.*
import esmeta.spec.*

/** model for global object */
case class GlobalObject(cfg: CFG) {

  /** shortcuts */
  private val T = true
  private val F = false
  private val U = Undef
  private val spec = cfg.program.spec
  given CFG = cfg

  /** get global object */
  def obj: RecordObj = recordObj("Object")(
    INNER_MAP -> mapAddr(GLOBAL),
    PRIVATE_ELEMENTS -> elemsAddr(GLOBAL),
  )

  /** get map for heap */
  lazy val map: Map[Addr, Obj] = {
    import PropKey.Str
    var nmap = List(
      // NOTE: globalThis is added in SetDefaultGlobalBindings
      Str("print") -> DataDesc(intrAddr("print"), T, F, T),
      Str("Infinity") -> DataDesc(Number(Double.PositiveInfinity), F, F, F),
      Str("NaN") -> DataDesc(Number(Double.NaN), F, F, F),
      Str("undefined") -> DataDesc(Undef, F, F, F),
      // test262
      Str("$262") -> DataDesc(intrAddr("$262"), T, F, T),
    )
    val wellKnowns = for {
      row <- spec.tables(WELL_KNOWN_INTRINSICS).rows
      List(intrCell, globCell) = row.take(2).map(_.trim) if globCell != ""
      intrKey = intrCell.replace("%", "")
      globKey = globCell.replace("`", "")
    } yield { Str(globKey) -> DataDesc(intrAddr(intrKey), T, F, T) }
    nmap = wellKnowns ++ nmap
    getMapObjects(GLOBAL, GLOBAL, nmap)
  }
}
