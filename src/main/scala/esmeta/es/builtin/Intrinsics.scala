package esmeta.es.builtin

import esmeta.es.*
import esmeta.es.util.Parser
import esmeta.cfg.CFG
import esmeta.state.*
import esmeta.spec.*
import esmeta.ty.*
import esmeta.util.BaseUtils.*
import esmeta.util.ManualInfo

/** model for intrinsic built-in objects */
case class Intrinsics(
  cfg: CFG,
  _map: Map[String, Struct] = Map(),
) {

  /** shortcuts */
  private def spec = cfg.program.spec
  given CFG = cfg

  lazy val parser = Parser(cfg)
  lazy val map: Map[String, Struct] = parser.fromFileWithParser(
    ManualInfo.intrinsicsFile,
    parser.intrMap,
  )

  /** get map for heap */
  lazy val heap: Map[Addr, Obj] = {
    var _map = Map[Addr, Obj]()

    // add intrinsic objects
    map.foreach {
      case (name, Struct(typeName, imap, nmap))
          if !(yets contains name.split("\\.").head) =>
        val addr @ NamedAddr(iname) = intrAddr(name)
        // base object
        _map += addr -> recordObj(typeName)(
          List(
            INNER_MAP -> mapAddr(iname),
            PRIVATE_ELEMENTS -> elemsAddr(iname),
          ) ++ imap: _*,
        )
        // inner map object
        _map ++= getMapObjects(iname, name, nmap)
      case _ =>
    }

    // not yet objects
    yets.foreach { (name, _) => _map += (intrAddr(name) -> YetObj(name, name)) }

    // result
    _map
  }

  lazy val kinds: Map[String, ValueTy] =
    val xs = (for {
      x <- names.toList
      addr = intrAddr(x)
      case (obj: RecordObj) <- heap.get(addr)
      ty =
        if (obj.map contains "Construct") ConstructorT
        else if (obj.map contains "Call") FunctionT
        else ObjectT
    } yield x -> ty).toMap ++ yets
    xs.map { case (x, ty) => s"%$x%" -> ty }

  val names: Set[String] = map.keySet ++ yets.keySet

  /** get intrinsic map */
  val obj: MapObj = MapObj(
    names.toList.map(x => Str(s"%$x%") -> intrAddr(x)),
  )

  // get closures
  private def clo(name: String): Clo = Clo(cfg.fnameMap(name), Map())
  private def intrClo(name: String): Clo = clo(intrName(name))
}
