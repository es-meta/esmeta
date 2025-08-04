package esmeta.es.builtin

import esmeta.state.*

object TempalteReplacer {

  extension (struct: Struct)
    /** replace template name to instance name */
    def replace(x: String, y: String): Struct = Struct(
      struct.typeName.replace(x, y),
      struct.imap.map { case (k, v) => k.replace(x, y) -> v.replace(x, y) },
      struct.nmap.map { case (k, d) => k.replace(x, y) -> d.replace(x, y) },
    )

  extension (key: PropKey)
    /** replace template name to instance name */
    def replace(x: String, y: String): PropKey = key match
      case PropKey.Str(str) => PropKey.Str(str.replace(x, y))
      case PropKey.Sym(sym) => PropKey.Sym(sym.replace(x, y))

  extension (desc: PropDesc)
    /** replace template name to instance name */
    def replace(x: String, y: String): PropDesc = desc match
      case DataDesc(v, w, e, c) =>
        DataDesc(v.replace(x, y), w, e, c)
      case AccessorDesc(g, s, e, c) =>
        AccessorDesc(g.replace(x, y), s.replace(x, y), e, c)

  extension (v: Value)
    /** replace template name to instance name */
    def replace(x: String, y: String): Value = v match
      case NamedAddr(name) => NamedAddr(name.replace(x, y))
      case Str(s)          => Str(s.replace(x, y))
      case v               => v
}
