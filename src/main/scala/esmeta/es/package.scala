package esmeta.es

import esmeta.cfg.*
import esmeta.es.util.*
import esmeta.spec.Grammar
import esmeta.state.*
import esmeta.util.BaseUtils.*

/** ECMAScript elements */
trait ESElem {
  override def toString: String = toString()

  /** stringify with options */
  def toString(
    detail: Boolean = true,
    location: Boolean = false,
    grammar: Option[Grammar] = None,
  ): String = {
    val stringifier = ESElem.getStringifier(detail, location, grammar)
    import stringifier.elemRule
    stringify(this)
  }
}
object ESElem {
  val getStringifier =
    cached[(Boolean, Boolean, Option[Grammar]), Stringifier] {
      Stringifier(_, _, _)
    }
}

/** create a record object with concrete methods */
def recordObj(tname: String)(using CFG): RecordObj = recordObj(tname)()

/** create a record object with concrete methods */
def recordObj(tname: String)(
  fields: (String, Value)*,
)(using cfg: CFG): RecordObj = recordObj(tname)(fields)

/** create a record object with concrete methods */
def recordObj(tname: String)(
  fields: Iterable[(String, Value)],
)(using cfg: CFG): RecordObj = {
  val obj = RecordObj(tname, fields.toMap)
  for {
    (name, f) <- cfg.tyModel.methodOf(tname)
  } obj.map += name -> Clo(cfg.fnameMap(f), Map())
  obj
}
