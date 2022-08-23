package esmeta.ai.domain.addr

import esmeta.ai.*
import esmeta.ir.*
import esmeta.util.BaseUtils.*
import esmeta.state.*

/** address partition */
trait Partition {

  /** addresses partition elements */
  type Elem

  /** addresses partition context */
  type Context

  /** abstraction of addresses to partition elements with contexts */
  def phi(addr: Addr, ctxt: Context): Elem
}

/** allocation site partition with heap cloning */
object AllocSitePartition extends Partition {

  /** addresses partition elements */
  sealed trait Elem {

    /** check named elements */
    def isNamed: Boolean = this match
      case Named(_) | SubMap(Named(_)) => true
      case _                           => false

    /** get base elements */
    def base: Base
  }

  /** addresses partition context */
  type Context = (Int, View)

  /** abstraction of addresses to partition elements with contexts */
  def phi(addr: Addr, ctxt: (Int, View)): Elem =
    val (asite, view) = ctxt
    addr match
      case NamedAddr(name) =>
        name match
          case subMapPattern(base) => SubMap(Named(base))
          case name                => Named(name)
      case _: DynamicAddr => AllocSite(asite, view)

  sealed trait Base extends Elem { def base = this }
  case class Named(name: String) extends Base
  case class AllocSite(k: Int, view: View) extends Base
  case class SubMap(base: Base) extends Elem

  private val subMapPattern = "(.+).SubMap".r
}
