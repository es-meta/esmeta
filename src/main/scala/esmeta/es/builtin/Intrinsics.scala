package esmeta.es.builtin

import esmeta.es.*
import esmeta.es.util.Parser

/** model for intrinsic built-in objects */
case class Intrinsics(
  templates: List[Template] = Nil,
  models: List[Model] = Nil,
) extends ESElem {

  /** get the template by name */
  def getInstances(name: String): Map[String, Map[String, String]] =
    templateMap.get(name).fold(Map())(_.instances)

  /** a mapping from template names to templates */
  lazy val templateMap: Map[String, Template] =
    templates.map(t => t.name -> t).toMap

  /** replaced models */
  lazy val replacedModels: List[Model] =
    import TemplateReplacer.*
    models.flatMap(_.replace(templates))
}
object Intrinsics extends Parser.From(Parser.intr)

/** intrinsic model */
case class Model(
  name: String,
  tname: String,
  imap: List[(String, String)] = List(),
  nmap: List[(PropKey, String)] = List(),
) extends ESElem
object Model extends Parser.From(Parser.model)

/** property key */
enum PropKey extends ESElem:
  case Str(str: String)
  case Sym(sym: String)
object PropKey extends Parser.From(Parser.propKey)

/** templates */
case class Template(
  name: String,
  instances: Map[String, Map[String, String]] = Map(),
) extends ESElem
object Template extends Parser.From(Parser.template)
