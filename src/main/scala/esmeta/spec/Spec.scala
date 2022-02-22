package esmeta.spec

import org.jsoup.nodes.Document

/** ECMAScript specifications (ECMA-262) */
case class Spec(
  version: Option[String] = None,
  grammar: Grammar = Grammar(),
  algorithms: List[Algorithm] = Nil,
  tables: Map[String, Table] = Map(),
  typeModel: TypeModel = TypeModel(),
  document: Document = new Document(""),
) extends SpecElem
