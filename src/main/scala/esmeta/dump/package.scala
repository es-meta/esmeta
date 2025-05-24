package esmeta.dump

import esmeta.cfg.Func
import esmeta.lang.NoteStep;
import esmeta.state.Feature
import esmeta.util.BasicParsers

val DUMMY_BODY = NoteStep("this is a dummy body to insert after dumping");

case class NodeViewInfoJson(
  index: Int,
  nodeView: NodeViewJson,
  script: String,
)

case class NodeViewJson(
  node: NodeJson,
  view: Option[ViewJson],
)

case class ViewJson(
  enclosing: List[Feature],
  feature: Feature,
  path: Option[String],
)

case class NodeJson(
  id: Int,
  inst: String,
  func: Func,
)

private object JsonParser extends BasicParsers {
  lazy val nodeName: Parser[Int] = ("\\w+".r ~> "[" ~> "\\d+".r <~ "]") ^^ {
    _.toInt
  }

  lazy val callPath: Parser[List[Int]] = {
    ("CallPath[" ~> repsep("\\d+".r ^^ { _.toInt }, "<-") <~ "]")
  }

  lazy val methodName = ("Record[" ~> "\\w+".r <~ "]") ~ ("." ~> "\\w+".r) ^^ {
    case t ~ n => (t, n)
  }
}
