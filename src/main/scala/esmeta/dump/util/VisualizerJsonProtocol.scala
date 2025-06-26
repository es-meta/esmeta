package esmeta.dump.util

import esmeta.cfg.*
import esmeta.dump.*
import esmeta.error.ESMetaError
import esmeta.spec.*
import esmeta.state.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import io.circe.*, io.circe.syntax.*
import scala.collection.mutable.{ListBuffer, Map as MMap, Set as MSet}

class VisualizerJsonProtocol(cfg: CFG) {
  def funcNameToFeature(fName: String): Feature = {
    val func = cfg.getFunc(fName)
    func.head match
      case Some(head: SyntaxDirectedOperationHead) =>
        SyntacticFeature(func, head)
      case Some(head: BuiltinHead) =>
        BuiltinFeature(func, head)
      case _ =>
        throw ESMetaError(s"invalid feature $fName")
  }

  def parseNodeName(name: String): Int = {
    val result = JsonParser.parseAll(JsonParser.nodeName, name)
    result match
      case JsonParser.Success(value, _) => value
      case _ => throw ESMetaError(s"invalid node name $name")
  }

  given nodeDecoder: Decoder[NodeJson] = Decoder.instance(c =>
    for {
      id <- c.downField("name").as[String].map(parseNodeName(_))
      inst <- c.downField("inst").as[String]
      func <- c.downField("func").as[String].map(cfg.getFunc)
    } yield NodeJson(id, inst, func),
  )

  given viewDecoder: Decoder[ViewJson] = Decoder.instance(c =>
    for {
      enclosing <- c
        .downField("enclosing")
        .as[List[String]]
        .map(_.map(funcNameToFeature))
      feature <- c.downField("feature").as[String].map(funcNameToFeature)
      path <- c.downField("path").as[Option[String]].map(parseCallPath(_))
    } yield ViewJson(enclosing, feature, path),
  )

  given nodeViewDecoder: Decoder[NodeViewJson] = Decoder.instance(c =>
    for {
      node <- c.downField("node").as[NodeJson]
      view <- c.downField("view").as[Option[ViewJson]]
    } yield NodeViewJson(node, view),
  )

  given nodeViewInfoDecoder: Decoder[NodeViewInfoJson] = Decoder.instance(c =>
    for {
      index <- c.downField("index").as[Int]
      nodeView <- c.downField("nodeView").as[NodeViewJson]
      script <- c.downField("script").as[String].map(_.stripSuffix(".js"))
    } yield NodeViewInfoJson(index, nodeView, script),
  )

  def parseCallPath(path: Option[String]): Option[String] = {
    def parse(path: String): String = {
      val result = JsonParser.parseAll(JsonParser.callPath, path)
      result match
        case JsonParser.Success(nodeList, _) =>
          nodeList
            .flatMap { nodeId =>
              val node = cfg.nodeMap(nodeId)
              val funcId = cfg.funcOf(node).id
              node.loc.map(loc => s"$funcId|${loc.stepString}")
            }
            .mkString("-")
        case JsonParser.NoSuccess(msg, next) =>
          throw ESMetaError(
            s"invalid call path $path: $msg at line ${next.pos.line}, column ${next.pos.column}",
          )
        case _ => throw ESMetaError(s"invalid call path $path")
    }
    path match
      case None       => None
      case Some(path) => Some(parse(path))
  }
}
