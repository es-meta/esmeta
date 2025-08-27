package esmeta.analyzer.eoggen.util

import esmeta.analyzer.eoggen.*
import esmeta.cfg.*
import esmeta.ir.{ISdoCall}
import esmeta.util.*
import esmeta.util.BaseUtils.raise
import esmeta.util.HtmlUtils.escapeES
import scala.annotation.{tailrec, targetName}
import esmeta.util.SystemUtils.dumpFile
import scala.util.chaining.*
import esmeta.util.HtmlUtils.escapeHtml

trait EOGDotDecl { self: Self =>

  /** type class for Dottable */
  given dottable_eog: Dottable[ControlPoint] = new Dottable[ControlPoint] {

    extension (x: ControlPoint)
      def id: String = x match
        case NodePoint(func, node, view) =>
          s"\"${func.id}:${node.id}:${view.toString()}\""
        case ReturnPoint(func, view) => s"\"${func.id}:return\""

      def label: String = x match
        case np @ NodePoint(func, node, view) =>
          val content =
            s"<TR><TD>${view.toString.escapeHtml}:${func.name.escapeHtml}<BR />${node.toString.escapeHtml}<BR /></TD></TR>"
          val table = npMap
            .get(np)
            .map(_.locals)
            .fold("<TR><TD><BR /></TD></TR>")(_.asTrsHtml)
          """<<TABLE BORDER="0" CELLBORDER="0" CELLSPACING="0">""" + content + "<TR><TD><TABLE>" + table + "</TABLE></TD></TR>" + "</TABLE>>"
        case ReturnPoint(func, view) => s"\"${func.name}:return\""

      def color: DotFile.Color = DotFile.Color.Black
      def bgColor: DotFile.Color =
        // if x == initialNp then DotFile.Color.Cyan
        // else
        if x.marked then DotFile.Color.Green
        else DotFile.Color.White
      // else {
      //   x match
      //     case NodePoint(_, _: Branch, _) => DotFile.Color.Yellow
      //     case NodePoint(_, _: Call, _)   => DotFile.Color.Green
      //     case ReturnPoint(_, _)          => DotFile.Color.Red
      //     case _                          => DotFile.Color.White
      // }

      override def subgraph: Option[String] =
        // None
        Some(s"${x.func.id.toString()}${x.view.toString()}")

      override def shape: DotFile.Shape = x match
        case NodePoint(_, node, _) =>
          node match
            case _: Block  => DotFile.Shape.Box
            case _: Call   => DotFile.Shape.Ellipse
            case _: Branch => DotFile.Shape.Diamond
        case _: ReturnPoint => DotFile.Shape.Ellipse

      override def edgeLabel(y: ControlPoint): String = (x, y) match
        case ((_: ReturnPoint), (_: NodePoint[?])) => "\"return\""
        case _                                     => "\"\""

    end extension
  }

  extension [A, B](map: Map[A, B]) {
    def asTrsHtml: String = map
      .map {
        case (k, v) =>
          s"<TR><TD>${k.toString.escapeHtml}</TD><TD>${v.toString.escapeHtml}</TD></TR>"
      }
      .mkString("\n")
  }

}
