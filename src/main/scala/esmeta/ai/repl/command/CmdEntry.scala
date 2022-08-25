package esmeta.ai.repl.command

import esmeta.ai.*
import esmeta.ai.repl.*
import esmeta.util.QueueWorklist
import scala.annotation.tailrec

// entry command
case object CmdEntry
  extends Command(
    "entry",
    "Show the set of entry functions of current function",
  ) {
  // options
  val options @ List(path, graph) = List("path", "graph")

  // run command
  def apply(
    repl: REPL,
    cpOpt: Option[ControlPoint],
    args: List[String],
  ): Unit = notYetCmd
  // TODO
  // cpOpt.map(cp => {
  //   var paths = Map[ControlPoint, Path]()
  //   var visited = Set[ControlPoint]()
  //   val worklist = QueueWorklist[Path](List(Nil))

  //   @tailrec
  //   def aux: Unit = worklist.next match {
  //     case Some(path) => {
  //       val curCp = path.headOption.getOrElse(cp)
  //       val func = sem.funcOf(curCp)
  //       val view = curCp.view
  //       val rp = ReturnPoint(func, view)
  //       visited += curCp
  //       func.headOption match {
  //         case Some(head: SyntaxDirectedHead) if head.withParams.isEmpty =>
  //           if (!(paths contains curCp)) paths += curCp -> path.reverse
  //         case _ => for {
  //           (nextCp, _) <- sem.getRetEdges(rp)
  //           if !(visited contains nextCp)
  //         } {
  //           worklist += nextCp :: path
  //         }
  //       }
  //       aux
  //     }
  //     case _ =>
  //   }
  //   aux

  //   // options
  //   args match {
  //     case "-path" :: _ => for {
  //       (_, path) <- paths
  //       len = path.length
  //       _ = println(s"[LENGTH = $len]")
  //       np <- path
  //       func = sem.funcOf(np)
  //       name = func.name
  //     } println(s"   <- $name:$np")
  //     case "-graph" :: _ => {
  //       val (topCp, shortest) = paths.toList.minBy(_._2.length)
  //       val func = sem.funcOf(topCp)
  //       val name = func.name
  //       println(s"- entry with the shortest path: $name:$topCp")
  //       dumpCFG(Some(cp), path = Some(shortest))
  //     }
  //     case _ => for {
  //       (topCp, _) <- paths
  //       f = sem.funcOf(topCp)
  //       name = f.name
  //     } println(name)
  //   }
  // })
}
