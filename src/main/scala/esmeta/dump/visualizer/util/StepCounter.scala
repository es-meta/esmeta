package esmeta.dump.visualizer.util

import esmeta.cfg.Node
import esmeta.interpreter.Interpreter
import esmeta.state.State

class StepCounter(
  st: State,
  targetNodeId: Int,
  targetCallPath: String,
) extends Interpreter(st) {
  override def eval(node: Node): Unit =
    val currentCP = new VisualizerJsonProtocol(st.cfg)
      .parseCallPath(Some(st.context.callPath.toString))
      .getOrElse("")
    if (node.id == targetNodeId && (currentCP == targetCallPath)) {
      throw new Exception(stepCnt.toString)
    }

    super.eval(node)
}
