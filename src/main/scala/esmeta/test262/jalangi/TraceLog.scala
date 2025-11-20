package esmeta.test262.jalangi

case class TraceLog(traces: Vector[Trace] = Vector.empty) {

  override def toString: String =
    traces.map(_.str).mkString("", "\n", "\n")
  
  lazy val sanitizedForJalangi: TraceLog = {
    val jalangiTraces = traces.filter(_.mode.contains(TraceMode.Jalangi))
    TraceLog(jalangiTraces)
  }

  lazy val sanitizedForNodeProf: TraceLog = {
    val nodeProfTraces = traces.filter(_.mode.contains(TraceMode.NodeProf))
    TraceLog(nodeProfTraces)
  }
}