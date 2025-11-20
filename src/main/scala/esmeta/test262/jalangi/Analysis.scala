package esmeta.test262.jalangi

import scala.collection.mutable.ArrayBuffer

class Analysis {

  val ts = ArrayBuffer.empty[Trace]

  def read(varName: String)(using Set[TraceMode]): Unit = this.synchronized {
    ts.append(Trace(summon[Set[TraceMode]], s"read: $varName"))
  }

  def getField(offset: Option[String])(using Set[TraceMode]): Unit = this.synchronized {
    // ts.append(Trace(summon[Set[TraceMode]], s"getField: ${offset.getOrElse("...")}"))
  }

  def getFieldPre(offset: Option[String])(using Set[TraceMode]): Unit = this.synchronized{
    // ts.append(Trace(summon[Set[TraceMode]], s"getFieldPre: ${offset.getOrElse("...")}"))
  }

  def __print(str: String): Unit = this.synchronized {
    ts.append(Trace(TraceMode.All, str))
  }
}
