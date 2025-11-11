package esmeta.test262.jalangi

class JalangiAnalysis {
  def read(varName: String): Unit = {
    println(s"read: $varName")
  }

  def getField(offset: Option[String]): Unit = {
    // println(s"getField: ${offset.getOrElse("...")}")
  }

  def getFieldPre(offset: Option[String]): Unit = {
    // println(s"getFieldPre: ${offset.getOrElse("...")}")
  }
}
