package esmeta.util

import scala.io.Source
import scala.quoted.*
import scala.compiletime.{error, codeOf}

object CompileTimeReader {
  // Method 1: Using scala.quoted macros (Scala 3)
  inline def readFileAtCompileTime(inline path: String): String = ${
    readFileAtCompileTimeImpl('path)
  }

  private def readFileAtCompileTimeImpl(path: Expr[String])(using Quotes): Expr[String] = {
    import quotes.reflect.*
    path.value match {
      case Some(p) =>
        try {
          val source = Source.fromFile(p)
          val content = source.mkString
          source.close()
          Expr(content)
        } catch {
          case e: Exception =>
            report.error(s"Failed to read file $p: ${e.getMessage}")
            Expr("")
        }
      case None =>
        report.error("Path must be a literal string")
        Expr("")
    }
  }
}

// // Usage example
// @main def example(): Unit = {
//   // Method 1: Using macro
//   val content1 = CompileTimeReader.readFileAtCompileTime("src/main/resources/example.txt")
//   println(content1)

// }