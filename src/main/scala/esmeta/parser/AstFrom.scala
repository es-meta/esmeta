package esmeta.parser

import esmeta.es.Ast
import scala.scalajs.js.annotation.JSExport


/** ECMAScript abstract syntax tree getter from strings or files */
trait AstFrom {
  def fromFile(str: String): Ast

  @JSExport
  def from(str: String): Ast
}
