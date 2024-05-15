package esmeta.state

import esmeta.cfg.*
import esmeta.es.*
import esmeta.ir.Name
import esmeta.spec.*
import esmeta.ty.AstSingleTy
import esmeta.util.UId

/** ECMAScript features */
sealed trait Feature extends StateElem with UId {
  def func: Func
  def head: Head
  def id: Int = func.id
}
case class SyntacticFeature(
  func: Func,
  head: SyntaxDirectedOperationHead,
) extends Feature
case class BuiltinFeature(
  func: Func,
  head: BuiltinHead,
) extends Feature
