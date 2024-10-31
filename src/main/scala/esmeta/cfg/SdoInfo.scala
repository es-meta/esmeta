package esmeta.cfg

import esmeta.ty.*

/* SDO information */
enum SdoInfo:
  case Default(func: Func, method: String)
  case Base(func: Func, name: String, i: Int, j: Int, method: String)

  /** function id */
  val func: Func

  /** method name */
  val method: String

extension (base: SdoInfo.Base) {
  def thisTy: ValueTy = ValueTy(ast = AstTy.Detail(base.name, base.i))
}
