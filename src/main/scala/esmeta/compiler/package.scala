package esmeta.compiler

import esmeta.ir.util.{Walker => IRWalker}
import esmeta.ir.{Type => IRType, *}
import esmeta.lang.*
import esmeta.ty.*

// conversion to references
inline def toStrERef(base: Ref, fields: String*): ERef =
  ERef(toStrRef(base, fields*))
inline def toStrRef(base: Ref, fields: String*): Ref =
  toRef(base, fields.map(EStr(_))*)
inline def toERef(base: Ref, fields: Expr*): ERef =
  ERef(toRef(base, fields*))
inline def toERef(fb: FuncBuilder, base: Expr, fields: Expr*): ERef =
  ERef(toRef(fb, base, fields*))
inline def toRef(base: Ref, fields: Expr*): Ref =
  fields.foldLeft(base)(Field(_, _))
inline def toRef(fb: FuncBuilder, base: Expr, fields: Expr*): Ref =
  toRef(getRef(fb, base), fields*)
inline def getRef(fb: FuncBuilder, expr: Expr): Ref = expr match {
  case ERef(ref) => ref
  case _         => val x = fb.newTId; fb.addInst(IAssign(x, expr)); x
}

// conversion to intrinsics
inline def toIntrinsic(base: Ref, intr: Intrinsic): Field =
  // convert intr to string for exceptional case in GetPrototypeFromConstructor
  Field(base, EStr(intr.toString(true, false)))
inline def toEIntrinsic(base: Ref, intr: Intrinsic): ERef =
  toERef(toIntrinsic(base, intr))

// current realm
inline def currentRealm: Ref = toStrRef(GLOBAL_CONTEXT, "Realm")

// current intrinsics
inline def currentIntrinsics: Ref =
  toStrRef(currentRealm, "Intrinsics")
