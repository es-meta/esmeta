package esmeta.compiler

import esmeta.ir.*
import esmeta.ir.util.{Walker => IRWalker}
import esmeta.lang.*

// conversion to references
inline def toStrERef(base: Ref, props: String*): ERef =
  ERef(toStrRef(base, props*))
inline def toStrRef(base: Ref, props: String*): Ref =
  toRef(base, props.map(EStr(_))*)
inline def toERef(base: Ref, props: Expr*): ERef =
  ERef(toRef(base, props*))
inline def toERef(fb: FuncBuilder, base: Expr, props: Expr*): ERef =
  ERef(toRef(fb, base, props*))
inline def toRef(base: Ref, props: Expr*): Ref =
  props.foldLeft(base)(Prop(_, _))
inline def toRef(fb: FuncBuilder, base: Expr, props: Expr*): Ref =
  toRef(getRef(fb, base), props*)
inline def getRef(fb: FuncBuilder, expr: Expr): Ref = expr match {
  case ERef(ref) => ref
  case _ =>
    val x = fb.newTId(); fb.addInst(IAssign(x, expr)); x
}

// conversion to intrinsics
inline def toIntrinsic(
  base: Ref,
  intr: Intrinsic,
  langOpt: Option[Syntax] = None,
): Prop =
  // convert intr to string for exceptional case in GetPrototypeFromConstructor
  Prop(base, EStr(intr.toString), langOpt)
inline def toEIntrinsic(base: Ref, intr: Intrinsic): ERef =
  toERef(toIntrinsic(base, intr))

// current realm
inline def currentRealm: Ref = toStrRef(GLOBAL_CONTEXT, "Realm")

// current intrinsics
inline def currentIntrinsics: Ref =
  toStrRef(currentRealm, "Intrinsics")
