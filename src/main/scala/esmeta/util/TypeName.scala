package esmeta.util

import scala.quoted.*

private def typeNameImpl[A: Type](using Quotes): Expr[String] = Expr(
  Type.show[A],
)

inline def typeName[A]: String = ${ typeNameImpl[A] }
