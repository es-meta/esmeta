package esmeta.lang

/** specification utilities */
object Utils {

  /** extensions for CalcExpression */
  extension (expr: CalcExpression) {
    def level: Int =
      import BinaryExpression.Op.*
      expr match {
        case BinaryExpression(_, Add | Sub, _)       => 0
        case BinaryExpression(_, Mul | Div | Mod, _) => 1
        case UnaryExpression(_, _)                   => 2
        case _                                       => 3
      }
  }
}
