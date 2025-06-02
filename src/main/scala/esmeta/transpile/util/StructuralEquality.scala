package esmeta.transpile.util

import esmeta.es.*

extension (ast: Ast) {

  /** check if two ASTs are structurally equal */
  def =:=(other: Ast): Boolean = (ast, other) match {
    case (Syntactic(n1, a1, r1, c1), Syntactic(n2, a2, r2, c2)) =>
      n1 == n2 && a1 == a2 && r1 == r2 &&
      c1.length == c2.length &&
      (c1 zip c2).forall {
        case (Some(a), Some(b)) => a =:= (b)
        case (None, None)       => true
        case _                  => false
      }
    case (Lexical(n1, s1), Lexical(n2, s2)) => n1 == n2 && s1 == s2
    case _                                  => false
  }

}
