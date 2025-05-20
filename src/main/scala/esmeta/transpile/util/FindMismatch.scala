package esmeta.transpile.util

import esmeta.es.*

enum Diff:
  case Identical
  case Diff(left: Ast, right: Ast)

extension (ast: Ast)

  def diffRoot(other: Ast): Diff =
    // this check is not fast - maybe use a hash or something?
    if ast =:= other then Diff.Identical
    else
      (ast, other) match
        case (l: Syntactic, r: Lexical) => Diff.Diff(l, r)
        case (l: Lexical, r: Syntactic) => Diff.Diff(l, r)
        case (l @ Lexical(n1, s1), r @ Lexical(n2, s2)) =>
          val cond = n1 == n2 && s1 == s2
          if cond then Diff.Identical
          else Diff.Diff(l, r)
        case (l @ Syntactic(n1, a1, i1, c1), r @ Syntactic(n2, a2, i2, c2)) =>
          val cond =
            n1 == n2 && a1 == a2 && i1 == i2 && c1.length == c2.length &&
            (c1.map(_.isDefined).toList == c2.map(_.isDefined).toList)
          if !cond then Diff.Diff(l, r)
          else
            val diffResult = (c1 zip c2)
              .map {
                case (Some(c1), Some(c2)) =>
                  c1.diffRoot(c2) // search recursively
                case (None, None) => Diff.Identical // no child => no diff
                case _            => ??? // impossible, already checked
              }
              .filter(diff => diff != Diff.Identical)
              .toList // 자식별로 재귀 탐색
            diffResult match
              case Nil         => Diff.Identical
              case diff :: Nil => diff
              case _           => Diff.Diff(l, r)
          end if

  inline def >/<(other: Ast): Diff = diffRoot(other)
