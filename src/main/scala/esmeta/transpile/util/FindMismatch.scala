package esmeta.transpile.util

import esmeta.es.*

enum Difference:
  case Identical
  case Diff(left: Ast, right: Ast)

  def toOption: Option[(Ast, Ast)] = this match
    case Identical  => None
    case Diff(l, r) => Some((l, r))

extension (ast: Ast)

  def diffRoot(other: Ast): Difference =
    // this check is not fast - maybe use a hash or something?
    if ast =:= other then Difference.Identical
    else
      (ast, other) match
        case (l: Hole, r: Lexical) =>
          Difference.Diff(l, r)
        case (l: Lexical, r: Hole) =>
          Difference.Diff(l, r)
        case (l: Hole, r: Syntactic) =>
          Difference.Diff(l, r)
        case (l: Syntactic, r: Hole) =>
          Difference.Diff(l, r)

        case (l: Hole, r: Hole) =>
          val Hole(name1, args1, label1, _) = l
          val Hole(name2, args2, label2, _) = r
          val cond = name1 == name2 && args1 == args2 && label1 == label2
          if cond then Difference.Identical
          else Difference.Diff(l, r)

        case (l: Syntactic, r: Lexical) => Difference.Diff(l, r)
        case (l: Lexical, r: Syntactic) => Difference.Diff(l, r)
        case (l @ Lexical(n1, s1), r @ Lexical(n2, s2)) =>
          val cond = n1 == n2 && s1 == s2
          if cond then Difference.Identical
          else Difference.Diff(l, r)
        case (l @ Syntactic(n1, a1, i1, c1), r @ Syntactic(n2, a2, i2, c2)) =>
          val cond =
            n1 == n2 && a1 == a2 && i1 == i2 && c1.length == c2.length &&
            (c1.map(_.isDefined).toList == c2.map(_.isDefined).toList)
          if !cond then Difference.Diff(l, r)
          else
            val diffResult = (c1 zip c2)
              .map {
                case (Some(c1), Some(c2)) =>
                  c1.diffRoot(c2) // search recursively
                case (None, None) => Difference.Identical // no child => no diff
                case _            => ??? // impossible, already checked
              }
              .filter(diff => diff != Difference.Identical)
              .toList // 자식별로 재귀 탐색
            diffResult match
              case Nil         => Difference.Identical
              case diff :: Nil => diff
              case _           => Difference.Diff(l, r)
          end if

  inline def >/<(other: Ast): Difference = diffRoot(other)
