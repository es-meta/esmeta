package esmeta.ty

import esmeta.ESMetaTest

/** test for types */
trait TyTest extends ESMetaTest {
  def category: String = "ty"

  // type declaration elements
  import TyDecl.Elem.*
  val absMethod = AbsMethod("a")
  val conMethod = ConMethod("b", false, None)
  val conMethodOpt = ConMethod("c", true, None)
  val conMethodTarget = ConMethod("d", false, Some("foo"))
  val conMethodOptTarget = ConMethod("e", true, Some("bar"))

  // type declarations
  val decl0 = TyDecl("A", None, Nil)
  val decl1 = TyDecl("A", None, List(absMethod))
  val decl2 = TyDecl("A", None, List(absMethod, conMethodOpt))
  val declParent0 = TyDecl("A", Some("B", true), Nil)
  val declParent1 = TyDecl("A", Some("B", false), List(absMethod))
  val declParent2 = TyDecl("A", Some("B", true), List(absMethod, conMethodOpt))

  // type models
  val tyModel0 = TyModel(Nil)
  val tyModel1 = TyModel(List(decl0))
  val tyModel2 = TyModel(List(declParent0, decl1))
  val tyModel3 = TyModel(List(decl0, declParent1, decl2))

  // field type map
  val fieldMap0 = FieldMap()
  val fieldMap1 = FieldMap("p" -> Binding(AnyT, false))
  val fieldMap2 = FieldMap(
    "p" -> Binding(AnyT, false),
    "q" -> Binding(BoolT, false),
    "r" -> Binding(UndefT, true),
  )

  /** a domain to check the lattice laws of */
  protected case class Domain[T, V](
    tys: List[T],
    values: List[V],
    contains: (T, V) => Boolean,
    le: (T, T) => Boolean,
    prune: (T, T) => T,
    join: (T, T) => T,
    meet: (T, T) => T,
    canon: T => T,
    isBottom: T => Boolean,
  )

  /** Check that each abstract operation over-approximates its concrete
    * counterpart: no value that belongs in the result may be missing from it. A
    * violation means the analyzer may drop a reachable value, which is exactly
    * the class of bug these operations have had.
    */
  protected def checkLaws[T, V](desc: String)(d: Domain[T, V]): Unit =
    import d.*
    def has(t: T, v: V) = contains(t, v)
    val violations = (for {
      l <- tys
      // canon must not change the meaning of a type
      v <- values
      if has(l, v) != has(canon(l), v)
    } yield s"canon changes membership of $v in $l") ++ (for {
      l <- tys
      r <- tys
      v <- values
      (op, ty, expected) <- List(
        ("--", prune(l, r), has(l, v) && !has(r, v)),
        ("||", join(l, r), has(l, v) || has(r, v)),
        ("&&", meet(l, r), has(l, v) && has(r, v)),
      )
      if expected && !has(ty, v)
    } yield s"$v is missing from ($l $op $r) = $ty") ++ (for {
      l <- tys
      r <- tys
      // the order must agree with containment, and bound each result
      msg <-
        (if (le(l, r) && values.exists(v => has(l, v) && !has(r, v)))
           List(s"$l <= $r but they differ on a concrete value")
         else Nil) ++
        (if (!le(l, join(l, r))) List(s"$l is not below ($l || $r)")
         else Nil) ++
        (if (!le(meet(l, r), l)) List(s"($l && $r) is not below $l")
         else Nil) ++
        (if (!le(prune(l, r), l)) List(s"($l -- $r) is not below $l") else Nil)
    } yield msg) ++ (for {
      l <- tys
      if !isBottom(prune(l, l))
    } yield s"($l -- $l) is not bottom")
    check(desc) {
      if (violations.nonEmpty) {
        println(s"[FAILED] $desc: ${violations.size} violation(s)")
        violations.distinct.take(10).foreach(v => println(s"- $v"))
        assert(violations.isEmpty)
      }
    }
}
