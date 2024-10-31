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
  val fieldMap1 = FieldMap("p" -> Binding(AnyT, false, false))
  val fieldMap2 = FieldMap(
    "p" -> Binding(AnyT, false, false),
    "q" -> Binding(BoolT, true, false),
  )
  val fieldMap3 = FieldMap(
    "p" -> Binding(AnyT, false, false),
    "q" -> Binding(BoolT, false, true),
    "r" -> Binding(NullT, true, true),
  )
}
