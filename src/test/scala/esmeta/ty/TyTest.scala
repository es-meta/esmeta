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
  val declParent0 = TyDecl("A", Some("B"), Nil)
  val declParent1 = TyDecl("A", Some("B"), List(absMethod))
  val declParent2 = TyDecl("A", Some("B"), List(absMethod, conMethodOpt))

  // type models
  val tyModel0 = TyModel(Nil)
  val tyModel1 = TyModel(List(decl0))
  val tyModel2 = TyModel(List(declParent0, decl1))
  val tyModel3 = TyModel(List(decl0, declParent1, decl2))

  // field type map
  val fieldMap0 = FieldMap(Map())
  val fieldMap1 = FieldMap(Map("p" -> Must(AnyT)))
  val fieldMap2 = FieldMap(Map("p" -> Must(AnyT), "q" -> Must(BoolT)))
  val fieldMap3 = FieldMap(
    Map("p" -> Must(AnyT), "q" -> Must(BoolT), "r" -> Must(ObjectT || NullT)),
  )
}
