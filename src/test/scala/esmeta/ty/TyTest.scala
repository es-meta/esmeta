package esmeta.ty

import esmeta.ESMetaTest

/** test for types */
trait TyTest extends ESMetaTest {
  def category: String = "ty"

  // type declaration elements
  import TyDecl.Elem.*
  val method = Method("a", false, Some("some-clo-name"))
  val methodTop = Method("b", false, None)
  val methodOpt = Method("c", true, Some("some-clo-name"))
  val methodOptTop = Method("d", true, None)

  // type declarations
  val decl0 = TyDecl("A", None, Nil)
  val decl1 = TyDecl("A", None, List(method))
  val decl2 = TyDecl("A", None, List(method, methodOptTop))
  val declParent0 = TyDecl("A", Some("B"), Nil)
  val declParent1 = TyDecl("A", Some("B"), List(method))
  val declParent2 = TyDecl("A", Some("B"), List(method, methodOptTop))

  // type models
  val tyModel0 = TyModel(Nil)
  val tyModel1 = TyModel(List(decl0))
  val tyModel2 = TyModel(List(declParent0, decl1))
  val tyModel3 = TyModel(List(decl0, declParent1, decl2))

  // field type map
  val fieldMap0 = FieldMap(Map())
  val fieldMap1 = FieldMap(Map("p" -> AnyT))
  val fieldMap2 = FieldMap(Map("p" -> AnyT, "q" -> BoolT))
  val fieldMap3 = FieldMap(
    Map("p" -> AnyT, "q" -> BoolT, "r" -> (ObjectT || NullT)),
  )
}
