package esmeta.ty

/** operation test */
class MathTinyTest extends TyTest {
  val name: String = "tyMathTest"

  def init: Unit =
    math
    number

  def math: Unit = {
    import MathTy.*, esmeta.state.*
    checkEqual("Canonicalize")(
      MathIntTy(IntSetTy(Set(1L, 2L))).canon -> MathIntTy(
        IntSetTy(Set(1L, 2L)),
      ),
      MathSignTy(Sign(false, true, false)).canon -> MathIntTy(IntSetTy(Set(0))),
      MathSetTy(Math(3), Math(5), Math(2)).canon -> MathIntTy(
        Set(3, 5, 2).map(scala.math.BigInt(_)),
      ),
    )
  }

  def number: Unit = {
    import NumberTy.*, esmeta.state.*
    checkEqual("Or")((NonNegInt || NaN) -> NumberIntTy(IntTy.NonNeg, true))
  }

  init
}
