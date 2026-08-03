package esmeta.fv

import esmeta.ir.Type
import esmeta.ty.*
import java.nio.file.Files
import org.scalatest.funsuite.AnyFunSuite

class FVExportDiagnosticTest extends AnyFunSuite {
  test("rocqTy identifies the component that rejects a type") {
    val cases = List(
      "record" -> RecordT(
        Map("Object" -> FieldMap("Call" -> Binding(BoolT))),
      ),
      "math" -> MathT(BigDecimal("0.5")),
      // Finite Boolean sets became exactly representable in Wave C, so keep
      // this diagnostic regression on a component that is still unsupported.
      "map" -> MapT(StrT, UndefT),
    )

    for ((component, ty) <- cases) {
      withClue(s"$component: ") {
        val error = intercept[FVExport.Unsupported] {
          FVExport.rocqTy(Type(ty))
        }
        assert(error.getMessage.contains(s"ty: $component:"))
      }
    }
  }

  test("annotation fallback catches only an explicit unsupported type") {
    assert(
      FVExport.unsupportedToNone[String](
        throw FVExport.Unsupported("known boundary"),
      ).isEmpty,
    )

    val defect = new IllegalStateException("unexpected exporter defect")
    val thrown = intercept[IllegalStateException] {
      FVExport.unsupportedToNone[String](throw defect)
    }
    assert(thrown eq defect)
  }

  test("exportFile propagates unexpected parser and IO failures") {
    val missing = Files.createTempFile("fv-export-missing-", ".ir")
    Files.delete(missing)

    val thrown = intercept[Throwable] {
      FVExport.exportFile(missing.toString)
    }

    assert(!thrown.isInstanceOf[FVExport.Unsupported])
  }
}
