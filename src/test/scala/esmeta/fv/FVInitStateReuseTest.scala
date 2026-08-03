package esmeta.fv

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import org.scalatest.funsuite.AnyFunSuite

class FVInitStateReuseTest extends AnyFunSuite {
  private def write(path: Path, content: String = ""): Unit = {
    Files.createDirectories(path.getParent)
    Files.writeString(path, content, StandardCharsets.UTF_8)
  }

  private def reusableBase(entries: List[String]): Path = {
    val formal = Files.createTempDirectory("fv-reuse-base-")
    write(formal.resolve("validation/Spec.v"))
    entries.foreach(entry => write(formal.resolve(entry)))
    for (prefix <- List("SpecFuncs", "SpecHeap")) {
      val modules = entries.collect {
        case entry if entry.matches(s"validation/spec/${prefix}_\\d{4}\\.v") =>
          entry.stripPrefix("validation/spec/").stripSuffix(".v")
      }
      write(
        formal.resolve(s"validation/spec/$prefix.v"),
        s"From ESMetaFV.validation.spec Require Export ${modules.mkString(" ")}.\n",
      )
    }
    val manifest =
      "SPEC_GENERATED_SOURCES := \\\n" +
      entries.zipWithIndex.map { (entry, index) =>
        val continuation = if (index + 1 < entries.size) " \\" else ""
        s"  $entry$continuation\n"
      }.mkString
    write(formal.resolve("validation/SpecSources.mk"), manifest)
    formal
  }

  private val facades = List(
    "validation/spec/SpecFuncs.v",
    "validation/spec/SpecGlobals.v",
    "validation/spec/SpecHeap.v",
  )

  test("validate every reusable split Spec source") {
    val entries = facades ++ List(
      "validation/spec/SpecFuncs_0000.v",
      "validation/spec/SpecFuncs_0001.v",
      "validation/spec/SpecHeap_0000.v",
    )
    val formal = reusableBase(entries)

    assert(
      FVInitState
        .validateReusableSplitSpecBase(formal.toFile)
        .map(_.toPath.toAbsolutePath.normalize) ==
      entries.map(formal.resolve(_).toAbsolutePath.normalize),
    )
  }

  test("reject a missing reusable split Spec shard") {
    val missing = "validation/spec/SpecFuncs_0001.v"
    val entries = facades ++ List(
      "validation/spec/SpecFuncs_0000.v",
      missing,
      "validation/spec/SpecHeap_0000.v",
    )
    val formal = reusableBase(entries)
    Files.delete(formal.resolve(missing))

    val error = intercept[IllegalStateException] {
      FVInitState.validateReusableSplitSpecBase(formal.toFile)
    }
    assert(error.getMessage.contains("--reuse-test262-base"))
    assert(error.getMessage.contains(missing))
  }

  test("reject unsafe and prefix-lookalike manifest entries") {
    val validShards = List(
      "validation/spec/SpecFuncs_0000.v",
      "validation/spec/SpecHeap_0000.v",
    )
    for (
      entry <- List(
        "../SpecFuncs_0000.v",
        "validation/spec/SpecFuncs_backup.v",
        "validation/spec/SpecHeap_00000.v",
      )
    ) {
      val formal = reusableBase(facades ++ validShards :+ entry)
      val error = intercept[IllegalStateException] {
        FVInitState.validateReusableSplitSpecBase(formal.toFile)
      }
      assert(error.getMessage.contains("unsafe or unexpected source entry"))
    }
  }

  test("reject a manifest with no numbered function or heap shards") {
    val formal = reusableBase(facades)
    val error = intercept[IllegalStateException] {
      FVInitState.validateReusableSplitSpecBase(formal.toFile)
    }
    assert(error.getMessage.contains("contains no SpecFuncs shard entries"))
  }

  test("reject facade and manifest shard drift in either direction") {
    val entries = facades ++ List(
      "validation/spec/SpecFuncs_0000.v",
      "validation/spec/SpecFuncs_0001.v",
      "validation/spec/SpecHeap_0000.v",
    )

    val missingImport = reusableBase(entries)
    write(
      missingImport.resolve("validation/spec/SpecFuncs.v"),
      "From ESMetaFV.validation.spec Require Export SpecFuncs_0000.\n",
    )
    val missingError = intercept[IllegalStateException] {
      FVInitState.validateReusableSplitSpecBase(missingImport.toFile)
    }
    assert(missingError.getMessage.contains("facade/manifest shard mismatch"))
    assert(missingError.getMessage.contains("SpecFuncs_0001.v"))

    val unmanifestedImport = reusableBase(
      entries.filterNot(_.endsWith("SpecFuncs_0001.v")),
    )
    write(unmanifestedImport.resolve("validation/spec/SpecFuncs_0001.v"))
    write(
      unmanifestedImport.resolve("validation/spec/SpecFuncs.v"),
      "From ESMetaFV.validation.spec Require Export " +
      "SpecFuncs_0000 SpecFuncs_0001.\n",
    )
    val extraError = intercept[IllegalStateException] {
      FVInitState.validateReusableSplitSpecBase(unmanifestedImport.toFile)
    }
    assert(extraError.getMessage.contains("facade/manifest shard mismatch"))
    assert(extraError.getMessage.contains("SpecFuncs_0001.v"))
  }
}
