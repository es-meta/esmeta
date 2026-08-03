package esmeta.fv

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.security.MessageDigest
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

  private def directEmission(index: Int, main: Boolean = false) =
    FVInitState.DirectFunctionEmission(
      funId = s"f$index",
      source = s"Definition df_$index := $index.",
      ordinaryEntry = s"ordinary_$index",
      continuationEntry = s"continuation_$index",
      isMain = main,
      mainEntry = Option.when(main)(s"entry_$index"),
    )

  private def sha256(text: String): String =
    MessageDigest
      .getInstance("SHA-256")
      .digest(text.getBytes(StandardCharsets.UTF_8))
      .map("%02x".format(_))
      .mkString

  private def writeDirectArtifacts(
    formal: Path,
    artifacts: FVInitState.DirectSplitArtifacts,
  ): Unit = {
    artifacts.files.foreach { (name, content) =>
      write(formal.resolve(s"validation/spec_direct/$name"), content)
    }
    write(formal.resolve("validation/DirectSources.mk"), artifacts.manifest)
  }

  test(
    "render deterministic contiguous direct shards with separate provenance",
  ) {
    val functions =
      (0 until 5).toList.map(index => directEmission(index, main = index == 2))
    val first = FVInitState.renderDirectSplitArtifacts(functions, chunkSize = 2)
    val second =
      FVInitState.renderDirectSplitArtifacts(functions, chunkSize = 2)
    assert(first == second)
    assert(
      first.files.map(_._1) ==
      List(
        "DirectFuncs_0000.v",
        "DirectFuncs_0001.v",
        "DirectFuncs_0002.v",
        "DirectFuncs.v",
        "DirectNames.v",
      ),
    )
    assert(
      first.manifest.contains("DIRECT_GENERATOR_PROVENANCE := direct-itree-v1"),
    )
    assert(first.manifest.contains("DIRECT_GENERATED_SOURCES :="))
    assert(!first.manifest.contains("SPEC_GENERATED_SOURCES"))
    assert(
      first.files.map((name, content) => name -> sha256(content)) ==
      second.files.map((name, content) => name -> sha256(content)),
    )
    val facade = first.files.toMap.apply("DirectFuncs.v")
    assert(facade.contains("Definition direct_ir_funid_fnsems"))
    assert(facade.contains("Definition direct_ir_cont_fnsems"))
    assert(facade.contains("Definition direct_ir_entry := entry_2"))
    assert(facade.contains("list_to_map (direct_ir_entry ::"))
  }

  test(
    "validate direct ordinary and continuation domains and entry separately",
  ) {
    FVInitState.validateDirectDomains(
      expectedFunIds = List("a", "b"),
      ordinaryFunIds = List("b", "a"),
      continuationFunIds = List("a", "b"),
      hasMain = true,
      mainEntryCount = 1,
    )
    val missingContinuation = intercept[IllegalStateException] {
      FVInitState.validateDirectDomains(
        List("a", "b"),
        List("a", "b"),
        List("a"),
        hasMain = true,
        mainEntryCount = 1,
      )
    }
    assert(
      missingContinuation.getMessage.contains("continuation domain mismatch"),
    )
    val duplicateOrdinary = intercept[IllegalStateException] {
      FVInitState.validateDirectDomains(
        List("a", "b"),
        List("a", "b", "b"),
        List("a", "b"),
        hasMain = false,
        mainEntryCount = 0,
      )
    }
    assert(duplicateOrdinary.getMessage.contains("duplicate direct ordinary"))
    val unexpectedEntry = intercept[IllegalStateException] {
      FVInitState.validateDirectDomains(
        List("a"),
        List("a"),
        List("a"),
        hasMain = false,
        mainEntryCount = 1,
      )
    }
    assert(unexpectedEntry.getMessage.contains("direct entry mismatch"))
  }

  test("reject direct manifest gaps duplicates and unmanifested shards") {
    val artifacts = FVInitState.renderDirectSplitArtifacts(
      (0 until 3).toList.map(directEmission(_)),
      chunkSize = 1,
    )

    val gap = Files.createTempDirectory("fv-direct-gap-")
    writeDirectArtifacts(gap, artifacts)
    val gapManifest = artifacts.manifest.linesIterator
      .filterNot(_.contains("DirectFuncs_0001.v"))
      .mkString("\n") + "\n"
    write(gap.resolve("validation/DirectSources.mk"), gapManifest)
    val gapError = intercept[IllegalStateException] {
      FVInitState.validateDirectSplitSpecBase(gap.toFile)
    }
    assert(gapError.getMessage.contains("non-contiguous"))

    val duplicate = Files.createTempDirectory("fv-direct-duplicate-")
    writeDirectArtifacts(duplicate, artifacts)
    val duplicatedManifest = artifacts.manifest.replace(
      "  validation/spec_direct/DirectFuncs_0001.v \\",
      "  validation/spec_direct/DirectFuncs_0001.v \\\n" +
      "  validation/spec_direct/DirectFuncs_0001.v \\",
    )
    write(duplicate.resolve("validation/DirectSources.mk"), duplicatedManifest)
    val duplicateError = intercept[IllegalStateException] {
      FVInitState.validateDirectSplitSpecBase(duplicate.toFile)
    }
    assert(duplicateError.getMessage.contains("duplicate source entries"))

    val extra = Files.createTempDirectory("fv-direct-extra-")
    writeDirectArtifacts(extra, artifacts)
    write(extra.resolve("validation/spec_direct/DirectFuncs_0003.v"))
    val extraError = intercept[IllegalStateException] {
      FVInitState.validateDirectSplitSpecBase(extra.toFile)
    }
    assert(extraError.getMessage.contains("unmanifested"))
  }

  test(
    "validate a complete direct file set without touching generic artifacts",
  ) {
    val formal = Files.createTempDirectory("fv-direct-valid-")
    val generic = formal.resolve("validation/spec/SpecFuncs_0000.v")
    write(generic, "generic sentinel\n")
    val artifacts = FVInitState.renderDirectSplitArtifacts(
      List(directEmission(0, main = true), directEmission(1)),
      chunkSize = 1,
    )
    writeDirectArtifacts(formal, artifacts)

    assert(
      FVInitState
        .validateDirectSplitSpecBase(formal.toFile)
        .map(_.getName)
        .toSet ==
      Set(
        "DirectFuncs.v",
        "DirectFuncs_0000.v",
        "DirectFuncs_0001.v",
        "DirectNames.v",
      ),
    )
    assert(Files.readString(generic) == "generic sentinel\n")
  }
}
