package esmeta.test262.jalangi

import esmeta.util.SystemUtils.*

object Diff {
  enum ColorOption:
    case Always, Never, Auto
    override def toString(): String = this match
      case Always => "always"
      case Never  => "never"
      case Auto   => "auto"

  // TODO check if this works on various a and b inputs
  import java.nio.file.{Files, Paths}
  import java.nio.charset.StandardCharsets

  def get(
    a: String,
    b: String,
    colorOpt: ColorOption = ColorOption.Always,
  ): String = {
    val fileA = Files.createTempFile("a", ".txt")
    val fileB = Files.createTempFile("b", ".txt")
    Files.write(fileA, a.getBytes(StandardCharsets.UTF_8))
    Files.write(fileB, b.getBytes(StandardCharsets.UTF_8))
    try {
      executeCmdNonZero(
        s"diff -y --color=${colorOpt.toString} ${fileA.toAbsolutePath} ${fileB.toAbsolutePath}",
      )._2
    } finally {
      Files.deleteIfExists(fileA)
      Files.deleteIfExists(fileB)
    }
  }
}
