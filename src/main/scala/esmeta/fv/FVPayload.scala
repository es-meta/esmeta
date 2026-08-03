package esmeta.fv

import esmeta.fv.FVExport.Unsupported
import java.io.{ByteArrayOutputStream, DataOutputStream}
import java.nio.CharBuffer
import java.nio.charset.{CodingErrorAction, StandardCharsets}
import java.nio.file.{
  AtomicMoveNotSupportedException,
  Files,
  Path,
  StandardCopyOption,
}

/** Versioned, compact input format for the extracted Test262 ITree runner.
  *
  * The Rocq-extracted semantics remains the executable definition.  This
  * format only replaces the former path that reified every test tuple as a
  * large Rocq term and then typechecked, extracted, compiled and linked it.
  *
  * Multi-byte integers are big-endian.  ECMAScript strings are encoded as
  * their exact UTF-16 code units; metadata strings use strict UTF-8.
  */
private[fv] object FVPayload {
  val Magic: Array[Byte] = "ESFVIT07".getBytes(StandardCharsets.US_ASCII)

  final class Encoder private[FVPayload] (
    private val out: DataOutputStream,
  ) {
    def tag(value: Int): Unit = {
      if (value < 0 || value > 255)
        throw IllegalArgumentException(s"payload tag out of range: $value")
      out.writeByte(value)
    }

    def bool(value: Boolean): Unit = tag(if value then 1 else 0)

    def u16(value: Int): Unit = {
      if (value < 0 || value > 0xffff)
        throw IllegalArgumentException(
          s"payload uint16 out of range: $value",
        )
      out.writeShort(value)
    }

    def nat(value: Int): Unit = {
      if (value < 0)
        throw IllegalArgumentException(
          s"payload natural out of range: $value",
        )
      out.writeInt(value)
    }

    def utf8(value: String): Unit = {
      val encoder = StandardCharsets.UTF_8
        .newEncoder()
        .onMalformedInput(CodingErrorAction.REPORT)
        .onUnmappableCharacter(CodingErrorAction.REPORT)
      val buffer =
        try encoder.encode(CharBuffer.wrap(value))
        catch {
          case error: java.nio.charset.CharacterCodingException =>
            throw Unsupported(
              s"payload metadata is not valid UTF-8: ${error.getMessage}",
            )
        }
      val bytes = new Array[Byte](buffer.remaining)
      buffer.get(bytes)
      nat(bytes.length)
      out.write(bytes)
    }

    /** Preserve lone surrogates and every other ECMAScript code unit. */
    def cstr(value: String): Unit = {
      val units = value.toCharArray
      nat(units.length)
      units.foreach(unit => u16(unit.toInt))
    }

    def integer(value: scala.math.BigInt): Unit = utf8(value.toString)

    /** Match FVExport.floatLit exactly: finite values retain their bits
      * (including signed zero), while NaN is canonicalized by the extracted
      * PrimFloat.nan constant instead of preserving an unobservable payload.
      */
    def float64(value: Double): Unit =
      if value.isNaN then tag(1)
      else if value == Double.PositiveInfinity then tag(2)
      else if value == Double.NegativeInfinity then tag(3)
      else {
        tag(0)
        out.writeLong(java.lang.Double.doubleToRawLongBits(value))
      }

    def list[A](values: Iterable[A])(write: A => Unit): Unit = {
      val stable = values.toList
      nat(stable.length)
      stable.foreach(write)
    }
  }

  /** Build one complete payload in memory so failed representation leaves no
    * partial file and can be classified as NOT_REPRESENTABLE.
    */
  def encode(globalIndex: Int)(write: Encoder => Unit): Array[Byte] = {
    val bytes = new ByteArrayOutputStream()
    val out = new DataOutputStream(bytes)
    out.write(Magic)
    val encoder = new Encoder(out)
    encoder.nat(globalIndex)
    write(encoder)
    out.flush()
    bytes.toByteArray
  }

  /** Replace a payload atomically; a killed export cannot leave a truncated
    * file that a persistent worker might consume.
    */
  def writeAtomic(path: Path, bytes: Array[Byte]): Unit = {
    val parent = path.toAbsolutePath.getParent
    Files.createDirectories(parent)
    val temporary = Files.createTempFile(parent, s".${path.getFileName}.", ".tmp")
    try {
      Files.write(temporary, bytes)
      try
        Files.move(
          temporary,
          path,
          StandardCopyOption.ATOMIC_MOVE,
          StandardCopyOption.REPLACE_EXISTING,
        )
      catch {
        case _: AtomicMoveNotSupportedException =>
          Files.move(
            temporary,
            path,
            StandardCopyOption.REPLACE_EXISTING,
          )
      }
    } finally Files.deleteIfExists(temporary)
  }
}
