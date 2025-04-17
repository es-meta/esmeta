package worker

import io.circe.*, io.circe.syntax.*, io.circe.generic.semiauto.*;
import io.circe.parser.decode

import scala.concurrent.{Future, Promise as SPromise}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js;

package object util {

  trait Input extends js.Object {
    val funcs: String
    val version: String
    val grammar: String
    val tables: String
    val tyModel: String
  }

  def decodeWithMeasure[T](
    tag: String,
  )(json: String)(using Decoder[T]): Future[T] = Future {
    val start = System.currentTimeMillis()
    val result =
      decode[T](json) match
        case Left(err) =>
          println(s"${tag} Decoding failed: ${err}")
          throw new Exception(s"Failed to decode ${tag}")
        case Right(value) =>
          value
    val end = System.currentTimeMillis()
    println(s"${tag} Decoded successfully, Time taken: ${end - start} ms")
    result
  }

  def withMeasure[T](tag: String)(f: => T): T = {
    val start = System.currentTimeMillis()
    val result = f
    val end = System.currentTimeMillis()
    println(s"${tag} done, Time taken: ${end - start} ms")
    result
  }

  inline def benchmark[T](f: => T)(log: Long => Unit): (T) = {
    val start = System.currentTimeMillis()
    val result = f
    val end = System.currentTimeMillis()
    log(end - start)
    result
  }

  def measureFutureTime[T](future: Future[T]): Future[T] = {
    val startTime = System.nanoTime()
    future.map { result =>
      val endTime = System.nanoTime()
      val duration = endTime - startTime
      println(s"Time taken for total build: ${duration / 1000000} ms")
      (result)
    }
  }

}
