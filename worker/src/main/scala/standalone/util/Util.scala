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
    val funcsCfg: String
  }

  def allRight[L, R](list: List[Either[L, R]]): Either[List[L], List[R]] = {
    val (ls, rs) = list.foldRight(List[L]() -> List[R]()) {
      case either -> (ls, rs) =>
        either match
          case Left(l) =>
            (l :: ls) -> rs
          case Right(r) =>
            ls -> (r :: rs)
    }
    if (ls.isEmpty) Right(rs)
    else Left(ls)
  }

  def decodeWithMeasure[T](
    tag: String,
  )(json: String)(using Decoder[T]): Future[T] = futureWithErrorLog {
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

  def decodeListWithMeasure[T](
    tag: String,
  )(
    json: String,
    rateCallback: js.UndefOr[js.Function1[Double, Unit]] = js.undefined,
  )(using Decoder[T]): Future[List[T]] =
    futureWithErrorLog {
      val start = System.currentTimeMillis()
      val result =
        decode[List[Json]](json) match
          case Left(err) =>
            println(s"${tag} Decoding failed: ${err}")
            throw new Exception(s"Failed to decode ${tag}")
          case Right(jsons) =>
            jsons.zipWithIndex.map {
              case (json, idx) =>
                decode[T](json.noSpaces) match
                  case Left(err) =>
                    println(s"${tag} Decoding failed for item: ${err}")
                    throw new Exception(s"Failed to decode item in ${tag}")
                  case Right(value) =>
                    rateCallback.foreach { callback =>
                      callback(idx.toDouble / jsons.length.toDouble)
                    }
                    value
            }
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

  def withMeasureFuture[T](tag: String)(f: => T): Future[T] = futureWithErrorLog {
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

  def futureWithErrorLog[T](f: => T): Future[T] = Future {
    try {
      f
    } catch {
      case e: Throwable =>
        println(s"Error occurred: ${e.getMessage}")
        throw e
    }
  }

}
