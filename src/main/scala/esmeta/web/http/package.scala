package esmeta.web.http

import cats.effect.*
import io.circe.*, io.circe.syntax.*
import org.http4s.MediaType
import org.http4s.dsl.io.*
import org.http4s.headers.`Content-Type`

object models {
  type BpData = (Boolean, String, List[Int], Boolean)

  type AddBreakpointRequest = BpData
  type RunRequest = (String, List[BpData])
  type ResumeFromIterRequest = (String, List[BpData], Int)
}

extension [T: Encoder](t: T)
  def asJsonOk = Ok(t.asJson.noSpaces)
    .map(_.withContentType(`Content-Type`(MediaType.application.json)))
