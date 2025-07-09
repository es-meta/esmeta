package esmeta.web.http

import io.circe.*, io.circe.syntax.*
import zio.*
import zio.http.*

/** web server host */
val ESMETA_HOST = sys.env.getOrElse("ESMETA_HOST", "localhost")

extension [T: Encoder](t: T) {
  inline def asJsonResponse = Response
    .text(t.asJson.noSpaces)
    .addHeader(Header.ContentType(MediaType.application.json))
}
