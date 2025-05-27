package esmeta.web.http

import akka.http.scaladsl.model.{ContentTypes, HttpEntity}
import io.circe.Json

/** web server host */
val ESMETA_HOST = sys.env.getOrElse("ESMETA_HOST", "localhost")

extension (json: Json) {
  inline def asHttpEntity = HttpEntity(
    ContentTypes.`application/json`,
    json.noSpaces,
  )
}
