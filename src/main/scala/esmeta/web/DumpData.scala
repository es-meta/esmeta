package esmeta.web

import io.circe.Json

case class DumpData(
  program: Json,
  funcs: Json,
  spec: Json,
)
