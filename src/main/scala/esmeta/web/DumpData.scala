package esmeta.web

import io.circe.Json

case class DumpData(program: Json, spec: Json, grammar: Json)
