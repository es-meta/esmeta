package esmeta.es.builtin

import esmeta.state.*

/** builtin model structure */
case class Struct(
  typeName: String,
  imap: List[(String, Value)] = List(),
  nmap: List[(String, Property)] = List(),
)
