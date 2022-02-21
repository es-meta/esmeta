package esmeta.js.builtin

import esmeta.interp.*

/** builtin model structure */
case class Struct(
  typeName: String,
  imap: List[(String, PureValue)] = List(),
  nmap: List[(String, Property)] = List(),
)
