package esmeta.error

import esmeta.VERSION

class ESMetaError(
  val errMsg: String,
  val tag: String = s"ESMeta v$VERSION",
) extends Error(s"[$tag] $errMsg")
