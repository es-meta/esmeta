package esmeta.util

enum ConcurrentPolicy:
  case Single
  case Fixed(nThread: Int)
  case Auto
