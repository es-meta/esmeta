package esmeta.peval.domain

enum PValueExistence:
  case TOP(pv: PValue)
  case PV(pv: PValue)
  case Uninit
  case BOT
