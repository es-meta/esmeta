package esmeta.ai

import esmeta.error.*

/** exploded */
def exploded(msg: String): Nothing = throw AnalysisImprecise(msg)
