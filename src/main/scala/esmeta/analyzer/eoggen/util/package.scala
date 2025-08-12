package esmeta.analyzer.eoggen.util

import esmeta.analyzer.eoggen.*

type Self = EOGGenUtil & EOGGenerator

trait EOGGenUtil extends EOGDecl {
  self: Self =>
}
