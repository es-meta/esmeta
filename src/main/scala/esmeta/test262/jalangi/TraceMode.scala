package esmeta.test262.jalangi

enum TraceMode:
  case Jalangi, NodeProf
  lazy val singleton: Set[TraceMode] = this match
    case Jalangi  => TraceMode.JalangiSet
    case NodeProf => TraceMode.NodeProfSet

object TraceMode:
  lazy val All = TraceMode.values.toSet
  lazy val Empty = Set.empty[TraceMode]
  lazy val JalangiSet = Set(TraceMode.Jalangi)
  lazy val NodeProfSet = Set(TraceMode.NodeProf)
