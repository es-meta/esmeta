package esmeta.web.http

/** web server host */
val ESMETA_HOST = sys.env.getOrElse("ESMETA_HOST", "localhost")

object models {
  type BpData = (Boolean, String, List[Int], Boolean)

  type AddBreakpointRequest = BpData
  type RunRequest = (String, List[BpData])
  type ResumeFromIterRequest = (String, List[BpData], Int)
}
