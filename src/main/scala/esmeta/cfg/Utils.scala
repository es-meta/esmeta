package esmeta.cfg

/** specification utilities */
object Utils {

  /** extensions for functions */
  extension (func: Func) {

    /** check whether it is builtin */
    def isBuiltin: Boolean = func.kind == Func.Kind.Builtin
  }
}
