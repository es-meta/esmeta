package esmeta.cfg

/** specification utilities */
object Utils {

  /** extensions for functions */
  extension (func: Func) {
    def isBuiltin: Boolean = func.kind == Func.Kind.Builtin
  }
}
