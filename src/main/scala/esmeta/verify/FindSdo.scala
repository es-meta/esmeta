package esmeta.verify

import esmeta.*
import esmeta.cfg.*

class FindSdo(
  cfg: CFG,
  target: Func,
) extends analyzer.tychecker.TyChecker(
    cfg,
    targetPattern = None,
    inferTypeGuard = true,
    typeSens = false,
    config = analyzer.tychecker.TyChecker.Config(),
    ignore = analyzer.tychecker.TyChecker.Ignore(),
    log = false,
    detail = false,
    silent = false,
    useRepl = false,
    replContinue = false,
  ) {
  override def targetFuncs: List[Func] = List(target)
}
