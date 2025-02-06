package esmeta.analyzer.es

trait AbsTransferDecl { analyzer: ESAnalyzer =>

  /** abstract transfer function */
  class AbsTransfer extends AbsTransferLike {

    /** loading monads */
    import monad.*

    /** loading predefined abstract values */
    import AbsValue.*

    // =========================================================================
    // Implementation for General AbsTransfer
    // =========================================================================
    def apply(np: NodePoint[?]): Unit = ???
    def apply(rp: ReturnPoint): Unit = ???
    def transfer(inst: esmeta.ir.NormalInst)(using
      np: NodePoint[?],
    ): monad.Updater = ???
    def transfer(expr: esmeta.ir.Expr)(using
      np: NodePoint[esmeta.cfg.Node],
    ): monad.Result[AbsValue] = ???
    def transfer(
      st: AbsState,
      unary: esmeta.ir.EUnary,
      operand: AbsValue,
    )(using
      np: NodePoint[esmeta.cfg.Node],
    ): AbsValue = ???
    def transfer(
      st: AbsState,
      binary: esmeta.ir.EBinary,
      left: AbsValue,
      right: AbsValue,
    )(using
      np: NodePoint[esmeta.cfg.Node],
    ): AbsValue = ???
    def transfer(
      st: AbsState,
      vop: esmeta.ir.VOp,
      vs: List[AbsValue],
    )(using
      np: NodePoint[esmeta.cfg.Node],
    ): AbsValue = ???
    def transfer(
      st: AbsState,
      mop: esmeta.ir.MOp,
      vs: List[AbsValue],
    )(using
      np: NodePoint[esmeta.cfg.Node],
    ): AbsValue = ???
  }
}
