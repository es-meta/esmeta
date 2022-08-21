package esmeta.ai

// TODO import esmeta.analyzer.domain.*
import esmeta.ai.sensitivity.*
import esmeta.cfg.*
import esmeta.es.*

/** meta-level static analyzer for ECMAScript */
case class ESAnalyzer(
  sensitivity: Sensitivity, // analysis sensitivity
  // TODO
  // retDomain: RetDomain, // abstract return values and states domain
  // stateDomain: StateDomain, // abstract domain
  // heapDomain: HeapDomain, // abstract domain
  // objDomain: ObjDomain, // abstract domain
  // valueDomain: ValueDomain, // abstract domain
  // compDomain: CompDomain, // abstract domain
  // cloDomain: CloDomain, // abstract domain
  // contDomain: ContDomain, // abstract domain
  // locDomain: LocDomain, // abstract domain
  // astDomain: AstDomain, // abstract domain
  // grammarDomain: GrammarDomain, // abstract domain
  // codeUnitDomain: CodeUnitDomain, // abstract domain
  // constDomain: ConstDomain, // abstract domain
  // mathDomain: MathDomain, // abstract domain
  // simpleDomain: SimpleDomain, // abstract domain
  // numDomain: NumDomain, // abstract domain
  // bigIntDomain: BigIntDomain, // abstract domain
  // strDomain: StrDomain, // abstract domain
  // boolDomain: BoolDomain, // abstract domain
  // undefDomain: UndefDomain, // abstract domain
  // nullDomain: NullDomain, // abstract domain
  // absentDomain: AbsentDomain, // abstract domain
)(
  val script: Ast,
) extends Analyzer {

  /** abstract transfer function */
  // TODO val transfer: AbsTransfer = ???

  /** perform analysis */
  // TODO def apply(cfg: CFG): AbsSemantics = ???
}
