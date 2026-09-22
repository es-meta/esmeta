package esmeta

/** switches that turn off one part of the solver for the ablation study
  *
  * They are set once by the `solve` phase and read at the few places listed
  * below, so that removing this file and those reads removes the study.
  *
  *   - `noShape`: `RecordTy.update` for a property, a call, or a construct
  *   - `noTemplate`: `TemplateGenerator.templatesBySlot`
  */
object Ablation {

  /** drop the object structure from the type domain */
  var noShape: Boolean = false

  /** drop the call templates derived from the specification */
  var noTemplate: Boolean = false

  /** the switches that are on, for the log */
  def enabled: List[String] =
    Option.when(noShape)("no-shape").toList :::
    Option.when(noTemplate)("no-template").toList
}
