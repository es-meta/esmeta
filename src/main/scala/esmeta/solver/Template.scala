package esmeta.solver

import esmeta.ty.*

/** (partial) program of type `ty`, once its holes are filled */
// NOTE: structure for constraints on records' slots
case class Template(ty: ValueTy, expr: String, holes: List[Hole] = List())

enum Hole {
  case Slot(
    slotName: String,
    sink: List[String] = Nil,
    source: Option[String] = None,
  )
  case Base // the record the call is handed, not one it makes
  case Free(ty: ValueTy, named: String) // an argument the call needs anyway

  def slot: Option[String] = this match
    case Slot(slotName, _, _) => Some(slotName)
    case _                    => None

  // the surface writes $name where the hole goes, so the path names it
  def name: String = this match
    case Base                    => Hole.baseName
    case Free(_, named)          => named
    case Slot(slotName, sink, _) => (slotName :: sink).mkString(".")

}

object Hole {

  // the placeholders a hole is written under in the surface program
  val baseName = "Base"
  val argName = "Arg"

  // a sink step into a list; every other step is a record's field name
  val elemName = "Elem"

  /** the type a hole must be filled with, given what its slot must hold */
  def holeTy(
    slot: ValueTy,
    sink: List[String],
    source: Option[String],
  ): ValueTy = source match
    case Some(prop) =>
      ObjectT.copied(record =
        RecordTy.Elem(
          Map("Object" -> FieldMap.Top),
          ObjShape(Map(Property.PStr(prop) -> Desc(false, false, slot))),
        ),
      )
    case None =>
      sink.foldLeft(slot) { (t, step) =>
        if (step == elemName) t.list.elem else t.record(step).value
      }
}

// ---------------------------------------------------------------------------
// what the analysis reads off the IR
// ---------------------------------------------------------------------------

/** which record the write lands on */
enum Rec:
  case Created // the call makes one:            () -> T
  case Changed // it moves the one it was handed: S -> T

/** what lands in the slot */
enum Val:
  case Arg(idx: Int) // the caller's idx-th argument
  case Receiver // what the caller calls it on
  // no caller position holds it, so it becomes part of the type instead
  case Const(ty: ValueTy)

/** the slots a call writes, by which record they land on */
case class WriteSummary(
  created: List[Map[String, Set[Val]]] = Nil,
  changed: Map[Int, Map[String, Set[Val]]] = Map(),
)
