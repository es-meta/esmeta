package esmeta.es.util.dsl

import esmeta.lang.*
import esmeta.lang.util.{UnitWalker => LangUnitWalker}

import scala.collection.mutable

object Analyzer {
  def buildContext(body: Step): DSLContext = {
    DSLContext(
      searchVariablesWithSlot(body, "SetData").map((_ -> "SetData")).toMap ++
      searchVariablesWithSlot(body, "MapData").map((_ -> "MapData")).toMap,
    )
  }

  private def searchVariablesWithSlot(step: Step, slot: String): Set[String] = {
    val result = mutable.Set[String]()
    new LangUnitWalker {
      override def walk(step: Step): Unit = step match
        case LetStep(
              Variable(v, _),
              ListCopyExpression(
                ReferenceExpression(Access(_, s, _, _)),
              ),
            ) if s == slot =>
          result.add(v)
        case LetStep(
              Variable(v, _),
              ReferenceExpression(Access(_, s, _, _)),
            ) if s == slot =>
          result.add(v)
        case SetStep(
              Access(_, s, _, _),
              ReferenceExpression(Variable(v, _)),
            ) if s == slot =>
          result.add(v)
        case _ => super.walk(step)
    }.walk(step)
    result.toSet
  }
}
