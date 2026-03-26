package esmeta.es.util.dsl

import esmeta.lang.*
import esmeta.lang.util.{UnitWalker => LangUnitWalker}

import scala.collection.mutable

object Analyzer {
  def buildContext(body: Step): DSLContext = {
    DSLContext(
      variableTypes = searchVariablesWithSlot(body, "SetData")
        .map((_ -> "SetData"))
        .toMap ++
        searchVariablesWithSlot(body, "MapData")
          .map((_ -> "MapData"))
          .toMap,
      copyOf = searchCopyRelationships(body),
    )
  }

  private def searchVariablesWithSlot(
    step: Step,
    slot: String,
  ): Set[String] = {
    val result = mutable.Set[String]()
    new LangUnitWalker {
      override def walk(step: Step): Unit = step match
        case LetStep(
              Variable(v, _),
              ListCopyExpression(
                ReferenceExpression(Access(_, s, _, _)),
              ),
            ) if s.endsWith(slot) =>
          result.add(v)
        case LetStep(
              Variable(v, _),
              ReferenceExpression(Access(_, s, _, _)),
            ) if s.endsWith(slot) =>
          result.add(v)
        case SetStep(
              Access(_, s, _, _),
              ReferenceExpression(Variable(v, _)),
            ) if s.endsWith(slot) =>
          result.add(v)
        case _ => super.walk(step)
    }.walk(step)
    result.toSet
  }

  /** Search for copy relationships: `Let X = copy of Y` and verify no mutation
    * of Y occurs between the copy and the end of the enclosing StepBlock.
    *
    * Copy relationships are recorded immediately and propagate into nested
    * blocks. If a mutation is found later in the same block, the relationship
    * is invalidated.
    */
  private def searchCopyRelationships(
    body: Step,
  ): Map[String, Reference] = {
    val result = mutable.Map[String, Reference]()

    new LangUnitWalker {
      override def walk(sb: StepBlock): Unit = {
        // Track active copies in this block for mutation checking
        val activeInBlock = mutable.Map[String, Reference]()

        for (subStep <- sb.steps) {
          val step = subStep.step
          step match {
            // Let X = a copy of Y
            case LetStep(
                  Variable(v, _),
                  ListCopyExpression(ReferenceExpression(ref)),
                ) =>
              activeInBlock(v) = ref
              result(v) = ref

            // Let X = IN__SetDataCopy(Y) (post-transformation form)
            case LetStep(
                  Variable(v, _),
                  InvokeAbstractOperationExpression(
                    "IN__SetDataCopy",
                    List(ReferenceExpression(ref)),
                    _,
                  ),
                ) =>
              activeInBlock(v) = ref
              result(v) = ref

            case _ =>
              // Check if this step mutates any active source ref
              val mutatedRefs = findMutatedRefs(step)
              for ((v, sourceRef) <- activeInBlock.toList) {
                if (mutatedRefs.exists(refEquals(_, sourceRef))) {
                  activeInBlock.remove(v)
                  result.remove(v)
                }
              }
          }

          // Recurse into child structures (IfStep bodies, etc.)
          // Copy relationships from outer blocks are already in result
          walk(step)
        }
      }
    }.walk(body)
    result.toMap
  }

  /** Find references that are mutated by this step (top-level only). */
  private def findMutatedRefs(step: Step): List[Reference] = step match {
    case AppendStep(_, ref)     => List(ref)
    case PrependStep(_, ref)    => List(ref)
    case SetStep(ref, _)        => List(ref)
    case RemoveStep(_, _, _)    => List()
    case ReplaceStep(_, _, ref) => List(ref)
    case InsertStep(_, ref)     => List(ref)
    case AddStep(_, ref)        => List(ref)
    case _                      => List()
  }

  /** Check if two references refer to the same location. */
  private def refEquals(a: Reference, b: Reference): Boolean =
    (a, b) match {
      case (Variable(n1, _), Variable(n2, _)) => n1 == n2
      case (
            Access(b1, n1, _, _),
            Access(b2, n2, _, _),
          ) =>
        n1 == n2 && refEquals(b1, b2)
      case (IndexLookup(b1, _), IndexLookup(b2, _)) =>
        refEquals(b1, b2)
      case _ => a == b
    }
}
