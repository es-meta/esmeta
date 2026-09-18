package esmeta.solver

import esmeta.cfg.Func
import esmeta.es.builtin.{INNER_CODE, intrAddr}
import esmeta.solver.Solver.{Invocation, getInvocation}
import esmeta.state.{Clo, RecordObj}
import esmeta.ty.*
import esmeta.util.BaseUtils.optional
import esmeta.util.ManualInfo
import io.circe.Json

/** derive indexed synthesis templates from pre-analysis returns */
class TemplateGenerator(analyzer: SymAnalyzer) {
  import TemplateGenerator.*
  import analyzer.{cfg => _, *}

  private val cfg = analyzer.cfg

  private def getTemplates(
    results: List[(InternalReturnPoint, (AbsValue, AbsState))],
  ): List[Template] = results
    .sortBy(_._1.node.id)
    .flatMap { (irp, result) =>
      val (value, st) = result
      optional(getTemplate(irp, value, st)).flatten
    }
    .distinctBy(t => (t.invocation, t.inputs, t.relation, t.value))

  private def getTemplate(
    irp: InternalReturnPoint,
    retV: AbsValue,
    st: AbsState,
  ): Option[Template] = {
    val (func, node) = (irp.func, irp.node)
    given NodePoint[?] = NodePoint(func, node, emptyView)
    val refined = transfer.refine(st.constr && retV.guard.lookup(NormalT))(st)
    if (refined.isBottom) return None
    val symty = retV.onlySym(using refined).symty match
      case SymTy.STy(ty) => SymTy.STy(ty && NormalT)
      case other         => other
    for {
      constr <- getInputConstr(analyzer)(symty, NormalT, refined)
      normalSt = transfer.refine(constr)(refined)
      if !normalSt.isBottom && normalSt.symEnv.values.forall(!_.isBottom)
      if symty.ty(using normalSt) ⊑ NormalT
      invocation <- getInvocation(analyzer)(func, normalSt)
      value = normalSt
        .get(AbsValue(symty), AbsValue(StrT("Value")))(using normalSt)
        .onlySym(using normalSt)
        .symty
      valueTy = value.ty(using normalSt)
      if !valueTy.isBottom && valueTy ⊑ ESValueT
      inputs = normalSt.constrForSyms
      relation = normalSt.constr.sexpr.filter(_.bases.forall {
        case sym: Int => inputs.contains(sym)
        case _        => false
      })
    } yield new Template(analyzer)(func, value, inputs, relation, invocation)
  }

  private val startTime = System.nanoTime()

  private val builtins = cfg.funcs.filter(_.isBuiltin).sortBy(_.id)

  // complete preparation before any synthesis request or target deadline
  private val templates: List[Template] = {
    val returns = analyzer.builtinResults.toList.groupBy(_._1.func)
    val results = builtins.flatMap { entry =>
      if (
        Solver.funcAccessExpr(entry).nonEmpty &&
        cfg.init.initHeap.map
          .get(intrAddr(entry.name.stripPrefix("INTRINSICS.")))
          .exists {
            case obj: RecordObj =>
              (obj.map.contains("Call") || obj.map.contains("Construct")) &&
              obj.map.get(INNER_CODE).exists {
                case Clo(code, _) => code == entry
                case _            => false
              }
            case _ => false
          }
      ) getTemplates(returns.getOrElse(entry, Nil))
      else Nil
    }
    analyzer.builtinResults.clear()
    results.filter(_.slots.nonEmpty)
  }

  val templatesBySlot: Map[String, List[Template]] =
    templates
      .flatMap { template => template.slots.toList.sorted.map(_ -> template) }
      .groupMap(_._1)(_._2)

  val summary: String = {
    val seconds = (System.nanoTime() - startTime) / 1e9
    f"Template generation: ${builtins.size} entries, " +
    f"${templates.size} templates, " +
    f"$seconds%.2f seconds\n"
  }

  /** synthesis candidates grouped by their lookup slots */
  lazy val json: Json = {
    val rendered = templates.map { template =>
      val forms = template.invocation.forms.map { (expr, holes) =>
        val (receiver, args) = holes.partition(_._1 == "#THIS")
        Json.fromFields(
          List(
            "template" -> Json.fromString(expr),
            "holes" -> Json.fromFields((args ++ receiver).map { (name, ty) =>
              name -> Json.fromString(ty.toString)
            }),
            "result" -> Json.fromString(template.value.toString),
          ) ++
          template.relation.map(r => "relation" -> Json.fromString(r.toString)),
        )
      }
      template -> forms
    }.toMap
    Json.fromFields(
      templatesBySlot.toList.sortBy(_._1).map { (slot, templates) =>
        val forms = templates.flatMap(rendered).distinct.sortBy(_.noSpaces)
        slot -> Json.arr(forms*)
      },
    )
  }
}

object TemplateGenerator {

  class Template(val analyzer: SymAnalyzer)(
    val entry: Func,
    val value: analyzer.SymTy,
    val inputs: Map[Int, ValueTy],
    val relation: Option[analyzer.SymExpr],
    val invocation: Invocation,
  ) {
    import analyzer.{cfg => _, *}

    private def inputState: AbsState =
      AbsState(true, Map.empty, inputs, TypeProp.Top, Effect.Bot)

    val returnTy: ValueTy = value.ty(using inputState)

    // index constructed records, not values forwarded from an input or call
    lazy val slots: Set[String] = value match {
      case SymTy.SRecord(_, fields) =>
        getSlots(returnTy) ++ fields.collect {
          case (field, symbolic)
              if symbolic.hasSym && !returnTy.record(field).value.isBottom =>
            field
        }
      case SymTy.STy(_) => getSlots(returnTy)
      case _            => Set.empty
    }

    def specialize(required: ValueTy): Option[Invocation] = {
      val st = inputState
      given NodePoint[?] = NodePoint(entry, entry.entry, emptyView)
      for {
        prop <- getInputConstr(analyzer)(value, required, st)
        refined = transfer.refine(TypeProp.Elem(Map(), relation) && prop)(st)
        if !refined.isBottom && refined.symEnv.values.forall(!_.isBottom)
        resultTy = value.ty(using refined)
        if !resultTy.isBottom && resultTy ⊑ required
        refinedInvocation <- getInvocation(analyzer)(entry, refined)
      } yield invocation.copy(
        thisTy = invocation.thisTy && refinedInvocation.thisTy,
        newTargetTy = invocation.newTargetTy && refinedInvocation.newTargetTy,
        paramTys =
          invocation.paramTys.zip(refinedInvocation.paramTys).map(_ && _),
        variadicTys =
          invocation.variadicTys.zip(refinedInvocation.variadicTys).map(_ && _),
      )(using analyzer.cfg)
    }
  }

  private def getInputConstr(analyzer: SymAnalyzer)(
    symty: analyzer.SymTy,
    ty: ValueTy,
    st: analyzer.AbsState,
  ): Option[analyzer.TypeProp] =
    import analyzer.*, SymTy.*
    given AbsState = st
    if (symty.ty ⊑ ty) Some(TypeProp.Top)
    else
      symty match
        case ref: SymRef => transfer.toBase(ref, ty).map(TypeProp(_))
        case SRecord(_, fields) =>
          (explicitFields(ty) ++ fields.keySet).toList.sorted
            .foldLeft(Option(TypeProp.Top)) {
              case (acc, field) =>
                acc.flatMap { prop =>
                  val required = ty.record(field).value
                  if (symty.ty.record(field).value ⊑ required) Some(prop)
                  else
                    for {
                      fieldSymty <- fields.get(field)
                      next <- getInputConstr(analyzer)(fieldSymty, required, st)
                    } yield prop && next
                }
            }
        case STy(_) => None

  private def explicitFields(ty: ValueTy): Set[String] = ty.record match
    case RecordTy.Elem(map, _) => map.values.flatMap(_.map.keySet).toSet
    case _                     => Set.empty

  // include explicit refinements and slot information implied by subtype names
  def getSlots(ty: ValueTy): Set[String] = ty.record match {
    case RecordTy.Elem(map, _) =>
      val model = ManualInfo.tyModel
      map.toList.flatMap { (name, fields) =>
        val base = model.baseOf(name)
        val declared = model.diffOf(base, name).toList.flatMap(_.fields)
        val record = RecordTy.Elem(Map(name -> fields))
        (fields.fields ++ declared).filter { field =>
          val binding = record(field)
          val general = model.getField(base, field)
          !binding.value.isBottom && binding != general && binding <= general
        }
      }.toSet
    case _ => Set.empty
  }

}
