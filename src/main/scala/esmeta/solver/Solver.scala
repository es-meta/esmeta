package esmeta.solver

import esmeta.cfg.Func
import esmeta.spec.*
import esmeta.state.*
import esmeta.ty.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import scala.collection.mutable.{Set => MSet}

trait Solver { self: SymInterp =>

  import tychecker.*, SymTy.*, Solver.*

  /** check the satisfiability of the given abstract state */
  def check: Boolean =
    val AbsState(reachable, locals, symEnv, constr) = st
    reachable &&
    symEnv.forall { case (sym, ty) => !ty.isBottom }

  /** reify a satisfiable path into an ECMAScript program */
  def reify: Option[String] = reifyAll.headOption

  def reifyAll: LazyList[String] =
    given AbsState = st
    // get constraints for each symbolic input
    val thisValue = st.getConstr(SThis.sym)
    val newTarget = st.getConstr(SNewTarget.sym)
    val args = entryFunc.head match
      case Some(h: BuiltinHead) =>
        val variadicAt = h.params.indexWhere(_.kind == ParamKind.Variadic)
        if (variadicAt < 0) // no variadic argument
          (0 until h.arity._2).toList.map(i => st.getConstr(i))
        else // contains variadic argument
          val fixed = h.params.indices
            .filter(_ != variadicAt)
            .map(i => st.getConstr(i))
            .toList
          val (before, after) = fixed.splitAt(variadicAt)
          // only a refined argument is in the environment
          st.symEnv.keysIterator.flatMap(variadicIdxOf).maxOption match
            case None => before ++ after
            case Some(i) =>
              val variadic = (0 to i).toList.map { k =>
                st.getConstr(SVariadicIdx(k).sym)
              }
              before ++ variadic ++ after
      case _ => Nil
    // reify into a JS program
    getPath(entryFunc) match
      case None       => LazyList.empty
      case Some(path) =>
        // get candidates from analyzed type
        val thisCands = synthesizer.candidates(thisValue)
        val argCands = args.map(synthesizer.candidates)
        val ntCands = newTargetForms(newTarget, synthesizer)
        // enumerate programs by varying one position at a time
        val slots = (thisCands +: argCands) :+ ntCands
        oneChange(slots).flatMap { chosen =>
          val thisV = chosen.head
          val vs = chosen.slice(1, 1 + args.length)
          val newTarget = chosen.last
          invoke(path, thisV, vs, newTarget)
        }
}

object Solver {

  def oneChange(slots: List[LazyList[String]]): LazyList[List[String]] =
    if (slots.exists(_.isEmpty)) LazyList.empty
    else {
      val heads = slots.map(_.head)
      def rounds(tails: List[LazyList[String]]): LazyList[List[String]] =
        if (tails.forall(_.isEmpty)) LazyList.empty
        else
          val round = for {
            (alts, i) <- LazyList.from(tails).zipWithIndex
            if alts.nonEmpty
          } yield heads.updated(i, alts.head)
          round #::: rounds(tails.map(_.drop(1)))
      heads #:: rounds(slots.map(_.drop(1)))
    }

  // lazy distinct
  def distinct(xs: LazyList[String]): LazyList[String] =
    val seen = MSet[String]()
    xs.filter(seen.add)

  def prioritize(
    rows: List[(ValueTy, List[String])],
    ty: ValueTy,
    synthesizer: TySynthesizer,
  ): LazyList[String] =
    distinct(rows.to(LazyList).flatMap(_._2).flatMap(fill(_, ty, synthesizer)))

  private def fill(
    template: String,
    ty: ValueTy,
    synthesizer: TySynthesizer,
  ): LazyList[String] =
    synthesizer.slotChoices(template, ty).fold(LazyList.empty) { choices =>
      val (fields, alts) = choices.unzip
      oneChange(alts).map { chosen =>
        fields.zip(chosen).foldLeft(template) {
          case (acc, (field, e)) => acc.replace("$" + field, e)
        }
      }
    }

  private def newTargetForms(
    ty: ValueTy,
    synthesizer: TySynthesizer,
  ): LazyList[String] =
    val ctor = synthesizer.candidates(ty && ConstructorT)
    if (UndefT ⊑ ty) "" #:: ctor else ctor // empty stands for no newTarget

  private def invoke(
    path: BuiltinPath,
    thisV: String,
    vs: List[String],
    newTarget: String,
  ): Option[String] =
    if (newTarget.isEmpty) { // without newTarget: XXX.call
      path match
        case BuiltinPath.YetPath(_) => None
        case BuiltinPath.Getter(base) =>
          descriptor(base).map(d => s"$d.get.call($thisV);")
        case BuiltinPath.Setter(base) =>
          val value = vs.headOption.getOrElse("undefined")
          descriptor(base).map(d => s"$d.set.call($thisV, $value);")
        case _ =>
          val args = (thisV :: vs).mkString(", ")
          access(path).map(fn => s"$fn.call($args);")
    } else { // with newTarget: Reflect.construct
      access(path).map { fn =>
        s"Reflect.construct($fn, [${vs.mkString(", ")}], $newTarget);"
      }
    }

  def getPath(func: Func): Option[BuiltinPath] = func.head match {
    case Some(h: BuiltinHead) => Some(h.path)
    case _                    => None
  }

  // JS expression to access a builtin function (None if unreachable)
  def funcAccessExpr(f: Func): Option[String] =
    f.head.collectFirst { case h: BuiltinHead => h.path }.flatMap(access)

  // JS expression accessing the builtin at path
  private def access(path: BuiltinPath): Option[String] = path match
    case BuiltinPath.Base(name) =>
      globalAlias.get(name) match
        case Some("")   => None // intrinsic unreachable from JS
        case Some(expr) => Some(expr)
        case None       => Some(name) // directly nameable global
    case BuiltinPath.NormalAccess(base, name) =>
      access(base).map(b => s"$b.$name")
    case BuiltinPath.SymbolAccess(base, sym) =>
      access(base).map(b => s"$b[Symbol.$sym]")
    case BuiltinPath.Getter(base) => access(base)
    case BuiltinPath.Setter(base) => access(base)
    case BuiltinPath.YetPath(_)   => None

  // Object.getOwnPropertyDescriptor(target, key) for a getter/setter base
  private def descriptor(base: BuiltinPath): Option[String] = base match
    case BuiltinPath.NormalAccess(b, n) =>
      val target = access(b)
      val key = s"\"${normStr(n)}\""
      target.map(t => s"Object.getOwnPropertyDescriptor($t, $key)")
    case BuiltinPath.SymbolAccess(b, s) =>
      val target = access(b)
      val key = s"Symbol.$s"
      target.map(t => s"Object.getOwnPropertyDescriptor($t, $key)")
    case _ => None

  // global alias for builtins that are not directly nameable but have a known JS expression to access them
  // https://github.com/tc39/test262/blob/main/harness/wellKnownIntrinsicObjects.js
  private val globalAlias: Map[String, String] = Map(
    "TypedArray" -> "Object.getPrototypeOf(Uint8Array)",
    "ArrayIteratorPrototype" -> "Object.getPrototypeOf([][Symbol.iterator]())",
    "AsyncFromSyncIteratorPrototype" -> "",
    "AsyncFunction" -> "(async function() {}).constructor",
    "AsyncGeneratorFunction" -> "(async function* () {}).constructor",
    "AsyncGeneratorPrototype" -> "Object.getPrototypeOf(async function* () {}).prototype",
    "AsyncIteratorPrototype" -> "Object.getPrototypeOf(Object.getPrototypeOf(async function* () {}).prototype)",
    "ForInIteratorPrototype" -> "",
    "GeneratorFunction" -> "(function* () {}).constructor",
    "GeneratorPrototype" -> "Object.getPrototypeOf(function * () {}).prototype",
    "IteratorHelperPrototype" -> "Object.getPrototypeOf(Iterator.from([]).drop(0))",
    "MapIteratorPrototype" -> "Object.getPrototypeOf(new Map()[Symbol.iterator]())",
    "SetIteratorPrototype" -> "Object.getPrototypeOf(new Set()[Symbol.iterator]())",
    "StringIteratorPrototype" -> "Object.getPrototypeOf(new String()[Symbol.iterator]())",
    "RegExpStringIteratorPrototype" -> """Object.getPrototypeOf(RegExp.prototype[Symbol.matchAll](""))""",
    "WrapForValidIteratorPrototype" -> "Object.getPrototypeOf(Iterator.from({ [Symbol.iterator](){ return {}; } }))",
    "ThrowTypeError" -> """(function() { "use strict"; return Object.getOwnPropertyDescriptor(arguments, "callee").get })()""",
  )
}
