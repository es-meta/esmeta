package esmeta.solver

import esmeta.analyzer.tychecker.TyChecker
import esmeta.cfg.{Branch, CFG, Func}
import esmeta.es.builtin.intrAddr
import esmeta.spec.*
import esmeta.state.*
import esmeta.ty.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import scala.collection.mutable.{Set => MSet}
import java.util.concurrent.TimeoutException

trait Solver { self: SymInterp =>

  import tychecker.*

  /** check the satisfiability of the given abstract state */
  def check: Boolean =
    st.reachable && st.symEnv.forall((_, ty) => !ty.isBottom)

  /** reify a satisfiable path into an ECMAScript program */
  def reify: Option[String] = reifyAll.headOption

  def reifyAll: LazyList[String] = reifyAll(wrap)

  def reifyAll(checkTimeout: () => Unit): LazyList[String] =
    reifyAll(wrap, checkTimeout)

  def reifyAll(config: Config): LazyList[String] =
    reifyAll(config, () => if (timeout) throw TimeoutException("solver"))

  def reifyAll(
    config: Config,
    checkTimeout: () => Unit,
  ): LazyList[String] = {
    // config.conds excludes the target branch
    val target = config.node match
      case branch: Branch => List(branch)
      case _              => Nil
    val literals =
      synthesizer.literals(target ++ config.conds.map(_.branch))
    Solver
      .getTemplate(tychecker)(entryFunc, config.state)
      .to(LazyList)
      .flatMap { template =>
        synthesizer.candidates(template)(using
          literals,
          Set.empty,
          checkTimeout,
        )
      }
      .map(_ + ";")
  }
}

object Solver {

  /** extract a template and its input types */
  def getTemplate(tychecker: TyChecker)(
    entryFunc: Func,
    st: tychecker.AbsState,
  ): Option[Template] =
    import tychecker.*, SymTy.*
    given AbsState = st
    // get constraints for each symbolic input
    val thisTy = st.getConstr(SThis.sym)
    val newTargetTy = st.getConstr(SNewTarget.sym)
    // newTarget alone does not imply a constructable entry
    val newTarget =
      if (isConstructable(entryFunc, cfg)) newTargetTy
      else newTargetTy && UndefT
    if (newTarget.isBottom) return None
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
    getPath(entryFunc).map(path => Template(path, thisTy, args, newTarget))

  /** JS call form and input types */
  case class Template(
    path: BuiltinPath,
    thisTy: ValueTy,
    argTys: List[ValueTy],
    newTargetTy: ValueTy,
  ) {
    def apply(
      thisV: String,
      vs: List[String],
      newTarget: String,
    ): Option[String] =
      if (newTarget.isEmpty) { // without newTarget: XXX.call
        path match
          case BuiltinPath.Getter(base) =>
            descriptor(base).map(d => s"$d.get.call($thisV)")
          case BuiltinPath.Setter(base) =>
            val value = vs.headOption.getOrElse("undefined")
            descriptor(base).map(d => s"$d.set.call($thisV, $value)")
          case _ =>
            val args = (thisV :: vs).mkString(", ")
            access(path).map(fn => s"$fn.call($args)")
      } else { // with newTarget: Reflect.construct
        access(path).map { fn =>
          s"Reflect.construct($fn, [${vs.mkString(", ")}], $newTarget)"
        }
      }

    def apply(vs: List[String]): Option[String] =
      access(path).map(fn => s"new ($fn)(${vs.mkString(", ")})")
  }

  private def isConstructable(func: Func, cfg: CFG): Boolean =
    cfg.init.intrHeap
      .get(intrAddr(func.name.stripPrefix("INTRINSICS.")))
      .exists {
        case record: RecordObj => record.map.contains("Construct")
        case _                 => false
      }

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
