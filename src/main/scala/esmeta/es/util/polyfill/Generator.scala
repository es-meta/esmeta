package esmeta.es.util.polyfill

import esmeta.error.NotSupported
import esmeta.es.util.polyfill.completion.CompletionPath
import esmeta.es.util.polyfill.dsl.*
import esmeta.lang.*
import esmeta.lang.util.UnitWalker as LangUnitWalker
import esmeta.spec.*

import scala.annotation.tailrec
import scala.collection.mutable

object Generator {
  def apply(
    spec: Spec,
    dslDir: Option[String],
    skipUnsupported: Boolean = false,
  ): List[Polyfill] =
    new Generator(spec, dslDir, skipUnsupported).result
}

/** polyfill generator
  *
  * @param skipUnsupported
  *   drop algorithms whose metalanguage the translator does not support yet,
  *   recording why in `skipped`, instead of failing on the first one
  */
class Generator(
  spec: Spec,
  dslDir: Option[String],
  skipUnsupported: Boolean = false,
) {

  private val translator = Translator(spec)
  private val skippedBuffer = mutable.ListBuffer[(String, String)]()

  lazy val result: List[Polyfill] =
    val optimizedTargets = optPaths.foldLeft(targets) { (x, optim) => optim(x) }
    CompletionPath(optimizedTargets).flatMap { algo =>
      try Some(translator.compile(algo))
      catch
        case e: NotSupported =>
          if (!skipUnsupported) throw NotSupported(e.reasonPath :+ algo.name)
          skippedBuffer += algo.name -> e.reasonPath.mkString("/")
          None
    }

  /** algorithms dropped by `skipUnsupported`, with the reason for each */
  lazy val skipped: Map[String, String] = { result; skippedBuffer.toMap }

  val optPaths: List[TransformPath] =
    List(ShorthandInlinePath(spec)) ++ dslDir.map(DSLPath(_))

  lazy val targets: List[Algorithm] = Targets(spec)
}

object Targets {

  val targetPatterns = List(
    // https://tc39.es/ecma262/#sec-properties-of-the-string-prototype-object
    """INTRINSICS\.(get:|set:)?String\..*""",
    // https://tc39.es/ecma262/#sec-properties-of-the-array-prototype-object
    """INTRINSICS\.(get:|set:)?Array\..*""",
    // https://tc39.es/ecma262/#sec-map-objects
    """INTRINSICS\.(get:|set:)?Map.*""",
    // https://tc39.es/ecma262/#sec-set-objects
    """INTRINSICS\.(get:|set:)?Set.*""",
    // https://tc39.es/ecma262/#sec-iterator-objects
    // """INTRINSICS\.(get:|set:)?Iterator.*""",
    // https://tc39.es/ecma262/#sec-promise-objects
    """INTRINSICS\.(get:|set:)?Promise.*""",
    """INTRINSICS\.(get:|set:)?WeakMap.*""",
    """INTRINSICS\.(get:|set:)?WeakSet.*""",
  )

  val ignoreTargets = List(
    // ES1
    "INTRINSICS.String.fromCharCode",
    // ES3
    "INTRINSICS.String.prototype.charAt",
    "INTRINSICS.String.prototype.charCodeAt",
    "INTRINSICS.String.prototype.concat",
    "INTRINSICS.String.prototype.indexOf",
    "INTRINSICS.String.prototype.lastIndexOf",
    "INTRINSICS.String.prototype.localeCompare",
    "INTRINSICS.String.prototype.match",
    "INTRINSICS.String.prototype.replace",
    "INTRINSICS.String.prototype.search",
    "INTRINSICS.String.prototype.slice",
    "INTRINSICS.String.prototype.split",
    "INTRINSICS.String.prototype.substring",
    "INTRINSICS.String.prototype.toLocaleLowerCase",
    "INTRINSICS.String.prototype.toLocaleUpperCase",
    "INTRINSICS.String.prototype.toLowerCase",
    "INTRINSICS.String.prototype.toString",
    "INTRINSICS.String.prototype.toUpperCase",
    "INTRINSICS.String.prototype.valueOf",
    // YET
    "INTRINSICS.String.prototype.matchAll",
    "INTRINSICS.String.prototype.normalize",
    "INTRINSICS.String.prototype.repeat",

    // ES3
    "INTRINSICS.Array.prototype.concat",
    "INTRINSICS.Array.prototype.join",
    "INTRINSICS.Array.prototype.pop",
    "INTRINSICS.Array.prototype.push",
    "INTRINSICS.Array.prototype.reverse",
    "INTRINSICS.Array.prototype.shift",
    "INTRINSICS.Array.prototype.slice",
    "INTRINSICS.Array.prototype.sort",
    "INTRINSICS.Array.prototype.splice",
    "INTRINSICS.Array.prototype.toLocaleString",
    "INTRINSICS.Array.prototype.toString",
    "INTRINSICS.Array.prototype.unshift",

    // Unsupported
    "INTRINSICS.MapIteratorPrototype.next",
    "INTRINSICS.SetIteratorPrototype.next",

    // Yet AOs
    "ArrayCreate",
    "ArraySpeciesCreate",
    "AsyncGeneratorYield",
    "Await",
    "CreateBuiltinFunction",
    "CreateIteratorFromClosure",
    "GeneratorResume",
    "GeneratorStart",
    "GeneratorYield",
    "GetFunctionRealm",
    "GetPrototypeFromConstructor",
    "RegExpInitialize",
    "StringToNumber",
    "StringToBigInt",

    // Generator
    "GeneratorYield",
    "GeneratorStart",
    "GeneratorValidate",
    "GeneratorResume",
    "GeneratorResumeAbrupt",
    "CreateIteratorFromClosure",
    "CreateArrayIterator",
  )

  def apply(spec: Spec): List[Algorithm] = {
    val initialTargets = spec.algorithms
      .filter(algo => targetPatterns.exists(algo.name.matches))
      .toSet

    // Build maximum set based on worklist algorithm
    val result = expand(initialTargets, initialTargets) {
      _.flatMap(getAOCallees(spec, _))
    }

    result
      .filter(algo => !ignoreTargets.contains(algo.name))
      .toList
      .sortWith(_.name < _.name)
  }

  @tailrec private def expand[T](acc: Set[T], curr: Set[T])(
    f: Set[T] => Set[T],
  ): Set[T] = {
    if (curr.isEmpty) acc
    else
      val next = f(curr) -- acc
      expand(acc ++ next, next)(f)
  }

  private def getAOCallees(spec: Spec, algo: Algorithm): Set[Algorithm] = {
    val result = mutable.Set[Algorithm]()
    new LangUnitWalker {
      override def walk(expr: Expression): Unit = expr match
        case InvokeAbstractOperationExpression(name, args, _) =>
          result ++= spec.fnameMap.get(name)
          walkList(args, walk)
        case XRefExpression(
              XRefExpressionOperator.Algo | XRefExpressionOperator.Definition |
              XRefExpressionOperator.InternalMethod,
              id,
            ) =>
          val targetAlgo = spec.getAlgoById(id)
          // Elem is reference; It is deinitialized at the erasure stage
          val capturedAlgo = targetAlgo.copy()
          result += capturedAlgo
        case _ => super.walk(expr)
    }.walk(algo.body)
    result.toSet
  }
}
