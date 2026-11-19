package esmeta.compiler

import esmeta.ir.*
import esmeta.spec.Grammar
import esmeta.util.BaseUtils.raise

/** generator of IR functions that find and call syntax-directed operations
  * (SDOs)
  *
  * For each SDO method `m`, it generates three auxiliary functions:
  *   - `__SDO_LOOKUP__.m(ast)` returns the closure of the SDO `m` defined for
  *     the production alternative of `ast` itself, falling back to `DEFAULT:m`,
  *     or `null` if none exists.
  *   - `__SDO_TARGET__.m(ast)` returns the AST node where the SDO `m` is
  *     defined, moving to the only child of a chain production with
  *     `__CHAIN_CHILD__` until `__SDO_LOOKUP__.m` finds one.
  *   - `__SDO__.m(this, ...)` calls the SDO found by `__SDO_LOOKUP__.m` for the
  *     AST node given by `__SDO_TARGET__.m`.
  *
  * `__SDO_LOOKUP__.m`, `__SDO_TARGET__.m`, and `__CHAIN_CHILD__` are pure
  * functions of ASTs, so the interpreter caches their results, and an SDO call
  * takes constant steps regardless of the length of production chains.
  *
  * An SDO call `sdo-call x = ast->m(args)` means `call x =
  * clo<"__SDO__.m">(ast, args)` for a syntactic AST.
  */
class SdoGenerator(grammar: Grammar, funcs: Iterable[Func]) {

  /** generated functions */
  lazy val result: List[Func] = for {
    (method, sdos) <- sdoMap.toList.sortBy(_._1)
    func <- List(
      lookup(method, sdos),
      target(method),
      dispatcher(method, sdos),
    )
  } yield func

  /** SDO definitions */
  private enum Sdo:
    case Base(func: Func, name: String, i: Int, j: Int)
    case Default(func: Func)
    val func: Func

  /** SDO definitions for each method */
  private lazy val sdoMap: Map[String, List[Sdo]] = (for {
    func <- funcs.toList if func.kind == FuncKind.SynDirOp
    (method, sdo) <- func.name match
      case sdoPattern(name, i, j, method) =>
        Some(method -> Sdo.Base(func, name, i.toInt, j.toInt))
      case defaultSdoPattern(method) => Some(method -> Sdo.Default(func))
      case _                         => None
  } yield (method, sdo)).groupMap(_._1)(_._2)

  /** lookup function for a method */
  private def lookup(method: String, sdos: List[Sdo]): Func =
    def ret(sdo: Sdo): String = s"""return clo<"${sdo.func.name}">"""
    val bases = sdos.collect { case sdo: Sdo.Base => sdo }
    val default = sdos.collectFirst { case sdo: Sdo.Default => sdo }
    // branches for each production alternative
    def alt(name: String, i: Int, sdos: List[Sdo.Base]): String =
      val optPos = optionalPositions(name, i)
      val rets = for (sdo <- sdos.sortBy(_.j)) yield
        val conds = for {
          (pos, k) <- optPos.zipWithIndex
          bit = (sdo.j >> (optPos.length - 1 - k)) & 1
        } yield
          if (bit == 1) s"(exists $AST[$pos])"
          else s"(! (exists $AST[$pos]))"
        if (conds.isEmpty) ret(sdo) else s"if ${and(conds)} ${ret(sdo)}"
      s"if (? $AST: Ast[$name[$i]]) {\n${rets.mkString("\n")}\n}"
    // binary search on production names, where `Ast[A|B]` also holds when
    // the only child of a chain production is `A` or `B`, so it falls through
    // to the other half if not found
    val byName = bases.groupBy(_.name)
    def search(names: Vector[String]): String =
      if (names.length == 1)
        val name = names.head
        byName(name)
          .groupBy(_.i)
          .toList
          .sortBy(_._1)
          .map {
            case (i, sdos) => alt(name, i, sdos)
          }
          .mkString("\n")
      else
        val (l, r) = names.splitAt(names.length / 2)
        s"""if (? $AST: Ast[${l.mkString("|")}]) {
        |${search(l)}
        |}
        |${search(r)}""".stripMargin
    val branches =
      if (byName.isEmpty) Nil else List(search(byName.keys.toVector.sorted))
    val last = default.fold("return null")(ret)
    Func.from(s"""def <AUX>:${sdoLookupName(method)}(
    |  $AST: Ast,
    |): Clo | Null = {
    |${(branches :+ last).mkString("\n")}
    |}""".stripMargin)

  /** target function for a method */
  private def target(method: String): Func =
    Func.from(s"""def <AUX>:${sdoTargetName(method)}(
    |  $AST: Ast,
    |): Ast = {
    |while true {
    |call $SDO = clo<"${sdoLookupName(method)}">($AST)
    |if (? $SDO: Clo) return $AST
    |call $AST = clo<"__CHAIN_CHILD__">($AST)
    |}
    |}""".stripMargin)

  /** dispatcher function for a method */
  private def dispatcher(method: String, sdos: List[Sdo]): Func =
    val func = sdos.head.func
    val params = func.params.drop(1)
    val names = params.map(_.lhs.name)
    for (x <- List(AST, SDO, RESULT) if names contains x)
      raise(s"`$x` is a parameter name of SDO `$method`")
    val (required, optional) = params.span(!_.optional)
    // pass only the given optional arguments, which are trailing
    val calls = for {
      n <- optional.length to 0 by -1
      args = AST :: (required ++ optional.take(n)).map(_.lhs.name)
      call = s"""call $RESULT = $SDO(${args.mkString(", ")})
      |return $RESULT""".stripMargin
    } yield
      if (n == 0) call
      else s"if (exists ${optional(n - 1).lhs.name}) {\n$call\n}"
    Func.from(s"""def <AUX>:${sdoDispatcherName(method)}(
    |${("this: Ast" :: params.map(_.toString))
      .map("  " + _ + ",")
      .mkString("\n")}
    |)${retTyStr(func.retTy)} = {
    |call $AST = clo<"${sdoTargetName(method)}">(this)
    |call $SDO = clo<"${sdoLookupName(method)}">($AST)
    |${calls.mkString("\n")}
    |}""".stripMargin)

  /** positions of optional children in a production alternative */
  private def optionalPositions(name: String, i: Int): List[Int] =
    val rhs = grammar.nameMap(name).rhsVec(i)
    rhs.ntsWithOptional.zipWithIndex.collect { case ((_, true), pos) => pos }

  /** conjunction of conditions */
  private def and(conds: List[String]): String =
    conds.reduceRight((l, r) => s"(&& $l $r)")

  /** stringified return type */
  private def retTyStr(ty: Type): String =
    if (ty == UnknownType) "" else s": $ty"

  private val AST = "ast"
  private val SDO = "sdo"
  private val RESULT = "result"
  private val defaultSdoPattern = """DEFAULT:(\w+)""".r
  private val sdoPattern = """(\w+)\[(\d+),(\d+)\]\.(\w+)""".r
}
