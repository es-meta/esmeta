package esmeta.solver

import esmeta.cfg.CFG
import esmeta.es.*
import esmeta.es.util.{Coverage, Walker}
import esmeta.spec.BuiltinHead
import esmeta.util.{ConcurrentPolicy => CP, ProgressBar}
import java.util.IdentityHashMap
import java.util.concurrent.{ConcurrentHashMap => CMMap}
import scala.jdk.CollectionConverters.*
import scala.util.Try

/** amplify witnesses into forms that may reveal bugs, keeping coverage */
class Amplifier(cfg: CFG) {
  private lazy val cov = Coverage(cfg, timeLimit = Some(2))

  /** branch sides a program touches, none if it fails */
  private def touched(js: String): Set[(Int, Boolean)] = Try {
    val conds = cov.run(js).touchedCondViews.keys.map(_.cond)
    conds.map(c => (c.branch.id, c.cond)).toSet
  }.getOrElse(Set.empty)

  /** amplified witnesses, the original program first */
  def apply(
    witnesses: Map[(Int, Boolean), String],
  ): Map[(Int, Boolean), List[String]] = {
    val programs = witnesses.toList.groupMap(_._2)(_._1)
    val amplified = CMMap[(Int, Boolean), List[String]]()
    val bar = ProgressBar(
      msg = s"amplifying ${programs.size} programs",
      iterable = programs,
      detail = false,
      concurrent = CP.Fixed(Runtime.getRuntime.availableProcessors),
    )
    bar.foreach { (js, conds) =>
      for ((c, l) <- amplify(js, conds.toSet)) amplified.put(c, l)
    }
    val count = programs.count { (js, conds) =>
      conds.exists(c => amplified.get(c) != List(js))
    }
    println(
      s"Amplification: ${bar.summary.time.simpleString}" +
      s" ($count of ${programs.size} programs amplified)",
    )
    amplified.asScala.toMap
  }

  /** apply rewriting rule at one site, while preserving coverage */
  def amplify(
    js: String,
    conds: Set[(Int, Boolean)],
  ): Map[(Int, Boolean), List[String]] =
    val variants = js :: (for {
      rule <- rules
      site <- 0 until Try(rewrite(js, rule, None)._2).getOrElse(0)
      next = Try(rewrite(js, rule, Some(site))._1).getOrElse(js)
      if (next != js)
    } yield next)
    (for {
      p <- variants.distinct
      bound = Try(run(p, bindResults)).getOrElse(p)
      cond <- if (p == js) conds else conds intersect touched(bound)
    } yield cond -> bound)
      .groupMap(_._1)(_._2)
      .map((cond, l) => cond -> l.distinct)

  type Rule = (Syntactic, Ast => String) => Option[String]

  /** rewrites that make engines take another path for the same behavior */
  lazy val rules: List[Rule] =
    accessorToProperty :: // accessor calls to property accesses
    positions(omitFrom) ::: // omit the arguments from a position on
    positions(weakenArg) ::: // pass `undefined` explicitly at a position
    positions(poisonArg) ::: // poison the argument at a position
    positions(proxyArg) // wrap the argument at a position in a Proxy

  private def positions(rule: Int => Rule): List[Rule] =
    (0 until maxArgs).map(rule).toList

  /** a program with a rewrite applied everywhere it applies */
  def run(src: String, transform: Rule): String =
    rewrite(src, transform, None)._1

  // a program with a rewrite applied at one site or all, and how many apply
  private def rewrite(
    src: String,
    transform: Rule,
    site: Option[Int],
  ): (String, Int) = {
    val origins = IdentityHashMap[Syntactic, Syntactic]()
    def text(ast: Ast): String = ast match
      case lex: Lexical => lex.str
      case syn: Syntactic =>
        syn.loc.flatMap(loc => loc.originText.map(loc.getString)) match
          case Some(str) => str
          case None =>
            val old = origins.get(syn)
            val loc = old.loc.get
            val base = loc.originText.get
            val sb = StringBuilder()
            var cur = loc.start.offset
            old.children.zip(syn.children).foreach {
              case (Some(o: Syntactic), Some(n)) if !(o eq n) =>
                val l = o.loc.get
                sb.append(base.substring(cur, l.start.offset)).append(text(n))
                cur = l.end.offset
              case _ =>
            }
            sb.append(base.substring(cur, loc.end.offset)).toString
    var sites = 0
    val walker = new Walker {
      override def walk(ast: Syntactic): Syntactic =
        val children = ast.children.map(_.map(walk))
        val isSame = children.zip(ast.children).forall {
          case (Some(n), Some(o)) => n eq o
          case (n, o)             => n.isEmpty && o.isEmpty
        }
        val node =
          if (isSame) ast
          else {
            val rebuilt = Syntactic(ast.name, ast.args, ast.rhsIdx, children)
            origins.put(rebuilt, ast)
            rebuilt
          }
        transform(node, text)
          .filter { _ =>
            sites += 1
            site.forall(_ == sites - 1)
          }
          .flatMap { str =>
            Try(
              cfg.esParser(node.name, node.args).fromWithSourceText(str)._1,
            ).toOption
          } match
          case Some(syn: Syntactic) => syn
          case _                    => node
    }
    val out = text(walker.walk(cfg.scriptParser.fromWithSourceText(src)._1))
    (out, sites)
  }

  private val nameMap = cfg.grammar.nameMap

  private def unwrap(ast: Ast): Ast = ast match
    case syn: Syntactic =>
      val rhs = nameMap(syn.name).rhsVec(syn.rhsIdx)
      syn.children.flatten match
        case Vector(child) if rhs.ts.isEmpty => unwrap(child)
        case _                               => syn
    case lex => lex

  private object Invoke {
    def unapply(ast: Ast): Option[(Ast, Syntactic)] = ast match
      case Syntactic("CallExpression", _, 0, Vector(Some(cover))) =>
        unapply(cover)
      case Syntactic(
            "CoverCallExpressionAndAsyncArrowHead",
            _,
            0,
            Vector(Some(callee), Some(a: Syntactic)),
          ) =>
        Some(callee -> a)
      case Syntactic(
            "CallExpression",
            _,
            3,
            Vector(Some(callee), Some(a: Syntactic)),
          ) =>
        Some(callee -> a)
      case _ => None
  }

  private object Dot {
    def unapply(ast: Ast): Option[(Ast, String)] = ast match
      case Syntactic(
            "MemberExpression",
            _,
            2,
            Vector(Some(base), Some(Lexical(_, name))),
          ) =>
        Some(base -> name)
      case Syntactic(
            "CallExpression",
            _,
            5,
            Vector(Some(base), Some(Lexical(_, name))),
          ) =>
        Some(base -> name)
      case _ => None
  }

  private def isDescriptor(ast: Ast): Boolean = ast match
    case Invoke(Dot(_, "getOwnPropertyDescriptor"), _) => true
    case _                                             => false

  private def args(ast: Syntactic): Option[List[(Boolean, Ast)]] = ast match
    case Syntactic("Arguments", _, 0, _)               => Some(Nil)
    case Syntactic("Arguments", _, _, Vector(Some(l))) => Some(argList(l))
    case _                                             => None

  private def argList(ast: Ast): List[(Boolean, Ast)] = ast match
    case Syntactic("ArgumentList", _, idx, Vector(Some(e))) =>
      List((idx == 1) -> e)
    case Syntactic("ArgumentList", _, idx, Vector(Some(l), Some(e))) =>
      argList(l) :+ ((idx == 3) -> e)
    case _ => Nil

  private def render(items: List[(Boolean, Ast)], text: Ast => String): String =
    items
      .map((spread, e) => (if (spread) "..." else "") + text(e))
      .mkString(", ")

  private val bare = Set(
    "MemberExpression",
    "CallExpression",
    "CoverCallExpressionAndAsyncArrowHead",
    "ArrayLiteral",
    "TemplateLiteral",
    "CoverParenthesizedExpressionAndArrowParameterList",
  )

  private def base(ast: Ast, text: Ast => String): String =
    val str = text(ast)
    unwrap(ast) match
      case Lexical("NumericLiteral", _)                => s"($str)"
      case _: Lexical                                  => str
      case syn: Syntactic if (bare.contains(syn.name)) => str
      case _                                           => s"($str)"

  private val identifier = "[A-Za-z_$][\\w$]*".r

  private def property(key: Ast, text: Ast => String): String =
    unwrap(key) match
      case Lexical("StringLiteral", str)
          if (str.length >= 2 &&
          identifier.matches(str.substring(1, str.length - 1))) =>
        "." + str.substring(1, str.length - 1)
      case _ => s"[${text(key)}]"

  /** omit the arguments from a position on, which the callee sees as absent */
  def omitFrom(idx: Int)(ast: Syntactic, text: Ast => String): Option[String] =
    for {
      list <- if (ast.name == "Arguments") args(ast) else None
      if (idx < list.size && list.drop(idx).forall(!_._1))
    } yield s"(${render(list.take(idx), text)})"

  /** pass `undefined` explicitly at a position, present but without a value */
  def weakenArg(idx: Int)(ast: Syntactic, text: Ast => String): Option[String] =
    for {
      list <- if (ast.name == "Arguments") args(ast) else None
      (isSpread, arg) <- list.lift(idx)
      if (!isSpread && text(arg) != "undefined")
    } yield replaceAt(list, idx, "undefined", text)

  private def replaceAt(
    list: List[(Boolean, Ast)],
    idx: Int,
    str: String,
    text: Ast => String,
  ): String = list.zipWithIndex
    .map {
      case ((spread, e), i) =>
        if (i == idx) str else (if (spread) "..." else "") + text(e)
    }
    .mkString("(", ", ", ")")

  /** a value whose every use throws an error */
  val poison =
    "new Proxy(function(){}, new Proxy({}, { get() { throw new EvalError; } }))"

  /** poison the argument at a position */
  def poisonArg(idx: Int)(ast: Syntactic, text: Ast => String): Option[String] =
    for {
      list <- if (ast.name == "Arguments") args(ast) else None
      (isSpread, _) <- list.lift(idx)
      if (!isSpread)
    } yield replaceAt(list, idx, poison, text)

  /** wrap the argument at a position in a Proxy without traps */
  def proxyArg(idx: Int)(ast: Syntactic, text: Ast => String): Option[String] =
    for {
      list <- if (ast.name == "Arguments") args(ast) else None
      (isSpread, arg) <- list.lift(idx)
      if (!isSpread)
    } yield replaceAt(list, idx, s"new Proxy(${text(arg)}, {})", text)

  /** the widest argument list a builtin call can take, plus its receiver */
  private lazy val maxArgs: Int = (for {
    f <- cfg.funcs.iterator if f.isBuiltin
    head <- f.head.collect { case h: BuiltinHead => h }
  } yield head.params.size).maxOption.getOrElse(0) + 1

  def accessorToProperty(ast: Syntactic, text: Ast => String): Option[String] =
    ast match
      case Syntactic("AssignmentExpression", _, _, _) =>
        unwrap(ast) match
          case Invoke(Dot(Dot(desc @ Invoke(_, d), kind), "call"), a)
              if (isDescriptor(desc) && (kind == "get" || kind == "set")) =>
            (args(d), args(a)) match
              case (
                    Some(List((false, _), (false, key))),
                    Some((false, recv) :: rest),
                  ) =>
                val target = base(recv, text) + property(key, text)
                (kind, rest) match
                  case ("get", _)               => Some(target)
                  case ("set", Nil)             => Some(s"$target = undefined")
                  case ("set", (false, v) :: _) => Some(s"$target = ${text(v)}")
                  case _                        => None
              case _ => None
          case _ => None
      case _ => None

  private def statements(list: Ast): List[Ast] = list match
    case Syntactic("StatementList", _, 0, Vector(Some(item))) => List(item)
    case Syntactic("StatementList", _, 1, Vector(Some(l), Some(item))) =>
      statements(l) :+ item
    case _ => Nil

  /** name each top-level expression statement so its value can be checked */
  def bindResults(ast: Syntactic, text: Ast => String): Option[String] =
    ast match
      case Syntactic(
            "Script",
            _,
            _,
            Vector(Some(Syntactic("ScriptBody", _, _, Vector(Some(list))))),
          ) =>
        val used = "[A-Za-z_$][\\w$]*".r.findAllIn(text(ast)).toSet
        val fresh =
          LazyList.from(0).map(i => s"__res$i").filterNot(used).iterator
        // binding a directive would drop its effect, such as strict mode
        val (prologue, rest) = statements(list).span(isDirective)
        val bound = rest.map { stmt =>
          unwrap(stmt) match
            case Syntactic("ExpressionStatement", _, _, Vector(Some(e))) =>
              // a comma would declare the operands after the first
              val value = e match
                case Syntactic("Expression", _, 1, _) => s"(${text(e)})"
                case _                                => text(e)
              s"const ${fresh.next()} = $value;"
            case _ => text(stmt)
        }
        Option.when(bound != rest.map(text)) {
          (prologue.map(text) ++ bound).mkString("\n")
        }
      case _ => None

  private def isDirective(stmt: Ast): Boolean = unwrap(stmt) match
    case Syntactic("ExpressionStatement", _, _, Vector(Some(e))) =>
      unwrap(e) match
        case Lexical("StringLiteral", _) => true
        case _                           => false
    case _ => false
}
