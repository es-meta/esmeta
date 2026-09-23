package esmeta.solver

import esmeta.cfg.{CFG, Func}
import esmeta.es.*
import esmeta.es.util.{Coverage, Walker}
import esmeta.ir.{Name, Ref}
import esmeta.ir.util.UnitWalker
import esmeta.spec.BuiltinHead
import esmeta.util.{ConcurrentPolicy => CP, ProgressBar}
import java.util.IdentityHashMap
import java.util.concurrent.{ConcurrentHashMap => CMMap}
import scala.jdk.CollectionConverters.*
import scala.util.Try

/** reduce witnesses into simpler or bug-triggering forms, keeping coverage */
class Reducer(cfg: CFG) {
  private lazy val cov = Coverage(cfg, timeLimit = Some(2))

  /** branch sides a program touches, none if it fails */
  private def touched(js: String): Set[(Int, Boolean)] = Try {
    val conds = cov.run(js).touchedCondViews.keys.map(_.cond)
    conds.map(c => (c.branch.id, c.cond)).toSet
  }.getOrElse(Set.empty)

  /** reduced witnesses */
  def apply(
    witnesses: Map[(Int, Boolean), String],
  ): Map[(Int, Boolean), String] = {
    val programs = witnesses.toList.groupMap(_._2)(_._1)
    val reduced = CMMap[(Int, Boolean), String](witnesses.asJava)
    val bar = ProgressBar(
      msg = s"reducing ${programs.size} programs",
      iterable = programs,
      detail = false,
      concurrent = CP.Fixed(Runtime.getRuntime.availableProcessors),
    )
    bar.foreach { (js, conds) =>
      for ((c, s) <- reduce(js, conds.toSet)) reduced.put(c, s)
    }
    val count = programs.count { (js, conds) =>
      conds.exists(c => reduced.get(c) != js)
    }
    println(
      s"Reduction: ${bar.summary.time.simpleString}" +
      s" ($count of ${programs.size} programs reduced)",
    )
    reduced.asScala.toMap
  }

  /** smaller programs for the branch sides they still cover */
  def reduce(
    js: String,
    conds: Set[(Int, Boolean)],
  ): Map[(Int, Boolean), String] = {
    val groups = transforms.foldLeft(Map(js -> conds)) { (groups, transform) =>
      groups.toList
        .flatMap { (cur, covered) =>
          val next = Try(run(cur, transform)).getOrElse(cur)
          if (next == cur) List(cur -> covered)
          else {
            val kept = covered intersect touched(next)
            List(next -> kept, cur -> (covered -- kept))
          }
        }
        .groupMapReduce(_._1)(_._2)(_ ++ _)
        .filter(_._2.nonEmpty)
    }
    for {
      (cur, covered) <- groups
      if cur != js
      cond <- covered
    } yield cond -> cur
  }

  lazy val transforms: List[(Syntactic, Ast => String) => Option[String]] =
    flattenSpread :: // simplify: flatten spread elements into arguments
    weakenArgs ::: // trigger: pass `undefined` explicitly at each position
    trimUndefined :: // trigger: omit trailing `undefined` arguments
    constructToNew :: // simplify: `Reflect.construct` to `new` if possible
    dropReceiver :: // simplify: drop receivers builtins never read
    accessorToProperty :: // simplify: accessor calls to property accesses
    callAsMethod :: // simplify: prototype method calls to method calls
    hoistIIFE :: // simplify: hoist IIFE bodies to the top of the program
    Nil

  def run(
    src: String,
    transform: (Syntactic, Ast => String) => Option[String],
  ): String = {
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
        transform(node, text).flatMap { str =>
          Try(
            cfg.esParser(node.name, node.args).fromWithSourceText(str)._1,
          ).toOption
        } match
          case Some(syn: Syntactic) => syn
          case _                    => node
    }
    text(walker.walk(cfg.scriptParser.fromWithSourceText(src)._1))
  }

  private val nameMap = cfg.grammar.nameMap

  /** exposed builtins whose algorithms never read the this value */
  lazy val thisFree: Set[String] = (for {
    f <- cfg.funcs.iterator if f.isBuiltin
    expr <- Solver.funcAccessExpr(f) if (!readsThis(f))
  } yield expr).toSet

  def readsThis(f: Func): Boolean = {
    var found = false
    val walker = new UnitWalker {
      override def walk(ref: Ref): Unit = ref match
        case Name("this") => found = true
        case _            => super.walk(ref)
    }
    walker.walk(f.irFunc)
    found
  }

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

  private object Index {
    def unapply(ast: Ast): Option[(Ast, Ast)] = ast match
      case Syntactic("MemberExpression", _, 1, Vector(Some(base), Some(idx))) =>
        Some(base -> idx)
      case Syntactic("CallExpression", _, 4, Vector(Some(base), Some(idx))) =>
        Some(base -> idx)
      case _ => None
  }

  private def isProto(ast: Ast): Boolean = ast match
    case Dot(_, "prototype")                 => true
    case Invoke(Dot(_, "getPrototypeOf"), _) => true
    case _                                   => false

  private def isDescriptor(ast: Ast): Boolean = ast match
    case Invoke(Dot(_, "getOwnPropertyDescriptor"), _) => true
    case _                                             => false

  private def isMethodRef(ast: Ast): Boolean = ast match
    case Dot(p, _)   => isProto(p) || isDescriptor(p)
    case Index(p, _) => isProto(p)
    case _           => false

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

  private def elems(ast: Ast): Option[List[(Boolean, Ast)]] = unwrap(ast) match
    case Syntactic("ArrayLiteral", _, 0, Vector(None))          => Some(Nil)
    case Syntactic("ArrayLiteral", _, 1, Vector(Some(l)))       => elemList(l)
    case Syntactic("ArrayLiteral", _, 2, Vector(Some(l), None)) => elemList(l)
    case _                                                      => None

  private def elemList(ast: Ast): Option[List[(Boolean, Ast)]] = ast match
    case Syntactic("ElementList", _, _, Vector(None, Some(e))) =>
      Some(List(elem(e)))
    case Syntactic("ElementList", _, _, Vector(Some(l), None, Some(e))) =>
      elemList(l).map(_ :+ elem(e))
    case _ => None

  private def elem(ast: Ast): (Boolean, Ast) = ast match
    case Syntactic("SpreadElement", _, 0, Vector(Some(e))) => true -> e
    case e                                                 => false -> e

  private def flatten(items: List[(Boolean, Ast)]): List[(Boolean, Ast)] =
    items.flatMap {
      case (true, e) => elems(e).getOrElse(List(true -> e))
      case item      => List(item)
    }

  private def trim(
    items: List[(Boolean, Ast)],
    text: Ast => String,
  ): List[(Boolean, Ast)] =
    items.reverse
      .dropWhile((spread, e) => !spread && text(e) == "undefined")
      .reverse

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

  private def constructor(ast: Ast, text: Ast => String): String =
    val str = text(ast)
    unwrap(ast) match
      case _: Lexical                                 => str
      case Syntactic("MemberExpression", _, 1 | 2, _) => str
      case _                                          => s"($str)"

  private val identifier = "[A-Za-z_$][\\w$]*".r

  private def property(key: Ast, text: Ast => String): String = unwrap(
    key,
  ) match
    case Lexical("StringLiteral", str)
        if (str.length >= 2 && identifier.matches(
          str.substring(1, str.length - 1),
        )) =>
      "." + str.substring(1, str.length - 1)
    case _ => s"[${text(key)}]"

  def flattenSpread(
    ast: Syntactic,
    text: Ast => String,
  ): Option[String] =
    val items = ast.name match
      case "Arguments"    => args(ast)
      case "ArrayLiteral" => elems(ast)
      case _              => None
    for {
      list <- items
      if (list.exists((spread, e) => spread && elems(e).isDefined))
    } yield
      val inner = render(flatten(list), text)
      if (ast.name == "Arguments") s"($inner)" else s"[$inner]"

  /** weaken the argument at one position to `undefined` if the target allows */
  def weakenArg(idx: Int)(ast: Syntactic, text: Ast => String): Option[String] =
    for {
      list <- if (ast.name == "Arguments") args(ast) else None
      (isSpread, arg) <- list.lift(idx)
      if (!isSpread && text(arg) != "undefined")
      inner = list.zipWithIndex
        .map {
          case ((spread, e), i) =>
            if (i == idx) "undefined" else (if (spread) "..." else "") + text(e)
        }
        .mkString(", ")
    } yield s"($inner)"

  /** one transform per argument position a builtin call can have */
  private lazy val weakenArgs = (0 until maxArgs).map(weakenArg).toList

  /** the widest argument list a builtin call can take, plus its receiver */
  private lazy val maxArgs: Int = (for {
    f <- cfg.funcs.iterator if f.isBuiltin
    head <- f.head.collect { case h: BuiltinHead => h }
  } yield head.params.size).maxOption.getOrElse(0) + 1

  def trimUndefined(
    ast: Syntactic,
    text: Ast => String,
  ): Option[String] =
    for {
      list <- if (ast.name == "Arguments") args(ast) else None
      trimmed = trim(list, text)
      if (trimmed.size < list.size)
    } yield s"(${render(trimmed, text)})"

  def constructToNew(
    ast: Syntactic,
    text: Ast => String,
  ): Option[String] =
    ast match
      case Syntactic(
            "LeftHandSideExpression",
            _,
            1,
            Vector(Some(Invoke(callee, a))),
          ) if (text(callee) == "Reflect.construct") =>
        args(a).flatMap {
          case (false, fn) :: (false, arr) :: rest
              if (rest.size <= 1 && rest.forall(!_._1)) =>
            elems(arr).map { es =>
              s"new ${constructor(fn, text)}(${render(trim(flatten(es), text), text)})"
            }
          case _ => None
        }
      case _ => None

  def dropReceiver(ast: Syntactic, text: Ast => String): Option[String] =
    ast match
      case Invoke(Dot(fn, "call"), a)
          if (!isMethodRef(fn) && thisFree.contains(text(fn))) =>
        args(a).flatMap {
          case (false, _) :: rest =>
            Some(s"${base(fn, text)}(${render(rest, text)})")
          case Nil => Some(s"${base(fn, text)}()")
          case _   => None
        }
      case _ => None

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

  def callAsMethod(ast: Syntactic, text: Ast => String): Option[String] =
    ast match
      case Invoke(Dot(fn, "call"), a) =>
        val access = fn match
          case Dot(p, name) if (isProto(p))  => Some("." + name)
          case Index(p, idx) if (isProto(p)) => Some(s"[${text(idx)}]")
          case _                             => None
        access.flatMap { acc =>
          args(a).flatMap {
            case (false, recv) :: rest =>
              Some(s"${base(recv, text)}$acc(${render(rest, text)})")
            case _ => None
          }
        }
      case _ => None

  private val bodies = Set("FunctionBody", "ConciseBody", "ClassBody")

  private def arrowBody(callee: Ast): Option[Ast] = unwrap(callee) match
    case Syntactic(
          "CoverParenthesizedExpressionAndArrowParameterList",
          _,
          0,
          Vector(Some(e)),
        ) =>
      unwrap(e) match
        case Syntactic(
              "ArrowFunction",
              _,
              0,
              Vector(
                _,
                Some(Syntactic("ConciseBody", _, 1, Vector(Some(body)))),
              ),
            ) =>
          body match
            case Syntactic(
                  "FunctionBody",
                  _,
                  _,
                  Vector(Some(Syntactic(_, _, _, Vector(Some(list))))),
                ) =>
              Some(list)
            case _ => None
        case _ => None
    case _ => None

  private def statements(list: Ast): List[Ast] = list match
    case Syntactic("StatementList", _, 0, Vector(Some(item))) => List(item)
    case Syntactic("StatementList", _, 1, Vector(Some(l), Some(item))) =>
      statements(l) :+ item
    case _ => Nil

  private def returned(stmt: Ast): Option[Ast] = unwrap(stmt) match
    case Syntactic("ReturnStatement", _, 1, Vector(Some(e))) => Some(e)
    case _                                                   => None

  private def iifes(ast: Ast): List[(Syntactic, List[Ast], Ast)] = ast match
    case syn: Syntactic =>
      val nested = () => syn.children.flatten.toList.flatMap(iifes)
      syn match
        case Invoke(callee, Syntactic("Arguments", _, 0, _)) =>
          (for {
            list <- arrowBody(callee)
            stmts = statements(list)
            last <- stmts.lastOption
            ret <- returned(last)
          } yield List((syn, stmts.init, ret))).getOrElse(nested())
        case _ if (bodies.contains(syn.name)) => Nil
        case _                                => nested()
    case _ => Nil

  def hoistIIFE(
    ast: Syntactic,
    text: Ast => String,
  ): Option[String] =
    ast match
      case Syntactic("Script", _, _, _) =>
        val found = iifes(ast).sortBy(_._1.loc.get.start.offset)
        val names = (ast :: found.flatMap(_._2)).flatMap(declared)
        Option.when(found.nonEmpty && names.distinct.size == names.size) {
          val src = text(ast)
          val base = ast.loc.get.start.offset
          val body = found.reverse.foldLeft(src) {
            case (acc, (node, _, ret)) =>
              val loc = node.loc.get
              acc.substring(0, loc.start.offset - base) + text(ret) +
              acc.substring(loc.end.offset - base)
          }
          val prelude = found.flatMap((_, stmts, _) => stmts.map(text))
          (prelude :+ body).mkString("\n")
        }
      case _ => None

  // names an AST declares in its own scope, not in nested functions
  private def declared(ast: Ast): List[String] = ast match
    case Syntactic("BindingIdentifier", _, _, _) =>
      unwrap(ast) match
        case Lexical(_, name) => List(name)
        case _                => Nil
    case syn: Syntactic if (!scopes.contains(syn.name)) =>
      syn.children.flatten.toList.flatMap(declared)
    case _ => Nil

  private val scopes = bodies ++ Set(
    "FormalParameters",
    "UniqueFormalParameters",
    "ArrowParameters",
    "FunctionExpression",
    "GeneratorExpression",
    "AsyncFunctionExpression",
    "AsyncGeneratorExpression",
    "ClassExpression",
  )
}
