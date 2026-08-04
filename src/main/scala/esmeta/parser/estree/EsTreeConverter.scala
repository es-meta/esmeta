package esmeta.parser.estree

import esmeta.error.EsTreeError
import esmeta.es.*
import esmeta.spec.*
import esmeta.util.{Loc, Pos}
import scala.collection.concurrent.{TrieMap => Cache}

/** ECMA-262 concrete syntax tree builder from ESTree
  *
  * ESTree is an abstract syntax tree: it keeps the shape of a program but drops
  * the derivation that produced it. The ASTs of ESMeta, on the other hand, are
  * concrete syntax trees of the ECMA-262 grammar, where even a single numeric
  * literal is wrapped in the whole
  * `AssignmentExpression`-to-`PrimaryExpression` chain.
  *
  * The missing derivation is recovered in two steps:
  *
  *   1. each ESTree node is mapped to the *core* production that actually
  *      describes its shape (e.g. a `BinaryExpression` with the `+` operator
  *      maps to `AdditiveExpression`), and
  *   1. the chain of productions from the requested nonterminal down to that
  *      core production is searched in the grammar itself, considering only
  *      alternatives that derive a single nonterminal.
  *
  * Because the second step reads the grammar, the parameters of nonterminals
  * (`In`, `Yield`, `Await`, ...) and the alternative indices never appear as
  * magic numbers, and the converter keeps working when the grammar changes.
  *
  * Locations are outside the equality of ASTs, and this parser assigns them on
  * its own terms: a node that stands for a node of the ESTree spans exactly its
  * source range, and any other node spans its children together with the
  * terminals of its alternative, except for a left-recursive alternative, which
  * ends at its last child as [[esmeta.parser.ESParser]] does. A node with no
  * child at all, such as an empty `FormalParameters`, has no location.
  */
class EsTreeConverter(val grammar: Grammar) {

  /** convert an ESTree program into an AST of the given goal symbol */
  def apply(
    tree: EsTree,
    code: String,
    goal: String = "Script",
  ): Ast = Conversion(code).program(tree, goal)

  // ---------------------------------------------------------------------------
  // grammar utilities
  // ---------------------------------------------------------------------------
  private val nameMap = grammar.nameMap
  private val lexicalNames = grammar.lexicalNames

  private def prodOf(name: String): Production =
    nameMap.getOrElse(name, error(s"unknown production: $name"))

  private def argsSetOf(name: String, args: List[Boolean]): Set[String] =
    val params = prodOf(name).lhs.params
    if (params.length != args.length)
      error(s"wrong number of arguments for $name: $args")
    (params zip args).collect { case (param, true) => param }.toSet

  private def toBools(
    argsSet: Set[String],
    args: List[NonterminalArgument],
  ): List[Boolean] =
    import NonterminalArgumentKind.*
    args.map {
      case NonterminalArgument(True, _)     => true
      case NonterminalArgument(False, _)    => false
      case NonterminalArgument(Pass, param) => argsSet contains param
    }

  /** the signature of a symbol, used to look up alternatives by shape
    *
    * Lookaheads and `[no LineTerminator here]` are invisible in signatures
    * because they never contribute to a tree.
    */
  private def sigOf(symbol: Symbol): Option[String] = symbol match
    case Terminal(term)        => Some(s"`$term`")
    case Nonterminal(name, _)  => Some(name)
    case Optional(symbol)      => sigOf(symbol).map(_ + "?")
    case ButNot(base, _)       => Some(base.name)
    case ButOnlyIf(base, _, _) => Some(base.name)
    case Empty                 => Some("[empty]")
    case Lookahead(_, _)       => None
    case NoLineTerminator      => None
    case _                     => None

  private def sigOf(rhs: Rhs): String = rhs.symbols.flatMap(sigOf).mkString(" ")

  /** the index of the alternative of `name` with the given signature */
  private val idxCache = Cache[(String, String), Int]()
  private def idxOf(name: String, sig: String): Int =
    idxCache.getOrElseUpdate(
      (name, sig), {
        val found = for {
          (rhs, idx) <- prodOf(name).rhsVec.zipWithIndex
          if sigOf(rhs) == sig
        } yield idx
        found match
          case List(idx) => idx
          case Nil       => error(s"$name has no alternative `$sig`")
          case idxes =>
            error(s"$name has ambiguous alternatives `$sig`: $idxes")
      },
    )

  /** the name of the lexical node produced by a `but not` symbol
    *
    * It must agree with [[esmeta.parser.ESParser]], which names such nodes
    * after the symbol itself.
    */
  private def butNotName(base: Nonterminal, cases: List[Symbol]): String =
    s"$base \\ ${cases.mkString("(", ", ", ")")}"

  /** a child position of an alternative
    *
    * Terminals never become children, so there is one slot per nonterminal of
    * the alternative, in order.
    */
  private case class Slot(
    name: String,
    args: List[Boolean],
    lexical: Boolean,
    optional: Boolean,
  )

  private val terminalCache =
    Cache[(String, Int), (List[String], List[String])]()

  private val slotCache = Cache[(String, List[Boolean], Int), List[Slot]]()
  private def slotsOf(
    name: String,
    args: List[Boolean],
    idx: Int,
  ): List[Slot] = slotCache.getOrElseUpdate(
    (name, args, idx), {
      val argsSet = argsSetOf(name, args)
      for {
        symbol <- prodOf(name).rhsVec(idx).symbols
        if symbol.getNt.isDefined
      } yield
        val optional = symbol match
          case Optional(_) => true
          case _           => false
        val base = symbol match
          case Optional(symbol) => symbol
          case symbol           => symbol
        base match
          case Nonterminal(ntName, ntArgs) =>
            if (lexicalNames contains ntName) Slot(ntName, Nil, true, optional)
            else Slot(ntName, toBools(argsSet, ntArgs), false, optional)
          case ButNot(base, cases) =>
            Slot(butNotName(base, cases), Nil, true, optional)
          case symbol => error(s"unsupported symbol in $name: $symbol")
    },
  )

  /** whether an alternative derives exactly one nonterminal
    *
    * Optional nonterminals are allowed and taken as absent, so that e.g.
    * `SingleNameBinding : BindingIdentifier Initializer?` bridges
    * `BindingElement` and `BindingIdentifier`.
    */
  private def isUnit(rhs: Rhs): Boolean =
    var required = 0
    val ok = rhs.symbols.forall {
      case Lookahead(_, _) | NoLineTerminator => true
      case Optional(symbol)                   => symbol.getNt.isDefined
      case symbol =>
        symbol.getNt.isDefined && { required += 1; true }
    }
    ok && required == 1

  /** a step of a chain of unit alternatives */
  private case class Step(
    name: String,
    args: List[Boolean],
    idx: Int,
    slots: List[Slot],
    at: Int,
  ) {
    def slot: Slot = slots(at)
  }

  /** the unit alternatives of a nonterminal that are available for `args` */
  private val stepCache = Cache[(String, List[Boolean]), List[Step]]()
  private def stepsOf(name: String, args: List[Boolean]): List[Step] =
    stepCache.getOrElseUpdate(
      (name, args), {
        val argsSet = argsSetOf(name, args)
        for {
          (rhs, idx) <- prodOf(name).rhsVec.zipWithIndex.toList
          if rhs.available(argsSet) && isUnit(rhs)
          slots = slotsOf(name, args, idx)
          at = slots.indexWhere(!_.optional)
        } yield Step(name, args, idx, slots, at)
      },
    )

  /** the chain of unit alternatives from a nonterminal down to another one */
  private val chainCache =
    Cache[(String, List[Boolean], String), Option[List[Step]]]()
  private def chainOf(
    from: String,
    args: List[Boolean],
    to: String,
  ): Option[List[Step]] = chainCache.getOrElseUpdate(
    (from, args, to), {
      var visited = Set((from, args))
      // paths are kept reversed, i.e. innermost step first
      var frontier =
        List[(String, List[Boolean], List[Step])]((from, args, Nil))
      var result: Option[List[Step]] = None
      while (result.isEmpty && frontier.nonEmpty)
        val hits = for {
          (name, args, path) <- frontier
          step <- stepsOf(name, args)
          if step.slot.name == to
        } yield step :: path
        hits match
          case Nil        =>
          case List(path) => result = Some(path.reverse)
          case paths =>
            error(s"$from has ambiguous chains to $to: ${paths.length} chains")
        if (result.isEmpty)
          val next = for {
            (name, args, path) <- frontier
            step <- stepsOf(name, args)
            slot = step.slot
            if !slot.lexical
            key = (slot.name, slot.args)
            if !visited.contains(key)
          } yield (key, (slot.name, slot.args, step :: path))
          // keep the first path reaching each nonterminal
          var seen = Set[(String, List[Boolean])]()
          frontier = for {
            (key, entry) <- next
            if !seen.contains(key)
            _ = seen += key
          } yield entry
          visited ++= seen
      result
    },
  )

  private def reachable(
    from: String,
    args: List[Boolean],
    to: String,
  ): Boolean =
    from == to || chainOf(from, args, to).isDefined

  private def error(msg: String): Nothing = throw EsTreeError(msg)

  // ---------------------------------------------------------------------------
  // per-source conversion
  // ---------------------------------------------------------------------------
  /** a child of an alternative under construction */
  private enum Kid:
    /** an absent optional nonterminal */
    case Absent

    /** an ESTree node to convert for the slot */
    case Tree(node: EsTree)

    /** the source range of a lexical token */
    case Text(from: Int, until: Int)

    /** an already built subtree */
    case Done(ast: Ast)

    /** a builder that needs the name and arguments of the slot */
    case Make(build: Slot => Ast)

  import Kid.*

  private class Conversion(src: String) {

    /** the offsets of the beginning of each line, for source positions */
    private lazy val lineStarts: Array[Int] =
      val buffer = Array.newBuilder[Int]
      buffer += 0
      var idx = 0
      while (idx < src.length)
        src.charAt(idx) match
          case CR =>
            if (idx + 1 < src.length && src.charAt(idx + 1) == LF) idx += 1
            buffer += idx + 1
          case LF | LS | PS => buffer += idx + 1
          case _            =>
        idx += 1
      buffer.result()

    private def posOf(offset: Int): Pos =
      var low = 0
      var high = lineStarts.length - 1
      while (low < high)
        val mid = (low + high + 1) / 2
        if (lineStarts(mid) <= offset) low = mid else high = mid - 1
      Pos(low + 1, offset - lineStarts(low) + 1, offset)

    private def locOf(from: Int, until: Int): Option[Loc] =
      Some(Loc(posOf(from), posOf(until)))

    // -------------------------------------------------------------------------
    // source scanning
    // -------------------------------------------------------------------------
    /** skip white spaces, line terminators, and comments */
    private def skipTrivia(from: Int): Int =
      var idx = from
      var moved = true
      while (moved)
        moved = false
        while (idx < src.length && isTrivia(src.charAt(idx))) {
          idx += 1; moved = true
        }
        if (idx + 1 < src.length && src.charAt(idx) == '/')
          src.charAt(idx + 1) match
            case '/' =>
              idx += 2
              while (idx < src.length && !isLineTerminator(src.charAt(idx)))
                idx += 1
              moved = true
            case '*' =>
              val close = src.indexOf("*/", idx + 2)
              idx = if (close < 0) src.length else close + 2
              moved = true
            case _ =>
      idx

    // the code points of `WhiteSpace` and `LineTerminator` of ECMA-262
    private inline val TAB = '\u0009'
    private inline val VT = '\u000b'
    private inline val FF = '\u000c'
    private inline val SP = '\u0020'
    private inline val NBSP = '\u00a0'
    private inline val ZWNBSP = '\ufeff'
    private inline val LF = '\u000a'
    private inline val CR = '\u000d'
    private inline val LS = '\u2028'
    private inline val PS = '\u2029'

    private def isLineTerminator(c: Char): Boolean =
      c == LF || c == CR || c == LS || c == PS

    private def isWhiteSpace(c: Char): Boolean =
      c == SP || c == TAB || c == VT || c == FF || c == NBSP || c == ZWNBSP ||
      // the rest of `USP`, i.e. the code points of category `Zs`
      (c > '\u00ff' && Character.getType(c) == Character.SPACE_SEPARATOR)

    private def isTrivia(c: Char): Boolean =
      isWhiteSpace(c) || isLineTerminator(c)

    /** skip white spaces and line terminators backwards
      *
      * Comments are not skipped: one between a token and its neighbour only
      * makes the span of a node narrower, never wrong.
      */
    private def skipSpaceBack(from: Int): Int =
      var idx = from
      while (idx > 0 && isTrivia(src.charAt(idx - 1))) idx -= 1
      idx

    /** the offset of the next token, if it is the given one */
    private def nextIs(from: Int, token: Char): Option[Int] =
      val idx = skipTrivia(from)
      if (idx < src.length && src.charAt(idx) == token) Some(idx) else None

    /** whether a trailing comma follows the last element of a list
      *
      * Only the closing bracket can follow it otherwise, so the next token
      * being a comma is enough.
      */
    private def hasTrailingComma(from: Int): Boolean =
      nextIs(from, ',').isDefined

    /** whether the next token is a given keyword */
    private def nextIsWord(from: Int, word: String): Boolean =
      val idx = skipTrivia(from)
      src.startsWith(word, idx) && {
        val after = idx + word.length
        after >= src.length || !isIdentifierPart(src.charAt(after))
      }

    private def isIdentifierPart(c: Char): Boolean =
      c == '$' || c == '_' || Character.isUnicodeIdentifierPart(c)

    // -------------------------------------------------------------------------
    // tree construction
    // -------------------------------------------------------------------------
    /** widen a span over the terminals an alternative begins and ends with
      *
      * Terminals are not children, so a node built from its children alone
      * would exclude the `=` of an `Initializer` or the braces of a `Block`.
      */
    private def widen(name: String, idx: Int, span: (Int, Int)): (Int, Int) =
      val (leading, trailing) = fixedTerminals(name, idx)
      var (start, end) = span
      // a terminal that does not match is an absent optional one, or a comment
      // in between, and simply leaves the span as it is
      for (term <- leading.reverse)
        val from = skipSpaceBack(start) - term.length
        if (from >= 0 && src.startsWith(term, from)) start = from
      // the reference parser ends a left-recursive alternative at its last
      // child, so `a[b]` spans up to `b` rather than up to the bracket
      if (!isLeftRecursive(name, idx))
        for (term <- trailing)
          val at = skipTrivia(end)
          if (src.startsWith(term, at)) end = at + term.length
      (start, end)

    /** whether an alternative begins with the nonterminal it defines */
    private def isLeftRecursive(name: String, idx: Int): Boolean =
      prodOf(name)
        .rhsVec(idx)
        .symbols
        .headOption
        .flatMap(_.getNt)
        .exists(_.name == name)

    /** the terminals of an alternative before and after its nonterminals */
    private def fixedTerminals(
      name: String,
      idx: Int,
    ): (List[String], List[String]) =
      terminalCache.getOrElseUpdate(
        (name, idx), {
          val symbols = prodOf(name).rhsVec(idx).symbols
          def terms(symbols: List[Symbol]): List[String] =
            symbols.takeWhile(_.getNt.isEmpty).flatMap {
              case Terminal(term)           => Some(term)
              case Optional(Terminal(term)) => Some(term)
              case _                        => None
            }
          (terms(symbols), terms(symbols.reverse).reverse)
        },
      )

    private def syntactic(
      name: String,
      args: List[Boolean],
      idx: Int,
      children: Vector[Option[Ast]],
    ): Syntactic =
      val syn = Syntactic(name, args, idx, children)
      for { child <- children.flatten } child.parent = Some(syn)
      // Without a range of its own, a node spans its children and the terminals
      // around them. A child that does not know its own range leaves the span
      // unknown: claiming a narrower one would be worse than claiming none,
      // because the stringifier slices the source with it and the specification
      // reads that text back -- an `Arguments` with no range of its own would
      // turn `f()` into `f`.
      val present = children.flatten
      val locs = present.flatMap(_.loc)
      if (locs.nonEmpty && locs.length == present.length)
        val (start, end) =
          widen(name, idx, (locs.head.start.offset, locs.last.end.offset))
        syn.setLoc(locOf(start, end))
      syn

    /** give a node the range its children cannot supply */
    private def at(ast: Ast, from: Int, until: Int): Ast =
      ast.loc = locOf(from, until)
      ast

    private def lexical(slot: Slot, from: Int, until: Int): Lexical =
      val lex = Lexical(slot.name, src.substring(from, until))
      lex.setLoc(locOf(from, until))
      lex

    /** build an alternative of a nonterminal from its children */
    private def build(
      name: String,
      args: List[Boolean],
      sig: String,
      kids: List[Kid],
    ): Syntactic =
      val idx = idxOf(name, sig)
      val slots = slotsOf(name, args, idx)
      if (slots.length != kids.length)
        error(
          s"$name[$idx] takes ${slots.length} children, given ${kids.length}",
        )
      val children = (slots zip kids).map {
        case (slot, Absent) =>
          if (!slot.optional) error(s"${slot.name} of $name[$idx] is required")
          None
        case (slot, Tree(node))        => Some(fill(slot, node))
        case (slot, Text(from, until)) => Some(lexical(slot, from, until))
        case (_, Done(ast))            => Some(ast)
        case (slot, Make(build))       => Some(build(slot))
      }
      syntactic(name, args, idx, children.toVector)

    /** build the child of a slot from an ESTree node */
    private def fill(slot: Slot, node: EsTree): Ast =
      if (slot.lexical) lexical(slot, node.start, node.end)
      else convert(slot.name, slot.args, node)

    /** a left-recursive list of nonterminals, e.g. `StatementList` */
    private def leftList[T](
      name: String,
      args: List[Boolean],
      items: List[T],
    )(
      base: T => (String, List[Kid]),
      cons: T => (String, List[Kid]),
    ): Ast =
      if (items.isEmpty) error(s"$name cannot be empty")
      val (headSig, headKids) = base(items.head)
      items.tail.foldLeft[Ast](build(name, args, headSig, headKids)) {
        case (acc, item) =>
          val (sig, kids) = cons(item)
          build(name, args, sig, Done(acc) :: kids)
      }

    /** a chain of `,` tokens */
    private def elision(slot: Slot, count: Int): Ast =
      leftList(slot.name, slot.args, List.fill(count)(()))(
        _ => ("`,`", Nil),
        _ => ("Elision `,`", Nil),
      )

    // -------------------------------------------------------------------------
    // dispatch
    // -------------------------------------------------------------------------
    /** convert an ESTree node for the given nonterminal and arguments */
    private def convert(
      name: String,
      args: List[Boolean],
      node: EsTree,
    ): Ast =
      val cands = candidates(node)
      val core = cands.find(cand => cand == name || reachable(name, args, cand))
      core match
        case None =>
          error(s"cannot derive ${node.tpe} from $name (tried $cands)")
        case Some(core) if core == name =>
          buildCore(name, args, node)
        case Some(core) =>
          val steps = chainOf(name, args, core).get
          descend(steps, node)

    /** build the steps of a chain, with the ESTree node at its end */
    private def descend(steps: List[Step], node: EsTree): Ast =
      descendWith(steps, fill(_, node))

    /** the core productions of an ESTree node, in order of preference */
    private def candidates(node: EsTree): List[String] = node.tpe match
      case "Identifier" =>
        List(
          "IdentifierReference",
          "BindingIdentifier",
          "LabelIdentifier",
          "IdentifierName",
          "Identifier",
        )
      case "PrivateIdentifier" => List("PrivateIdentifier")
      case "Literal" =>
        node.str("kind") match
          case "null"    => List("NullLiteral")
          case "boolean" => List("BooleanLiteral")
          case "string"  => List("StringLiteral")
          case "regexp"  => List("RegularExpressionLiteral")
          case _         => List("NumericLiteral")
      case "ThisExpression"   => List("PrimaryExpression")
      case "ArrayExpression"  => List("ArrayLiteral")
      case "ObjectExpression" => List("ObjectLiteral")
      case "ParenthesizedExpression" =>
        List("CoverParenthesizedExpressionAndArrowParameterList")
      case "TemplateLiteral"       => List("TemplateLiteral")
      case "SequenceExpression"    => List("Expression")
      case "AssignmentExpression"  => List("AssignmentExpression")
      case "ConditionalExpression" => List("ConditionalExpression")
      case "YieldExpression"       => List("YieldExpression")
      case "AwaitExpression"       => List("AwaitExpression")
      case "UnaryExpression"       => List("UnaryExpression")
      case "UpdateExpression"      => List("UpdateExpression")
      case "BinaryExpression"      => List(binaryCore(node.str("operator")))
      case "LogicalExpression" =>
        List(node.str("operator") match
          case "&&" => "LogicalANDExpression"
          case "||" => "LogicalORExpression"
          case _    => "CoalesceExpression",
        )
      case "ClassExpression"  => List("ClassExpression")
      case "ClassDeclaration" => List("ClassDeclaration")
      case "FunctionExpression" =>
        List(
          (node.bool("async"), node.bool("generator")) match
            case (false, false) => "FunctionExpression"
            case (false, true)  => "GeneratorExpression"
            case (true, false)  => "AsyncFunctionExpression"
            case (true, true)   => "AsyncGeneratorExpression",
        )
      case "FunctionDeclaration" =>
        List(
          (node.bool("async"), node.bool("generator")) match
            case (false, false) => "FunctionDeclaration"
            case (false, true)  => "GeneratorDeclaration"
            case (true, false)  => "AsyncFunctionDeclaration"
            case (true, true)   => "AsyncGeneratorDeclaration",
        )
      case "ArrowFunctionExpression" =>
        List(if (node.bool("async")) "AsyncArrowFunction" else "ArrowFunction")
      case "MetaProperty" =>
        List(
          if (node("meta").str("name") == "new") "NewTarget" else "ImportMeta",
        )
      case "ChainExpression" => List("OptionalExpression")
      case "MemberExpression" | "CallExpression" | "NewExpression" |
          "TaggedTemplateExpression" =>
        List(lhsCore(node))
      case "ImportExpression" => List("ImportCall")
      case "SpreadElement"    => List("SpreadElement", "PropertyDefinition")
      case "ObjectPattern"    => List("ObjectBindingPattern", "ObjectLiteral")
      case "ArrayPattern"     => List("ArrayBindingPattern", "ArrayLiteral")
      case "AssignmentPattern" =>
        node("left").tpe match
          case "Identifier" =>
            List("SingleNameBinding", "BindingElement", "AssignmentExpression")
          case _ => List("BindingElement", "AssignmentExpression")
      case "RestElement" =>
        List(
          "BindingRestElement",
          "BindingRestProperty",
          "FunctionRestParameter",
          "SpreadElement",
          "PropertyDefinition",
        )
      case "Property"           => List("PropertyDefinition", "BindingProperty")
      case "VariableDeclarator" => List("VariableDeclaration", "LexicalBinding")
      case "VariableDeclaration" =>
        List(
          if (node.str("kind") == "var") "VariableStatement"
          else "LexicalDeclaration",
        )
      case "ExpressionStatement"               => List("ExpressionStatement")
      case "BlockStatement"                    => List("Block")
      case "EmptyStatement"                    => List("EmptyStatement")
      case "DebuggerStatement"                 => List("DebuggerStatement")
      case "WithStatement"                     => List("WithStatement")
      case "ReturnStatement"                   => List("ReturnStatement")
      case "LabeledStatement"                  => List("LabelledStatement")
      case "BreakStatement"                    => List("BreakStatement")
      case "ContinueStatement"                 => List("ContinueStatement")
      case "IfStatement"                       => List("IfStatement")
      case "SwitchStatement"                   => List("SwitchStatement")
      case "ThrowStatement"                    => List("ThrowStatement")
      case "TryStatement"                      => List("TryStatement")
      case "CatchClause"                       => List("Catch")
      case "SwitchCase"                        => List("CaseClause")
      case "WhileStatement"                    => List("WhileStatement")
      case "DoWhileStatement"                  => List("DoWhileStatement")
      case "ForStatement"                      => List("ForStatement")
      case "ForInStatement" | "ForOfStatement" => List("ForInOfStatement")
      case "MethodDefinition" | "PropertyDefinition" | "StaticBlock" =>
        List("ClassElement")
      case "ImportDeclaration" => List("ImportDeclaration")
      case "ExportNamedDeclaration" | "ExportDefaultDeclaration" |
          "ExportAllDeclaration" =>
        List("ExportDeclaration")
      case tpe => error(s"unsupported ESTree node: $tpe")

    private def binaryCore(op: String): String = op match
      case "**"                => "ExponentiationExpression"
      case "*" | "/" | "%"     => "MultiplicativeExpression"
      case "+" | "-"           => "AdditiveExpression"
      case "<<" | ">>" | ">>>" => "ShiftExpression"
      case "<" | ">" | "<=" | ">=" | "instanceof" | "in" =>
        "RelationalExpression"
      case "==" | "!=" | "===" | "!==" => "EqualityExpression"
      case "&"                         => "BitwiseANDExpression"
      case "^"                         => "BitwiseXORExpression"
      case "|"                         => "BitwiseORExpression"
      case op                          => error(s"unknown binary operator: $op")

    // -------------------------------------------------------------------------
    // member, call, and optional expressions
    // -------------------------------------------------------------------------
    /** the base of a member or call expression */
    private def baseOf(node: EsTree): EsTree =
      if (node.tpe == "MemberExpression") node("object")
      else if (node.tpe == "TaggedTemplateExpression") node("tag")
      else node("callee")

    /** whether an optional link appears in the spine of a node */
    private def isOptional(node: EsTree): Boolean = node.tpe match
      case "MemberExpression" | "CallExpression" =>
        node.bool("optional") || isOptional(baseOf(node))
      case "TaggedTemplateExpression" => isOptional(baseOf(node))
      case _                          => false

    /** whether a `new` expression is followed by an argument list */
    private def hasArguments(node: EsTree): Boolean =
      node.list("arguments").nonEmpty ||
      nextIs(baseOf(node).end, '(').exists(_ < node.end)

    /** whether a core production derives `CallExpression` */
    private def isCallLevel(core: String): Boolean = core match
      case "CallExpression" | "SuperCall" | "ImportCall" => true
      case _                                             => false

    /** the level of a left-hand-side expression in the grammar */
    private def lhsCore(node: EsTree): String =
      if (isOptional(node)) "OptionalExpression"
      else
        node.tpe match
          case "NewExpression" =>
            if (hasArguments(node)) "MemberExpression" else "NewExpression"
          case "MemberExpression" =>
            if (baseOf(node).tpe == "Super") "SuperProperty"
            else if (isCallLevel(lhsCore(baseOf(node)))) "CallExpression"
            else "MemberExpression"
          case "CallExpression" =>
            baseOf(node).tpe match
              case "Super"  => "SuperCall"
              case "Import" => "ImportCall"
              case _        => "CallExpression"
          case "ImportExpression" => "ImportCall"
          case "TaggedTemplateExpression" =>
            if (isCallLevel(lhsCore(baseOf(node)))) "CallExpression"
            else "MemberExpression"
          case _ => "MemberExpression"

    // -------------------------------------------------------------------------
    // top level
    // -------------------------------------------------------------------------
    def program(tree: EsTree, goal: String): Ast =
      val body = tree.list("body")
      val ast = goal match
        case "Script" =>
          build(
            "Script",
            Nil,
            "ScriptBody?",
            List(
              if (body.isEmpty) Absent
              else
                Make(slot =>
                  build(
                    slot.name,
                    slot.args,
                    "StatementList",
                    List(Make(statements(_, body))),
                  ),
                ),
            ),
          )
        case "Module" =>
          build(
            "Module",
            Nil,
            "ModuleBody?",
            List(
              if (body.isEmpty) Absent
              else
                Make(slot =>
                  build(
                    slot.name,
                    slot.args,
                    "ModuleItemList",
                    List(Make(moduleItems(_, body))),
                  ),
                ),
            ),
          )
        case goal => error(s"unsupported goal symbol: $goal")
      // like the reference parser, the root spans its body rather than the
      // whole text, so that a leading comment stays outside of it
      ast.setLoc(locOf(0, src.length))
      ast

    private def statements(slot: Slot, items: List[EsTree]): Ast =
      leftList(slot.name, slot.args, items)(
        item => ("StatementListItem", List(Tree(item))),
        item => ("StatementList StatementListItem", List(Tree(item))),
      )

    private def moduleItems(slot: Slot, items: List[EsTree]): Ast =
      leftList(slot.name, slot.args, items)(
        item => ("ModuleItem", List(Tree(item))),
        item => ("ModuleItemList ModuleItem", List(Tree(item))),
      )

    // -------------------------------------------------------------------------
    // core productions
    // -------------------------------------------------------------------------
    private def buildCore(
      name: String,
      args: List[Boolean],
      node: EsTree,
    ): Ast =
      val ast = core(name, args, node)
      ast match
        case syn: Syntactic => syn.loc = locOf(node.start, node.end)
        case _              =>
      ast

    private def core(name: String, args: List[Boolean], node: EsTree): Ast =
      name match
        // ---------------------------------------------------------------------
        // identifiers
        // ---------------------------------------------------------------------
        case "IdentifierReference" | "BindingIdentifier" | "LabelIdentifier" =>
          // `yield` and `await` are alternatives of their own, but only when
          // spelled literally: `\u0061wait` is an `Identifier`
          src.substring(node.start, node.end) match
            case text @ ("yield" | "await")
                if hasAlternative(name, args, s"`$text`") =>
              build(name, args, s"`$text`", Nil)
            case _ => build(name, args, "Identifier", List(Tree(node)))
        case "Identifier" =>
          build(name, args, "IdentifierName", List(Text(node.start, node.end)))

        // ---------------------------------------------------------------------
        // primary expressions
        // ---------------------------------------------------------------------
        case "PrimaryExpression" => build(name, args, "`this`", Nil)
        case "CoverParenthesizedExpressionAndArrowParameterList" =>
          build(
            name,
            args,
            "`(` Expression `)`",
            List(Tree(node("expression"))),
          )
        case "ArrayLiteral"       => arrayLiteral(name, args, node)
        case "ObjectLiteral"      => objectLiteral(name, args, node)
        case "PropertyDefinition" => propertyDefinition(name, args, node)
        case "BindingProperty"    => bindingProperty(name, args, node)
        case "TemplateLiteral"    => templateLiteral(name, args, node)
        case "SpreadElement" =>
          build(
            name,
            args,
            "`...` AssignmentExpression",
            List(Tree(spreadArg(node))),
          )

        // ---------------------------------------------------------------------
        // operators
        // ---------------------------------------------------------------------
        case "Expression" => sequence(name, args, node)
        case "AdditiveExpression" | "ShiftExpression" | "RelationalExpression" |
            "EqualityExpression" | "BitwiseANDExpression" |
            "BitwiseXORExpression" | "BitwiseORExpression" |
            "LogicalANDExpression" | "LogicalORExpression" |
            "ExponentiationExpression" =>
          val op = node.str("operator")
          val (left, right) = (node("left"), node("right"))
          if (name == "RelationalExpression" && left.tpe == "PrivateIdentifier")
            build(
              name,
              args,
              s"PrivateIdentifier `in` ShiftExpression",
              List(Text(left.start, left.end), Tree(right)),
            )
          else
            val sig = binaryOperand(name) match
              case (leftNt, rightNt) => s"$leftNt `$op` $rightNt"
            build(name, args, sig, List(Tree(left), Tree(right)))
        case "MultiplicativeExpression" =>
          build(
            name,
            args,
            "MultiplicativeExpression MultiplicativeOperator ExponentiationExpression",
            List(
              Tree(node("left")),
              Make(operator(_, node.str("operator"), node("left").end)),
              Tree(node("right")),
            ),
          )
        case "CoalesceExpression" =>
          build(
            name,
            args,
            "CoalesceExpressionHead `??` BitwiseORExpression",
            List(Tree(node("left")), Tree(node("right"))),
          )
        case "ConditionalExpression" =>
          build(
            name,
            args,
            "ShortCircuitExpression `?` AssignmentExpression `:` AssignmentExpression",
            List(
              Tree(node("test")),
              Tree(node("consequent")),
              Tree(node("alternate")),
            ),
          )
        case "UnaryExpression" =>
          build(
            name,
            args,
            s"`${node.str("operator")}` UnaryExpression",
            List(Tree(node("argument"))),
          )
        case "UpdateExpression" =>
          val op = node.str("operator")
          if (node.bool("prefix"))
            build(
              name,
              args,
              s"`$op` UnaryExpression",
              List(Tree(node("argument"))),
            )
          else
            build(
              name,
              args,
              s"LeftHandSideExpression `$op`",
              List(Tree(node("argument"))),
            )
        case "AwaitExpression" =>
          build(
            name,
            args,
            "`await` UnaryExpression",
            List(Tree(node("argument"))),
          )
        case "YieldExpression" =>
          if (!node.has("argument")) build(name, args, "`yield`", Nil)
          else if (node.bool("delegate"))
            build(
              name,
              args,
              "`yield` `*` AssignmentExpression",
              List(Tree(node("argument"))),
            )
          else
            build(
              name,
              args,
              "`yield` AssignmentExpression",
              List(Tree(node("argument"))),
            )
        case "AssignmentExpression" => assignment(name, args, node)

        // ---------------------------------------------------------------------
        // member, call, and optional expressions
        // ---------------------------------------------------------------------
        case "MemberExpression" | "CallExpression" =>
          memberOrCall(name, args, node)
        case "NewExpression" =>
          build(name, args, "`new` NewExpression", List(Tree(baseOf(node))))
        case "SuperProperty" =>
          if (node.bool("computed"))
            build(
              name,
              args,
              "`super` `[` Expression `]`",
              List(Tree(node("property"))),
            )
          else
            build(
              name,
              args,
              "`super` `.` IdentifierName",
              List(Tree(node("property"))),
            )
        case "SuperCall" =>
          build(name, args, "`super` Arguments", List(Make(arguments(_, node))))
        case "ImportCall" =>
          if (node.has("options"))
            build(
              name,
              args,
              "`import` `(` AssignmentExpression `,` AssignmentExpression `,`? `)`",
              List(Tree(node("source")), Tree(node("options"))),
            )
          else
            build(
              name,
              args,
              "`import` `(` AssignmentExpression `,`? `)`",
              List(Tree(node("source"))),
            )
        case "OptionalExpression" => optional(name, args, node)
        case "NewTarget"  => build(name, args, "`new` `.` `target`", Nil)
        case "ImportMeta" => build(name, args, "`import` `.` `meta`", Nil)

        // ---------------------------------------------------------------------
        // functions and classes
        // ---------------------------------------------------------------------
        case "FunctionDeclaration" | "GeneratorDeclaration" |
            "AsyncFunctionDeclaration" | "AsyncGeneratorDeclaration" |
            "FunctionExpression" | "GeneratorExpression" |
            "AsyncFunctionExpression" | "AsyncGeneratorExpression" =>
          function(name, args, node)
        case "ArrowFunction"      => arrowFunction(name, args, node)
        case "AsyncArrowFunction" => asyncArrowFunction(name, args, node)
        case "ClassDeclaration" =>
          node.get("id") match
            case Some(id) =>
              build(
                name,
                args,
                "`class` BindingIdentifier ClassTail",
                List(Tree(id), Make(classTail(_, node))),
              )
            // `export default class {}`
            case None =>
              build(
                name,
                args,
                "`class` ClassTail",
                List(Make(classTail(_, node))),
              )
        case "ClassExpression" =>
          build(
            name,
            args,
            "`class` BindingIdentifier? ClassTail",
            List(
              node.get("id").fold[Kid](Absent)(Tree(_)),
              Make(classTail(_, node)),
            ),
          )
        case "ClassElement" => classElement(name, args, node)

        // ---------------------------------------------------------------------
        // statements
        // ---------------------------------------------------------------------
        case "ExpressionStatement" =>
          build(name, args, "Expression `;`", List(Tree(node("expression"))))
        case "Block" =>
          val body = node.list("body")
          build(
            name,
            args,
            "`{` StatementList? `}`",
            List(if (body.isEmpty) Absent else Make(statements(_, body))),
          )
        case "EmptyStatement"    => build(name, args, "`;`", Nil)
        case "DebuggerStatement" => build(name, args, "`debugger` `;`", Nil)
        case "VariableStatement" =>
          build(
            name,
            args,
            "`var` VariableDeclarationList `;`",
            List(Make(declarations(_, node))),
          )
        case "LexicalDeclaration" =>
          build(
            name,
            args,
            "LetOrConst BindingList `;`",
            List(
              Make(slot => letOrConst(slot, node.str("kind"), node.start)),
              Make(declarations(_, node)),
            ),
          )
        case "VariableDeclaration" | "LexicalBinding" =>
          declarator(name, args, node)
        case "IfStatement" =>
          if (node.has("alternate"))
            build(
              name,
              args,
              "`if` `(` Expression `)` Statement `else` Statement",
              List(
                Tree(node("test")),
                Tree(node("consequent")),
                Tree(node("alternate")),
              ),
            )
          else
            build(
              name,
              args,
              "`if` `(` Expression `)` Statement",
              List(Tree(node("test")), Tree(node("consequent"))),
            )
        case "DoWhileStatement" =>
          build(
            name,
            args,
            "`do` Statement `while` `(` Expression `)` `;`",
            List(Tree(node("body")), Tree(node("test"))),
          )
        case "WhileStatement" =>
          build(
            name,
            args,
            "`while` `(` Expression `)` Statement",
            List(Tree(node("test")), Tree(node("body"))),
          )
        case "ForStatement"     => forStatement(name, args, node)
        case "ForInOfStatement" => forInOfStatement(name, args, node)
        case "ContinueStatement" | "BreakStatement" =>
          val keyword = if (name == "ContinueStatement") "continue" else "break"
          node.get("label") match
            case None => build(name, args, s"`$keyword` `;`", Nil)
            case Some(label) =>
              build(
                name,
                args,
                s"`$keyword` LabelIdentifier `;`",
                List(Tree(label)),
              )
        case "ReturnStatement" =>
          node.get("argument") match
            case None => build(name, args, "`return` `;`", Nil)
            case Some(argument) =>
              build(name, args, "`return` Expression `;`", List(Tree(argument)))
        case "WithStatement" =>
          build(
            name,
            args,
            "`with` `(` Expression `)` Statement",
            List(Tree(node("object")), Tree(node("body"))),
          )
        case "SwitchStatement" =>
          build(
            name,
            args,
            "`switch` `(` Expression `)` CaseBlock",
            List(Tree(node("discriminant")), Make(caseBlock(_, node))),
          )
        case "CaseClause" =>
          val body = node.list("consequent")
          build(
            name,
            args,
            "`case` Expression `:` StatementList?",
            List(
              Tree(node("test")),
              if (body.isEmpty) Absent else Make(statements(_, body)),
            ),
          )
        case "DefaultClause" =>
          val body = node.list("consequent")
          build(
            name,
            args,
            "`default` `:` StatementList?",
            List(if (body.isEmpty) Absent else Make(statements(_, body))),
          )
        case "LabelledStatement" =>
          build(
            name,
            args,
            "LabelIdentifier `:` LabelledItem",
            List(Tree(node("label")), Tree(node("body"))),
          )
        case "ThrowStatement" =>
          build(
            name,
            args,
            "`throw` Expression `;`",
            List(Tree(node("argument"))),
          )
        case "TryStatement" =>
          (node.get("handler"), node.get("finalizer")) match
            case (Some(handler), None) =>
              build(
                name,
                args,
                "`try` Block Catch",
                List(Tree(node("block")), Tree(handler)),
              )
            case (None, Some(finalizer)) =>
              build(
                name,
                args,
                "`try` Block Finally",
                List(Tree(node("block")), Make(finallyClause(_, finalizer))),
              )
            case (Some(handler), Some(finalizer)) =>
              build(
                name,
                args,
                "`try` Block Catch Finally",
                List(
                  Tree(node("block")),
                  Tree(handler),
                  Make(finallyClause(_, finalizer)),
                ),
              )
            case _ => error("a try statement needs a catch or a finally clause")
        case "Catch" =>
          node.get("param") match
            case None =>
              build(name, args, "`catch` Block", List(Tree(node("body"))))
            case Some(param) =>
              build(
                name,
                args,
                "`catch` `(` CatchParameter `)` Block",
                List(Tree(param), Tree(node("body"))),
              )

        // ---------------------------------------------------------------------
        // patterns
        // ---------------------------------------------------------------------
        case "ObjectBindingPattern" => objectPattern(name, args, node)
        case "ArrayBindingPattern"  => arrayPattern(name, args, node)
        case "SingleNameBinding" =>
          build(
            name,
            args,
            "BindingIdentifier Initializer?",
            List(Tree(node("left")), Make(initializer(_, node("right")))),
          )
        case "BindingElement" =>
          build(
            name,
            args,
            "BindingPattern Initializer?",
            List(Tree(node("left")), Make(initializer(_, node("right")))),
          )
        case "BindingRestElement" =>
          val argument = node("argument")
          if (argument.tpe == "Identifier")
            build(
              name,
              args,
              "`...` BindingIdentifier",
              List(Tree(argument)),
            )
          else
            build(name, args, "`...` BindingPattern", List(Tree(argument)))
        case "BindingRestProperty" =>
          build(
            name,
            args,
            "`...` BindingIdentifier",
            List(Tree(node("argument"))),
          )

        // ---------------------------------------------------------------------
        // modules
        // ---------------------------------------------------------------------
        case "ImportDeclaration" => importDeclaration(name, args, node)
        case "ExportDeclaration" => exportDeclaration(name, args, node)

        case name => error(s"unsupported production: $name (${node.tpe})")

    /** whether a nonterminal has an available alternative with a signature */
    private def hasAlternative(
      name: String,
      args: List[Boolean],
      sig: String,
    ): Boolean =
      val argsSet = argsSetOf(name, args)
      prodOf(name).rhsVec.exists(rhs =>
        rhs.available(argsSet) && sigOf(rhs) == sig,
      )

    /** the operands of a binary operator alternative */
    private def binaryOperand(name: String): (String, String) = name match
      case "AdditiveExpression"   => (name, "MultiplicativeExpression")
      case "ShiftExpression"      => (name, "AdditiveExpression")
      case "RelationalExpression" => (name, "ShiftExpression")
      case "EqualityExpression"   => (name, "RelationalExpression")
      case "BitwiseANDExpression" => (name, "EqualityExpression")
      case "BitwiseXORExpression" => (name, "BitwiseANDExpression")
      case "BitwiseORExpression"  => (name, "BitwiseXORExpression")
      case "LogicalANDExpression" => (name, "BitwiseORExpression")
      case "LogicalORExpression"  => (name, "LogicalANDExpression")
      case "ExponentiationExpression" =>
        ("UpdateExpression", "ExponentiationExpression")
      case name => error(s"not a binary expression: $name")

    /** the argument of a spread element or a rest element */
    private def spreadArg(node: EsTree): EsTree = node("argument")

    private def sequence(name: String, args: List[Boolean], node: EsTree): Ast =
      leftList(name, args, node.list("expressions"))(
        item => ("AssignmentExpression", List(Tree(item))),
        item => ("Expression `,` AssignmentExpression", List(Tree(item))),
      )

    private def assignment(
      name: String,
      args: List[Boolean],
      node: EsTree,
    ): Ast =
      val (left, right) = (node("left"), node("right"))
      // a default value in a cover grammar is an `AssignmentPattern`, which has
      // no operator of its own
      node.strOpt("operator").getOrElse("=") match
        case "=" =>
          build(
            name,
            args,
            "LeftHandSideExpression `=` AssignmentExpression",
            List(Tree(left), Tree(right)),
          )
        case op @ ("&&=" | "||=" | "??=") =>
          build(
            name,
            args,
            s"LeftHandSideExpression `$op` AssignmentExpression",
            List(Tree(left), Tree(right)),
          )
        case op =>
          build(
            name,
            args,
            "LeftHandSideExpression AssignmentOperator AssignmentExpression",
            List(
              Tree(left),
              Make(operator(_, op, left.end)),
              Tree(right),
            ),
          )

    private def initializer(slot: Slot, node: EsTree): Ast =
      build(slot.name, slot.args, "`=` AssignmentExpression", List(Tree(node)))

    private def letOrConst(slot: Slot, kind: String, at: Int): Ast =
      val ast = build(slot.name, slot.args, s"`$kind`", Nil)
      ast.loc = locOf(at, at + kind.length)
      ast

    /** a node whose alternative is one operator token */
    private def operator(slot: Slot, op: String, from: Int): Ast =
      val ast = build(slot.name, slot.args, s"`$op`", Nil)
      val at = src.indexOf(op, from)
      if (at >= 0) ast.loc = locOf(at, at + op.length)
      ast

    private def declarations(slot: Slot, node: EsTree): Ast =
      val (baseSig, consSig) =
        if (slot.name == "VariableDeclarationList")
          (
            "VariableDeclaration",
            "VariableDeclarationList `,` VariableDeclaration",
          )
        else ("LexicalBinding", "BindingList `,` LexicalBinding")
      leftList(slot.name, slot.args, node.list("declarations"))(
        item => (baseSig, List(Tree(item))),
        item => (consSig, List(Tree(item))),
      )

    private def declarator(
      name: String,
      args: List[Boolean],
      node: EsTree,
    ): Ast =
      val id = node("id")
      val init = node.get("init")
      if (id.tpe == "Identifier")
        build(
          name,
          args,
          "BindingIdentifier Initializer?",
          List(Tree(id), init.fold[Kid](Absent)(x => Make(initializer(_, x)))),
        )
      else
        build(
          name,
          args,
          "BindingPattern Initializer",
          List(
            Tree(id),
            Make(
              initializer(
                _,
                init.getOrElse(
                  error("a destructuring binding needs an initializer"),
                ),
              ),
            ),
          ),
        )

    // -------------------------------------------------------------------------
    // array and object literals
    // -------------------------------------------------------------------------
    /** the elements of an array, split at the last present one */
    private def splitElements(
      items: List[Option[EsTree]],
    ): (List[(Int, EsTree)], Int) =
      val present = items.zipWithIndex.collect {
        case (Some(item), idx) => (idx, item)
      }
      present.lastOption match
        case None           => (Nil, items.length)
        case Some((idx, _)) =>
          // the number of `,` after the last element, i.e. the trailing elision
          val elision = items.length - idx - 1
          // elisions before each element
          val withElisions = present.zipWithIndex.map {
            case ((idx, item), order) =>
              val prev = if (order == 0) -1 else present(order - 1)._1
              (idx - prev - 1, item)
          }
          (withElisions, elision)

    private def arrayLiteral(
      name: String,
      args: List[Boolean],
      node: EsTree,
    ): Ast =
      // an `ArrayPattern` reaches here when it stands for the cover grammar
      val items = node.items("elements")
      val (elements, trailing) = splitElements(items)
      val close = node.end - 1
      def elisionKid(count: Int): Kid =
        if (count == 0) Absent else Make(elision(_, count))
      if (elements.isEmpty)
        build(name, args, "`[` Elision? `]`", List(elisionKid(trailing)))
      else
        val list = Make(elementList(_, elements))
        if (trailing > 0)
          build(
            name,
            args,
            "`[` ElementList `,` Elision? `]`",
            List(list, elisionKid(trailing)),
          )
        else if (hasTrailingComma(elements.last._2.end))
          build(
            name,
            args,
            "`[` ElementList `,` Elision? `]`",
            List(list, Absent),
          )
        else build(name, args, "`[` ElementList `]`", List(list))

    private def isSpread(node: EsTree): Boolean =
      node.tpe == "SpreadElement" || node.tpe == "RestElement"

    private def elementList(
      slot: Slot,
      elements: List[(Int, EsTree)],
    ): Ast =
      leftList(slot.name, slot.args, elements)(
        {
          case (count, item) =>
            val elisions = if (count == 0) Absent else Make(elision(_, count))
            if (isSpread(item))
              ("Elision? SpreadElement", List(elisions, Tree(item)))
            else ("Elision? AssignmentExpression", List(elisions, Tree(item)))
        },
        {
          case (count, item) =>
            val elisions = if (count == 0) Absent else Make(elision(_, count))
            if (isSpread(item))
              (
                "ElementList `,` Elision? SpreadElement",
                List(elisions, Tree(item)),
              )
            else
              (
                "ElementList `,` Elision? AssignmentExpression",
                List(elisions, Tree(item)),
              )
        },
      )

    private def objectLiteral(
      name: String,
      args: List[Boolean],
      node: EsTree,
    ): Ast =
      // an `ObjectPattern` reaches here when it stands for the cover grammar
      val props = node.list("properties")
      if (props.isEmpty) build(name, args, "`{` `}`", Nil)
      else
        val list = Make(slot =>
          leftList(slot.name, slot.args, props)(
            item => ("PropertyDefinition", List(Tree(item))),
            item =>
              (
                "PropertyDefinitionList `,` PropertyDefinition",
                List(Tree(item)),
              ),
          ),
        )
        if (hasTrailingComma(props.last.end))
          build(name, args, "`{` PropertyDefinitionList `,` `}`", List(list))
        else build(name, args, "`{` PropertyDefinitionList `}`", List(list))

    private def propertyDefinition(
      name: String,
      args: List[Boolean],
      node: EsTree,
    ): Ast =
      if (isSpread(node))
        build(
          name,
          args,
          "`...` AssignmentExpression",
          List(Tree(spreadArg(node))),
        )
      else if (node.bool("method") || node.str("kind") != "init")
        build(name, args, "MethodDefinition", List(Make(method(_, node))))
      else if (node.bool("shorthand"))
        node("value").tpe match
          case "AssignmentPattern" =>
            build(
              name,
              args,
              "CoverInitializedName",
              List(Make(coverInitializedName(_, node("value")))),
            )
          case _ =>
            build(name, args, "IdentifierReference", List(Tree(node("key"))))
      else
        build(
          name,
          args,
          "PropertyName `:` AssignmentExpression",
          List(Make(propertyName(_, node)), Tree(node("value"))),
        )

    private def coverInitializedName(slot: Slot, node: EsTree): Ast =
      build(
        slot.name,
        slot.args,
        "IdentifierReference Initializer",
        List(Tree(node("left")), Make(initializer(_, node("right")))),
      )

    private def propertyName(slot: Slot, node: EsTree): Ast =
      val key = node("key")
      if (node.bool("computed"))
        build(
          slot.name,
          slot.args,
          "ComputedPropertyName",
          List(
            Make(inner =>
              build(
                inner.name,
                inner.args,
                "`[` AssignmentExpression `]`",
                List(Tree(key)),
              ),
            ),
          ),
        )
      else convert(slot.name, slot.args, key)

    // -------------------------------------------------------------------------
    // binding patterns
    // -------------------------------------------------------------------------
    private def objectPattern(
      name: String,
      args: List[Boolean],
      node: EsTree,
    ): Ast =
      val props = node.list("properties")
      val (rest, rules) = props.lastOption match
        case Some(last) if last.tpe == "RestElement" =>
          (Some(last), props.init)
        case _ => (None, props)
      val close = node.end - 1
      val restKid = rest.fold[Kid](Absent)(Tree(_))
      if (rules.isEmpty)
        rest match
          case None => build(name, args, "`{` `}`", Nil)
          case Some(_) =>
            build(name, args, "`{` BindingRestProperty `}`", List(restKid))
      else
        val list = Make(slot =>
          leftList(slot.name, slot.args, rules)(
            item => ("BindingProperty", List(Tree(item))),
            item =>
              ("BindingPropertyList `,` BindingProperty", List(Tree(item))),
          ),
        )
        val last = rules.last.end
        if (rest.isDefined || hasTrailingComma(last))
          build(
            name,
            args,
            "`{` BindingPropertyList `,` BindingRestProperty? `}`",
            List(list, restKid),
          )
        else build(name, args, "`{` BindingPropertyList `}`", List(list))

    private def bindingProperty(
      name: String,
      args: List[Boolean],
      node: EsTree,
    ): Ast =
      if (node.bool("shorthand"))
        build(name, args, "SingleNameBinding", List(Tree(node("value"))))
      else
        build(
          name,
          args,
          "PropertyName `:` BindingElement",
          List(Make(propertyName(_, node)), Tree(node("value"))),
        )

    private def arrayPattern(
      name: String,
      args: List[Boolean],
      node: EsTree,
    ): Ast =
      val items = node.items("elements")
      val (rest, rules) = items.lastOption match
        case Some(Some(last)) if last.tpe == "RestElement" =>
          (Some(last), items.init)
        case _ => (None, items)
      val (elements, trailing) = splitElements(rules)
      val close = node.end - 1
      val restKid = rest.fold[Kid](Absent)(Tree(_))
      def elisionKid(count: Int): Kid =
        if (count == 0) Absent else Make(elision(_, count))
      if (elements.isEmpty)
        build(
          name,
          args,
          "`[` Elision? BindingRestElement? `]`",
          List(elisionKid(trailing), restKid),
        )
      else
        val list = Make(slot =>
          leftList(slot.name, slot.args, elements)(
            {
              case (count, item) =>
                (
                  "BindingElisionElement",
                  List(Make(bindingElisionElement(_, count, item))),
                )
            },
            {
              case (count, item) =>
                (
                  "BindingElementList `,` BindingElisionElement",
                  List(Make(bindingElisionElement(_, count, item))),
                )
            },
          ),
        )
        if (trailing > 0)
          build(
            name,
            args,
            "`[` BindingElementList `,` Elision? BindingRestElement? `]`",
            List(list, elisionKid(trailing), restKid),
          )
        else if (rest.isDefined || hasTrailingComma(elements.last._2.end))
          build(
            name,
            args,
            "`[` BindingElementList `,` Elision? BindingRestElement? `]`",
            List(list, Absent, restKid),
          )
        else build(name, args, "`[` BindingElementList `]`", List(list))

    private def bindingElisionElement(
      slot: Slot,
      count: Int,
      item: EsTree,
    ): Ast =
      build(
        slot.name,
        slot.args,
        "Elision? BindingElement",
        List(if (count <= 0) Absent else Make(elision(_, count)), Tree(item)),
      )

    // -------------------------------------------------------------------------
    // templates
    // -------------------------------------------------------------------------
    private def templateLiteral(
      name: String,
      args: List[Boolean],
      node: EsTree,
    ): Ast =
      val quasis = node.list("quasis")
      val exprs = node.list("expressions")
      if (quasis.length == 1)
        build(
          name,
          args,
          "NoSubstitutionTemplate",
          List(Text(node.start, node.end)),
        )
      else
        build(
          name,
          args,
          "SubstitutionTemplate",
          List(
            Make(slot =>
              build(
                slot.name,
                slot.args,
                "TemplateHead Expression TemplateSpans",
                List(
                  Text(node.start, quasis.head.end + 2),
                  Tree(exprs.head),
                  Make(templateSpans(_, quasis.tail, exprs.tail)),
                ),
              ),
            ),
          ),
        )

    private def templateSpans(
      slot: Slot,
      quasis: List[EsTree],
      exprs: List[EsTree],
    ): Ast =
      val tail = quasis.last
      val tailText = Text(tail.start - 1, tail.end + 1)
      if (quasis.length == 1)
        build(slot.name, slot.args, "TemplateTail", List(tailText))
      else
        val middles = quasis.init zip exprs
        build(
          slot.name,
          slot.args,
          "TemplateMiddleList TemplateTail",
          List(
            Make(inner =>
              leftList(inner.name, inner.args, middles)(
                {
                  case (quasi, expr) =>
                    (
                      "TemplateMiddle Expression",
                      List(Text(quasi.start - 1, quasi.end + 2), Tree(expr)),
                    )
                },
                {
                  case (quasi, expr) =>
                    (
                      "TemplateMiddleList TemplateMiddle Expression",
                      List(Text(quasi.start - 1, quasi.end + 2), Tree(expr)),
                    )
                },
              ),
            ),
            tailText,
          ),
        )

    // -------------------------------------------------------------------------
    // member, call, and optional expressions
    // -------------------------------------------------------------------------
    /** the range of an argument list that follows a callee */
    private def arguments(slot: Slot, node: EsTree): Ast =
      val items = node.list("arguments")
      val close = node.end - 1
      if (items.isEmpty)
        val open = skipTrivia(baseOf(node).end)
        at(build(slot.name, slot.args, "`(` `)`", Nil), open, node.end)
      else
        val list = Make(inner =>
          leftList(inner.name, inner.args, items)(
            item =>
              if (isSpread(item))
                ("`...` AssignmentExpression", List(Tree(spreadArg(item))))
              else ("AssignmentExpression", List(Tree(item))),
            item =>
              if (isSpread(item))
                (
                  "ArgumentList `,` `...` AssignmentExpression",
                  List(Tree(spreadArg(item))),
                )
              else ("ArgumentList `,` AssignmentExpression", List(Tree(item))),
          ),
        )
        if (hasTrailingComma(items.last.end))
          build(slot.name, slot.args, "`(` ArgumentList `,` `)`", List(list))
        else build(slot.name, slot.args, "`(` ArgumentList `)`", List(list))

    private def memberOrCall(
      name: String,
      args: List[Boolean],
      node: EsTree,
    ): Ast = node.tpe match
      case "NewExpression" =>
        build(
          name,
          args,
          "`new` MemberExpression Arguments",
          List(Tree(baseOf(node)), Make(arguments(_, node))),
        )
      case "TaggedTemplateExpression" =>
        build(
          name,
          args,
          s"$name TemplateLiteral",
          List(Tree(baseOf(node)), Tree(node("quasi"))),
        )
      case "CallExpression" =>
        val callee = baseOf(node)
        // `SuperCall` and `ImportCall` are alternatives of `CallExpression`, so
        // calling their result is the left-recursive alternative as well
        if (isCallLevel(lhsCore(callee)))
          build(
            name,
            args,
            "CallExpression Arguments",
            List(Tree(callee), Make(arguments(_, node))),
          )
        else
          build(
            name,
            args,
            "CoverCallExpressionAndAsyncArrowHead",
            List(
              Make(cover =>
                build(
                  cover.name,
                  cover.args,
                  "MemberExpression Arguments",
                  List(Tree(callee), Make(arguments(_, node))),
                ),
              ),
            ),
          )
      case _ =>
        val obj = Tree(baseOf(node))
        val property = node("property")
        if (node.bool("computed"))
          build(
            name,
            args,
            s"$name `[` Expression `]`",
            List(obj, Tree(property)),
          )
        else if (property.tpe == "PrivateIdentifier")
          build(
            name,
            args,
            s"$name `.` PrivateIdentifier",
            List(obj, Tree(property)),
          )
        else
          build(
            name,
            args,
            s"$name `.` IdentifierName",
            List(obj, Tree(property)),
          )

    private def optional(
      name: String,
      args: List[Boolean],
      node: EsTree,
    ): Ast =
      val target =
        if (node.tpe == "ChainExpression") node("expression") else node
      // walk out of the chain until its first optional link, which is the one
      // written `?.`; prepending keeps the links in innermost-first order
      var links = List[EsTree]()
      var cur = target
      while (!cur.bool("optional"))
        links = cur :: links
        cur = baseOf(cur)
      val base = baseOf(cur)
      val chain = cur :: links
      val baseSig = lhsCore(base) match
        case "OptionalExpression" => "OptionalExpression OptionalChain"
        case "CallExpression" | "SuperCall" | "ImportCall" =>
          "CallExpression OptionalChain"
        case _ => "MemberExpression OptionalChain"
      build(
        name,
        args,
        baseSig,
        List(Tree(base), Make(optionalChain(_, chain))),
      )

    private def optionalChain(slot: Slot, links: List[EsTree]): Ast =
      leftList(slot.name, slot.args, links)(
        link => linkSig(link, first = true),
        link => linkSig(link, first = false),
      )

    private def linkSig(link: EsTree, first: Boolean): (String, List[Kid]) =
      val prefix = if (first) "`?.`" else "OptionalChain"
      link.tpe match
        case "CallExpression" =>
          (s"$prefix Arguments", List(Make(arguments(_, link))))
        case "TaggedTemplateExpression" =>
          (s"$prefix TemplateLiteral", List(Tree(link("quasi"))))
        case _ =>
          val property = link("property")
          if (link.bool("computed"))
            (s"$prefix `[` Expression `]`", List(Tree(property)))
          else if (property.tpe == "PrivateIdentifier")
            val sig =
              if (first) s"$prefix PrivateIdentifier"
              else s"$prefix `.` PrivateIdentifier"
            (sig, List(Tree(property)))
          else
            val sig =
              if (first) s"$prefix IdentifierName"
              else s"$prefix `.` IdentifierName"
            (sig, List(Tree(property)))

    // -------------------------------------------------------------------------
    // functions
    // -------------------------------------------------------------------------
    /** the body of a function, i.e. `FunctionBody` and its variants */
    private def functionBody(slot: Slot, node: EsTree): Ast =
      // `GeneratorBody`, `AsyncFunctionBody`, ... all derive `FunctionBody`
      val steps = chainOf(slot.name, slot.args, "FunctionStatementList")
      steps match
        case Some(steps) =>
          val body = node.list("body")
          def leaf(slot: Slot): Ast =
            build(
              slot.name,
              slot.args,
              "StatementList?",
              List(if (body.isEmpty) Absent else Make(statements(_, body))),
            )
          descendWith(steps, leaf)
        case None =>
          error(s"${slot.name} does not derive FunctionStatementList")

    /** build the steps of a chain, innermost last */
    private def descendWith(steps: List[Step], leaf: Slot => Ast): Ast =
      steps match
        case Nil => error("empty chain")
        case step :: rest =>
          val child =
            if (rest.isEmpty) leaf(step.slot) else descendWith(rest, leaf)
          val children = step.slots.zipWithIndex.map {
            case (_, idx) => if (idx == step.at) Some(child) else None
          }
          syntactic(step.name, step.args, step.idx, children.toVector)

    /** the formal parameters of a function */
    private def formalParameters(slot: Slot, params: List[EsTree]): Ast =
      val name = slot.name
      // `UniqueFormalParameters` and `PropertySetParameterList` wrap them
      if (name != "FormalParameters")
        val steps = chainOf(name, slot.args, "FormalParameters")
        steps match
          case Some(steps) => descendWith(steps, formalParameters(_, params))
          case None        =>
            // `PropertySetParameterList : FormalParameter`
            build(name, slot.args, "FormalParameter", List(Tree(params.head)))
      else
        val (rest, rules) = params.lastOption match
          case Some(last) if last.tpe == "RestElement" =>
            (Some(last), params.init)
          case _ => (None, params)
        val restKid = rest.fold[Kid](Absent)(Tree(_))
        if (rules.isEmpty)
          rest match
            case None => build(name, slot.args, "[empty]", Nil)
            case Some(_) =>
              build(name, slot.args, "FunctionRestParameter", List(restKid))
        else
          val list = Make(inner =>
            leftList(inner.name, inner.args, rules)(
              item => ("FormalParameter", List(Tree(item))),
              item =>
                ("FormalParameterList `,` FormalParameter", List(Tree(item))),
            ),
          )
          rest match
            case Some(_) =>
              build(
                name,
                slot.args,
                "FormalParameterList `,` FunctionRestParameter",
                List(list, restKid),
              )
            case None =>
              if (hasTrailingComma(rules.last.end))
                build(name, slot.args, "FormalParameterList `,`", List(list))
              else build(name, slot.args, "FormalParameterList", List(list))

    private def function(
      name: String,
      args: List[Boolean],
      node: EsTree,
    ): Ast =
      val params = node.list("params")
      val body = node("body")
      val paramsKid = Make(formalParameters(_, params))
      val bodyKid = Make(functionBody(_, body))
      val star = if (node.bool("generator")) "`*` " else ""
      val async = if (node.bool("async")) "`async` " else ""
      val bodyNt = name match
        case "GeneratorDeclaration" | "GeneratorExpression" => "GeneratorBody"
        case "AsyncFunctionDeclaration" | "AsyncFunctionExpression" =>
          "AsyncFunctionBody"
        case "AsyncGeneratorDeclaration" | "AsyncGeneratorExpression" =>
          "AsyncGeneratorBody"
        case _ => "FunctionBody"
      val isExpression = name.endsWith("Expression")
      node.get("id") match
        case Some(id) =>
          build(
            name,
            args,
            s"$async`function` ${star}BindingIdentifier${if (isExpression) "?"
            else ""} `(` FormalParameters `)` `{` $bodyNt `}`",
            List(Tree(id), paramsKid, bodyKid),
          )
        case None if isExpression =>
          build(
            name,
            args,
            s"$async`function` ${star}BindingIdentifier? `(` FormalParameters `)` `{` $bodyNt `}`",
            List(Absent, paramsKid, bodyKid),
          )
        case None =>
          build(
            name,
            args,
            s"$async`function` ${star}`(` FormalParameters `)` `{` $bodyNt `}`",
            List(paramsKid, bodyKid),
          )

    private def conciseBody(slot: Slot, node: EsTree, expr: Boolean): Ast =
      if (expr)
        build(
          slot.name,
          slot.args,
          "ExpressionBody",
          List(
            Make(inner =>
              build(
                inner.name,
                inner.args,
                "AssignmentExpression",
                List(Tree(node)),
              ),
            ),
          ),
        )
      else
        build(
          slot.name,
          slot.args,
          if (slot.name == "AsyncConciseBody") "`{` AsyncFunctionBody `}`"
          else "`{` FunctionBody `}`",
          List(Make(functionBody(_, node))),
        )

    /** whether the parameters of an arrow function are parenthesized */
    private def isParenthesized(node: EsTree): Boolean =
      val from =
        if (node.bool("async")) skipTrivia(node.start + "async".length)
        else node.start
      from < src.length && src.charAt(from) == '('

    private def arrowFunction(
      name: String,
      args: List[Boolean],
      node: EsTree,
    ): Ast =
      build(
        name,
        args,
        "ArrowParameters `=>` ConciseBody",
        List(
          Make(arrowParameters(_, node)),
          Make(conciseBody(_, node("body"), node.bool("expression"))),
        ),
      )

    private def arrowParameters(slot: Slot, node: EsTree): Ast =
      val params = node.list("params")
      if (!isParenthesized(node))
        build(
          slot.name,
          slot.args,
          "BindingIdentifier",
          List(Tree(params.head)),
        )
      else
        build(
          slot.name,
          slot.args,
          "CoverParenthesizedExpressionAndArrowParameterList",
          List(Make(parenCover(_, node, params))),
        )

    /** the cover grammar of parenthesized expressions and arrow parameters */
    private def parenCover(
      slot: Slot,
      node: EsTree,
      params: List[EsTree],
    ): Ast =
      val name = slot.name
      val args = slot.args
      val (rest, rules) = params.lastOption match
        case Some(last) if last.tpe == "RestElement" =>
          (Some(last), params.init)
        case _ => (None, params)
      def restSig(prefix: String): (String, List[Kid]) =
        val target = rest.get("argument")
        if (target.tpe == "Identifier")
          (s"$prefix `...` BindingIdentifier `)`", List(Tree(target)))
        else (s"$prefix `...` BindingPattern `)`", List(Tree(target)))
      val close = node.end
      (rules, rest) match
        case (Nil, None) => build(name, args, "`(` `)`", Nil)
        case (Nil, Some(_)) =>
          val (sig, kids) = restSig("`(`")
          build(name, args, sig, kids)
        case (rules, restOpt) =>
          val list = Make(inner =>
            leftList(inner.name, inner.args, rules)(
              item => ("AssignmentExpression", List(Tree(item))),
              item => ("Expression `,` AssignmentExpression", List(Tree(item))),
            ),
          )
          restOpt match
            case Some(_) =>
              val (sig, kids) = restSig("`(` Expression `,`")
              build(name, args, sig, list :: kids)
            case None =>
              if (hasTrailingComma(rules.last.end))
                build(name, args, "`(` Expression `,` `)`", List(list))
              else build(name, args, "`(` Expression `)`", List(list))

    private def asyncArrowFunction(
      name: String,
      args: List[Boolean],
      node: EsTree,
    ): Ast =
      val params = node.list("params")
      val bodyKid = Make(conciseBody(_, node("body"), node.bool("expression")))
      if (!isParenthesized(node))
        build(
          name,
          args,
          "`async` AsyncArrowBindingIdentifier `=>` AsyncConciseBody",
          List(
            Make(slot =>
              build(
                slot.name,
                slot.args,
                "BindingIdentifier",
                List(Tree(params.head)),
              ),
            ),
            bodyKid,
          ),
        )
      else
        build(
          name,
          args,
          "CoverCallExpressionAndAsyncArrowHead `=>` AsyncConciseBody",
          List(Make(asyncArrowHead(_, node, params)), bodyKid),
        )

    /** `async (a, b)` seen as a call of `async` with arguments */
    private def asyncArrowHead(
      slot: Slot,
      node: EsTree,
      params: List[EsTree],
    ): Ast =
      val asyncEnd = node.start + "async".length
      build(
        slot.name,
        slot.args,
        "MemberExpression Arguments",
        List(
          Make(inner =>
            descendWith(
              chainOf(inner.name, inner.args, "Identifier").get,
              leaf =>
                build(
                  leaf.name,
                  leaf.args,
                  "IdentifierName",
                  List(Text(node.start, asyncEnd)),
                ),
            ),
          ),
          Make(inner =>
            argumentsOf(inner, params, skipTrivia(asyncEnd), node.end),
          ),
        ),
      )

    /** an argument list built from parameter nodes, for the cover grammars */
    private def argumentsOf(
      slot: Slot,
      params: List[EsTree],
      open: Int,
      close: Int,
    ): Ast =
      if (params.isEmpty)
        at(build(slot.name, slot.args, "`(` `)`", Nil), open, close)
      else
        val list = Make(inner =>
          leftList(inner.name, inner.args, params)(
            item =>
              if (item.tpe == "RestElement")
                ("`...` AssignmentExpression", List(Tree(item("argument"))))
              else ("AssignmentExpression", List(Tree(item))),
            item =>
              if (item.tpe == "RestElement")
                (
                  "ArgumentList `,` `...` AssignmentExpression",
                  List(Tree(item("argument"))),
                )
              else ("ArgumentList `,` AssignmentExpression", List(Tree(item))),
          ),
        )
        if (hasTrailingComma(params.last.end))
          build(slot.name, slot.args, "`(` ArgumentList `,` `)`", List(list))
        else build(slot.name, slot.args, "`(` ArgumentList `)`", List(list))

    // -------------------------------------------------------------------------
    // classes
    // -------------------------------------------------------------------------
    private def classTail(slot: Slot, node: EsTree): Ast =
      val body = classElements(node("body"))
      build(
        slot.name,
        slot.args,
        "ClassHeritage? `{` ClassBody? `}`",
        List(
          node
            .get("superClass")
            .fold[Kid](Absent)(sup =>
              Make(inner =>
                build(
                  inner.name,
                  inner.args,
                  "`extends` LeftHandSideExpression",
                  List(Tree(sup)),
                ),
              ),
            ),
          if (body.isEmpty) Absent
          else
            Make(inner =>
              build(
                inner.name,
                inner.args,
                "ClassElementList",
                List(
                  Make(list =>
                    leftList(list.name, list.args, body)(
                      item => ("ClassElement", List(classElementKid(item))),
                      item =>
                        (
                          "ClassElementList ClassElement",
                          List(classElementKid(item)),
                        ),
                    ),
                  ),
                ),
              ),
            ),
        ),
      )

    /** the elements of a class body, including the lone `;` ones
      *
      * ESTree drops `;` class elements, but the grammar keeps one node per `;`,
      * so they are recovered from the gaps between the remaining elements. Only
      * white space, comments, and `;` can appear in such a gap.
      */
    private def classElements(body: EsTree): List[Either[Int, EsTree]] =
      val close = body.end - 1
      var result = List.newBuilder[Either[Int, EsTree]]
      var pos = body.start + 1
      for (element <- body.list("body")) {
        semicolons(pos, element.start).foreach(at => result += Left(at))
        result += Right(element)
        pos = element.end
      }
      semicolons(pos, close).foreach(at => result += Left(at))
      result.result()

    private def semicolons(from: Int, until: Int): List[Int] =
      var result = List.newBuilder[Int]
      var idx = skipTrivia(from)
      while (idx < until && src.charAt(idx) == ';')
        result += idx
        idx = skipTrivia(idx + 1)
      result.result()

    private def classElementKid(item: Either[Int, EsTree]): Kid = item match
      case Right(node) => Tree(node)
      case Left(_)     => Make(slot => build(slot.name, slot.args, "`;`", Nil))

    private def classElement(
      name: String,
      args: List[Boolean],
      node: EsTree,
    ): Ast =
      val static = node.bool("static")
      node.tpe match
        case "StaticBlock" =>
          build(
            name,
            args,
            "ClassStaticBlock",
            List(
              Make(slot =>
                build(
                  slot.name,
                  slot.args,
                  "`static` `{` ClassStaticBlockBody `}`",
                  List(
                    Make(body =>
                      build(
                        body.name,
                        body.args,
                        "ClassStaticBlockStatementList",
                        List(
                          Make(list =>
                            build(
                              list.name,
                              list.args,
                              "StatementList?",
                              List(
                                node.list("body") match
                                  case Nil   => Absent
                                  case items => Make(statements(_, items)),
                              ),
                            ),
                          ),
                        ),
                      ),
                    ),
                  ),
                ),
              ),
            ),
          )
        case "PropertyDefinition" =>
          val sig =
            if (static) "`static` FieldDefinition `;`"
            else "FieldDefinition `;`"
          build(name, args, sig, List(Make(fieldDefinition(_, node))))
        case _ =>
          val sig =
            if (static) "`static` MethodDefinition" else "MethodDefinition"
          build(name, args, sig, List(Make(method(_, node))))

    private def fieldDefinition(slot: Slot, node: EsTree): Ast =
      build(
        slot.name,
        slot.args,
        "ClassElementName Initializer?",
        List(
          Make(classElementName(_, node)),
          node.get("value").fold[Kid](Absent)(v => Make(initializer(_, v))),
        ),
      )

    private def classElementName(slot: Slot, node: EsTree): Ast =
      val key = node("key")
      if (!node.bool("computed") && key.tpe == "PrivateIdentifier")
        build(
          slot.name,
          slot.args,
          "PrivateIdentifier",
          List(Text(key.start, key.end)),
        )
      else
        build(
          slot.name,
          slot.args,
          "PropertyName",
          List(Make(propertyName(_, node))),
        )

    /** a method definition of a class or an object literal */
    private def method(slot: Slot, node: EsTree): Ast =
      val name = slot.name
      val args = slot.args
      val value = node("value")
      val params = value.list("params")
      val bodyKid = Make(functionBody(_, value("body")))
      node.str("kind") match
        case "get" =>
          build(
            name,
            args,
            "`get` ClassElementName `(` `)` `{` FunctionBody `}`",
            List(Make(classElementNameOf(_, node)), bodyKid),
          )
        case "set" =>
          build(
            name,
            args,
            "`set` ClassElementName `(` PropertySetParameterList `)` `{` FunctionBody `}`",
            List(
              Make(classElementNameOf(_, node)),
              Make(formalParameters(_, params)),
              bodyKid,
            ),
          )
        case _ =>
          (value.bool("async"), value.bool("generator")) match
            case (false, false) =>
              build(
                name,
                args,
                "ClassElementName `(` UniqueFormalParameters `)` `{` FunctionBody `}`",
                List(
                  Make(classElementNameOf(_, node)),
                  Make(formalParameters(_, params)),
                  bodyKid,
                ),
              )
            case (false, true) =>
              build(
                name,
                args,
                "GeneratorMethod",
                List(Make(generatorMethod(_, node, params, value))),
              )
            case (true, false) =>
              build(
                name,
                args,
                "AsyncMethod",
                List(Make(asyncMethod(_, node, params, value))),
              )
            case (true, true) =>
              build(
                name,
                args,
                "AsyncGeneratorMethod",
                List(Make(asyncGeneratorMethod(_, node, params, value))),
              )

    /** the name of a method, either a class element name or a property name */
    private def classElementNameOf(slot: Slot, node: EsTree): Ast =
      if (slot.name == "ClassElementName") classElementName(slot, node)
      else propertyName(slot, node)

    private def generatorMethod(
      slot: Slot,
      node: EsTree,
      params: List[EsTree],
      value: EsTree,
    ): Ast =
      build(
        slot.name,
        slot.args,
        "`*` ClassElementName `(` UniqueFormalParameters `)` `{` GeneratorBody `}`",
        List(
          Make(classElementNameOf(_, node)),
          Make(formalParameters(_, params)),
          Make(functionBody(_, value("body"))),
        ),
      )

    private def asyncMethod(
      slot: Slot,
      node: EsTree,
      params: List[EsTree],
      value: EsTree,
    ): Ast =
      build(
        slot.name,
        slot.args,
        "`async` ClassElementName `(` UniqueFormalParameters `)` `{` AsyncFunctionBody `}`",
        List(
          Make(classElementNameOf(_, node)),
          Make(formalParameters(_, params)),
          Make(functionBody(_, value("body"))),
        ),
      )

    private def asyncGeneratorMethod(
      slot: Slot,
      node: EsTree,
      params: List[EsTree],
      value: EsTree,
    ): Ast =
      build(
        slot.name,
        slot.args,
        "`async` `*` ClassElementName `(` UniqueFormalParameters `)` `{` AsyncGeneratorBody `}`",
        List(
          Make(classElementNameOf(_, node)),
          Make(formalParameters(_, params)),
          Make(functionBody(_, value("body"))),
        ),
      )

    // -------------------------------------------------------------------------
    // statements
    // -------------------------------------------------------------------------
    private def finallyClause(slot: Slot, node: EsTree): Ast =
      build(slot.name, slot.args, "`finally` Block", List(Tree(node)))

    private def caseBlock(slot: Slot, node: EsTree): Ast =
      val cases = node.list("cases")
      val (before, rest) = cases.span(_.has("test"))
      rest match
        case Nil =>
          build(
            slot.name,
            slot.args,
            "`{` CaseClauses? `}`",
            List(if (before.isEmpty) Absent else Make(caseClauses(_, before))),
          )
        case dflt :: after =>
          build(
            slot.name,
            slot.args,
            "`{` CaseClauses? DefaultClause CaseClauses? `}`",
            List(
              if (before.isEmpty) Absent else Make(caseClauses(_, before)),
              Make(inner => buildCore("DefaultClause", inner.args, dflt)),
              if (after.isEmpty) Absent else Make(caseClauses(_, after)),
            ),
          )

    private def caseClauses(slot: Slot, cases: List[EsTree]): Ast =
      leftList(slot.name, slot.args, cases)(
        item => ("CaseClause", List(Tree(item))),
        item => ("CaseClauses CaseClause", List(Tree(item))),
      )

    private def forStatement(
      name: String,
      args: List[Boolean],
      node: EsTree,
    ): Ast =
      val test = node.get("test").fold[Kid](Absent)(Tree(_))
      val update = node.get("update").fold[Kid](Absent)(Tree(_))
      val body = Tree(node("body"))
      node.get("init") match
        case None =>
          build(
            name,
            args,
            "`for` `(` Expression? `;` Expression? `;` Expression? `)` Statement",
            List(Absent, test, update, body),
          )
        case Some(init) if init.tpe != "VariableDeclaration" =>
          build(
            name,
            args,
            "`for` `(` Expression? `;` Expression? `;` Expression? `)` Statement",
            List(Tree(init), test, update, body),
          )
        case Some(init) if init.str("kind") == "var" =>
          build(
            name,
            args,
            "`for` `(` `var` VariableDeclarationList `;` Expression? `;` Expression? `)` Statement",
            List(Make(declarations(_, init)), test, update, body),
          )
        case Some(init) =>
          build(
            name,
            args,
            "`for` `(` LexicalDeclaration Expression? `;` Expression? `)` Statement",
            List(Tree(init), test, update, body),
          )

    private def forInOfStatement(
      name: String,
      args: List[Boolean],
      node: EsTree,
    ): Ast =
      val isIn = node.tpe == "ForInStatement"
      val keyword = if (isIn) "`in`" else "`of`"
      val rightNt = if (isIn) "Expression" else "AssignmentExpression"
      val await = if (node.bool("await")) "`await` " else ""
      val left = node("left")
      val body = Tree(node("body"))
      val right = Tree(node("right"))
      if (left.tpe != "VariableDeclaration")
        build(
          name,
          args,
          s"`for` $await`(` LeftHandSideExpression $keyword $rightNt `)` Statement",
          List(Tree(left), right, body),
        )
      else
        val binding = left.list("declarations").head("id")
        if (left.str("kind") == "var")
          build(
            name,
            args,
            s"`for` $await`(` `var` ForBinding $keyword $rightNt `)` Statement",
            List(Tree(binding), right, body),
          )
        else
          build(
            name,
            args,
            s"`for` $await`(` ForDeclaration $keyword $rightNt `)` Statement",
            List(
              Make(slot =>
                build(
                  slot.name,
                  slot.args,
                  "LetOrConst ForBinding",
                  List(
                    Make(letOrConst(_, left.str("kind"), left.start)),
                    Tree(binding),
                  ),
                ),
              ),
              right,
              body,
            ),
          )

    // -------------------------------------------------------------------------
    // modules
    // -------------------------------------------------------------------------
    private def importDeclaration(
      name: String,
      args: List[Boolean],
      node: EsTree,
    ): Ast =
      val specifiers = node.list("specifiers")
      val source = node("source")
      val withKid = attributeKid(node, source)
      // `import {} from 'a'` has no specifier either, so an empty import clause
      // is told apart by whether the module specifier follows `import` directly
      val bare = skipTrivia(node.start + "import".length) == source.start
      if (bare)
        build(
          name,
          args,
          "`import` ModuleSpecifier WithClause? `;`",
          List(Make(moduleSpecifier(_, source)), withKid),
        )
      else
        build(
          name,
          args,
          "`import` ImportClause FromClause WithClause? `;`",
          List(
            Make(importClause(_, specifiers)),
            Make(fromClause(_, source)),
            withKid,
          ),
        )

    /** the `with` clause of an import or export declaration
      *
      * `with {}` has no attribute of its own, so ESTree records it exactly like
      * a declaration without a clause; the source tells them apart.
      */
    private def attributeKid(node: EsTree, source: EsTree): Kid =
      val attributes =
        if (node.has("attributes")) node.items("attributes").flatten else Nil
      if (attributes.isEmpty && !nextIsWord(source.end, "with")) Absent
      else Make(withClause(_, attributes, node))

    private def moduleSpecifier(slot: Slot, node: EsTree): Ast =
      build(
        slot.name,
        slot.args,
        "StringLiteral",
        List(Text(node.start, node.end)),
      )

    private def fromClause(slot: Slot, node: EsTree): Ast =
      build(
        slot.name,
        slot.args,
        "`from` ModuleSpecifier",
        List(Make(moduleSpecifier(_, node))),
      )

    private def withClause(
      slot: Slot,
      attributes: List[EsTree],
      node: EsTree,
    ): Ast =
      if (attributes.isEmpty) build(slot.name, slot.args, "`with` `{` `}`", Nil)
      else
        build(
          slot.name,
          slot.args,
          "`with` `{` WithEntries `,`? `}`",
          List(Make(withEntries(_, attributes))),
        )

    private def withEntries(slot: Slot, attributes: List[EsTree]): Ast =
      attributes match
        case attribute :: Nil =>
          build(
            slot.name,
            slot.args,
            "AttributeKey `:` StringLiteral",
            List(
              Make(attributeKey(_, attribute("key"))),
              Text(attribute("value").start, attribute("value").end),
            ),
          )
        case attribute :: rest =>
          build(
            slot.name,
            slot.args,
            "AttributeKey `:` StringLiteral `,` WithEntries",
            List(
              Make(attributeKey(_, attribute("key"))),
              Text(attribute("value").start, attribute("value").end),
              Make(withEntries(_, rest)),
            ),
          )
        case Nil => error("empty import attributes")

    private def attributeKey(slot: Slot, node: EsTree): Ast =
      val sig =
        if (node.tpe == "Identifier") "IdentifierName" else "StringLiteral"
      build(slot.name, slot.args, sig, List(Text(node.start, node.end)))

    private def importClause(slot: Slot, specifiers: List[EsTree]): Ast =
      val name = slot.name
      val args = slot.args
      val default = specifiers.find(_.tpe == "ImportDefaultSpecifier")
      val namespace = specifiers.find(_.tpe == "ImportNamespaceSpecifier")
      val named = specifiers.filter(_.tpe == "ImportSpecifier")
      (default, namespace, named) match
        // `import a, {} from 'b'` has no named specifier, so an empty list of
        // named imports shows up only as the `,` that introduces it
        case (Some(default), None, Nil) if hasTrailingComma(default.end) =>
          build(
            name,
            args,
            "ImportedDefaultBinding `,` NamedImports",
            List(
              Make(importedDefaultBinding(_, default)),
              Make(namedImports(_, Nil)),
            ),
          )
        case (Some(default), None, Nil) =>
          build(
            name,
            args,
            "ImportedDefaultBinding",
            List(Make(importedDefaultBinding(_, default))),
          )
        case (None, Some(namespace), Nil) =>
          build(
            name,
            args,
            "NameSpaceImport",
            List(Make(nameSpaceImport(_, namespace))),
          )
        case (None, None, named) =>
          build(name, args, "NamedImports", List(Make(namedImports(_, named))))
        case (Some(default), Some(namespace), Nil) =>
          build(
            name,
            args,
            "ImportedDefaultBinding `,` NameSpaceImport",
            List(
              Make(importedDefaultBinding(_, default)),
              Make(nameSpaceImport(_, namespace)),
            ),
          )
        case (Some(default), None, named) =>
          build(
            name,
            args,
            "ImportedDefaultBinding `,` NamedImports",
            List(
              Make(importedDefaultBinding(_, default)),
              Make(namedImports(_, named)),
            ),
          )
        case _ => error("unsupported import clause")

    private def importedDefaultBinding(slot: Slot, node: EsTree): Ast =
      build(slot.name, slot.args, "ImportedBinding", List(Tree(node("local"))))

    private def nameSpaceImport(slot: Slot, node: EsTree): Ast =
      build(
        slot.name,
        slot.args,
        "`*` `as` ImportedBinding",
        List(Tree(node("local"))),
      )

    private def namedImports(slot: Slot, specifiers: List[EsTree]): Ast =
      if (specifiers.isEmpty) build(slot.name, slot.args, "`{` `}`", Nil)
      else
        val list = Make(inner =>
          leftList(inner.name, inner.args, specifiers)(
            item => ("ImportSpecifier", List(Make(importSpecifier(_, item)))),
            item =>
              (
                "ImportsList `,` ImportSpecifier",
                List(Make(importSpecifier(_, item))),
              ),
          ),
        )
        if (hasTrailingComma(specifiers.last.end))
          build(slot.name, slot.args, "`{` ImportsList `,` `}`", List(list))
        else build(slot.name, slot.args, "`{` ImportsList `}`", List(list))

    private def importSpecifier(slot: Slot, node: EsTree): Ast =
      val imported = node("imported")
      val local = node("local")
      if (imported.start == local.start)
        build(slot.name, slot.args, "ImportedBinding", List(Tree(local)))
      else
        build(
          slot.name,
          slot.args,
          "ModuleExportName `as` ImportedBinding",
          List(Make(moduleExportName(_, imported)), Tree(local)),
        )

    private def moduleExportName(slot: Slot, node: EsTree): Ast =
      val sig =
        if (node.tpe == "Identifier") "IdentifierName" else "StringLiteral"
      build(slot.name, slot.args, sig, List(Text(node.start, node.end)))

    private def exportDeclaration(
      name: String,
      args: List[Boolean],
      node: EsTree,
    ): Ast = node.tpe match
      case "ExportAllDeclaration" =>
        build(
          name,
          args,
          "`export` ExportFromClause FromClause WithClause? `;`",
          List(
            Make(slot =>
              node.get("exported") match
                case None => build(slot.name, slot.args, "`*`", Nil)
                case Some(exported) =>
                  build(
                    slot.name,
                    slot.args,
                    "`*` `as` ModuleExportName",
                    List(Make(moduleExportName(_, exported))),
                  ),
            ),
            Make(fromClause(_, node("source"))),
            attributeKid(node, node("source")),
          ),
        )
      case "ExportDefaultDeclaration" =>
        val declaration = node("declaration")
        declaration.tpe match
          case "FunctionDeclaration" =>
            build(
              name,
              args,
              "`export` `default` HoistableDeclaration",
              List(Tree(declaration)),
            )
          case "ClassDeclaration" =>
            build(
              name,
              args,
              "`export` `default` ClassDeclaration",
              List(Tree(declaration)),
            )
          case _ =>
            build(
              name,
              args,
              "`export` `default` AssignmentExpression `;`",
              List(Tree(declaration)),
            )
      case _ =>
        node.get("declaration") match
          case Some(declaration) =>
            if (
              declaration.tpe == "VariableDeclaration" &&
              declaration.str("kind") == "var"
            )
              build(
                name,
                args,
                "`export` VariableStatement",
                List(Tree(declaration)),
              )
            else
              build(name, args, "`export` Declaration", List(Tree(declaration)))
          case None =>
            val specifiers = node.list("specifiers")
            node.get("source") match
              case Some(source) =>
                build(
                  name,
                  args,
                  "`export` ExportFromClause FromClause WithClause? `;`",
                  List(
                    Make(slot =>
                      build(
                        slot.name,
                        slot.args,
                        "NamedExports",
                        List(Make(namedExports(_, specifiers))),
                      ),
                    ),
                    Make(fromClause(_, source)),
                    attributeKid(node, source),
                  ),
                )
              case None =>
                build(
                  name,
                  args,
                  "`export` NamedExports `;`",
                  List(Make(namedExports(_, specifiers))),
                )

    private def namedExports(slot: Slot, specifiers: List[EsTree]): Ast =
      if (specifiers.isEmpty) build(slot.name, slot.args, "`{` `}`", Nil)
      else
        val list = Make(inner =>
          leftList(inner.name, inner.args, specifiers)(
            item => ("ExportSpecifier", List(Make(exportSpecifier(_, item)))),
            item =>
              (
                "ExportsList `,` ExportSpecifier",
                List(Make(exportSpecifier(_, item))),
              ),
          ),
        )
        if (hasTrailingComma(specifiers.last.end))
          build(slot.name, slot.args, "`{` ExportsList `,` `}`", List(list))
        else build(slot.name, slot.args, "`{` ExportsList `}`", List(list))

    private def exportSpecifier(slot: Slot, node: EsTree): Ast =
      val local = node("local")
      val exported = node("exported")
      if (local.start == exported.start)
        build(
          slot.name,
          slot.args,
          "ModuleExportName",
          List(Make(moduleExportName(_, local))),
        )
      else
        build(
          slot.name,
          slot.args,
          "ModuleExportName `as` ModuleExportName",
          List(
            Make(moduleExportName(_, local)),
            Make(moduleExportName(_, exported)),
          ),
        )
  }
}
