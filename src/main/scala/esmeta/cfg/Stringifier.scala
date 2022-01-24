package esmeta.cfg

import esmeta.LINE_SEP
import esmeta.cfg.Utils.*
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*

/** stringifier for CFG */
case class Stringifier(detail: Boolean) {
  import Appender.given

  // elements
  given elemRule: Rule[CFGElem] = (app, elem) =>
    elem match {
      case elem: CFG         => cfgRule(app, elem)
      case elem: Func        => funcRule(app, elem)
      case elem: Func.Kind   => funcKindRule(app, elem)
      case elem: Param       => paramRule(app, elem)
      case elem: Node        => nodeRule(app, elem)
      case elem: Branch.Kind => branchKindRule(app, elem)
      case elem: Inst        => instRule(app, elem)
      case elem: Expr        => exprRule(app, elem)
      case elem: UOp         => uopRule(app, elem)
      case elem: BOp         => bopRule(app, elem)
      case elem: COp         => copRule(app, elem)
      case elem: Ref         => refRule(app, elem)
      case elem: Type        => tyRule(app, elem)
    }

  // control-flow graphs (CFGs)
  given cfgRule: Rule[CFG] = (app, cfg) =>
    val CFG(main, funcs) = cfg
    given Rule[Iterable[(Func, Boolean)]] = iterableRule(sep = LINE_SEP * 2)
    given Ordering[Func] = Ordering.by(_.id)
    app >> cfg.funcs.sorted.map(f => (f, f.id == cfg.main))

  // functions
  given funcWithMainRule: Rule[(Func, Boolean)] = (app, pair) =>
    val (func, isMain) = pair
    app >> (if (isMain) "@main " else "") >> func
  given funcRule: Rule[Func] = (app, func) =>
    val Func(_, kind, name, params, entry, nodes, exit) = func
    given Rule[Iterable[Param]] = iterableRule("(", ", ", ")")
    app >> func.id >> ": " >> kind
    app >> name >> params >> " "
    app.wrap {
      given Ordering[Node] = Ordering.by(_.id)
      app :> entry
      for (node <- nodes.sorted) app :> node
      app :> exit
    }

  // function kinds
  given funcKindRule: Rule[Func.Kind] = (app, kind) =>
    import Func.Kind.*
    app >> (kind match {
      case AbsOp    => ""
      case NumMeth  => "<NUM>:"
      case SynDirOp => "<SYNTAX>:"
      case ConcMeth => "<CONC>:"
      case Builtin  => "<BUILTIN>:"
      case Clo      => "<CLO>:"
      case Cont     => "<CONT>:"
    })

  // function parameters
  given paramRule: Rule[Param] = (app, param) =>
    val Param(name, ty) = param
    app >> name >> ": " >> ty

  // nodes
  given nodeRule: Rule[Node] = (app, node) =>
    app >> node.id >> ": "
    node match {
      case Entry(_, next) =>
        app >> "<entry> -> " >> next
      case Exit(_) =>
        app >> "<exit>"
      case Linear(_, Vector(inst), next) =>
        app >> inst >> " -> " >> next
      case Linear(_, insts, next) =>
        app.wrap(for (inst <- insts) app :> inst) >> " -> " >> next
      case Branch(_, kind, cond, loc, thenNode, elseNode) =>
        app >> "<branch> " >> kind >> "(" >> cond >> ")" >> loc
        app >> " -t> " >> thenNode >> " -f> " >> elseNode
      case Call(_, lhs, fexpr, args, loc, next) =>
        given Rule[Iterable[Expr]] = iterableRule[Expr]("(", ", ", ")")
        app >> "<call> " >> lhs >> " = "
        app >> fexpr >> args >> loc >> " -> " >> next
    }

  // branch kinds
  given branchKindRule: Rule[Branch.Kind] = (app, kind) =>
    import Branch.Kind.*
    app >> (kind match {
      case If      => "if"
      case While   => "while"
      case Foreach => "foreach"
    })

  // instructions
  given instRule: Rule[Inst] = (app, inst) =>
    inst match {
      case ILet(lhs, expr, _) =>
        app >> "let " >> lhs >> " = " >> expr
      case IAssign(ref, expr, _) =>
        app >> ref >> " = " >> expr
      case IDelete(ref, _) =>
        app >> "delete " >> ref
      case IPush(from, to, front, _) =>
        app >> "push "
        if (front) app >> from >> " > " >> to
        else app >> to >> " < " >> from
      case IReturn(expr, _) =>
        app >> "return " >> expr
      case IAssert(expr, _) =>
        app >> "assert " >> expr
      case IPrint(expr, _) =>
        app >> "print " >> expr
    }
    app >> inst.loc

  // locations
  given locRule: Rule[Option[Loc]] = (app, loc) =>
    loc.fold(app)(loc => app >> " @ " >> loc.toString)

  // expressions
  given exprRule: Rule[Expr] = (app, expr) =>
    expr match {
      case EComp(tyExpr, tgtExpr, valExpr) =>
        app >> "comp[" >> tyExpr >> "/" >> tgtExpr >> "](" >> valExpr >> ")"
      case EIsCompletion(expr) =>
        app >> "(comp? " >> expr >> ")"
      case EReturnIfAbrupt(expr, check) =>
        app >> "[" >> (if (check) "?" else "!") >> " " >> expr >> "]"
      case EPop(list, front) =>
        app >> "(pop " >> (if (front) "<" else ">") >> " " >> list >> ")"
      case EYet(msg) =>
        app >> "(yet \"" >> normStr(msg) >> "\")"
      case EContains(list, elem) =>
        app >> "(contains " >> list >> " " >> elem >> ")"
      case ERef(ref) =>
        app >> ref
      case EUnary(uop, expr) =>
        app >> "(" >> uop >> " " >> expr >> ")"
      case EBinary(bop, left, right) =>
        app >> "(" >> bop >> " " >> left >> " " >> right >> ")"
      case EConvert(cop, expr) =>
        app >> "(" >> cop >> " " >> expr >> ")"
      case ETypeOf(base) =>
        app >> "(typeof " >> base >> ")"
      case ETypeCheck(expr, ty) =>
        app >> "(? " >> expr >> ": " >> ty >> ")"
      case EClo(fid, captured) =>
        given Rule[Iterable[Name]] = iterableRule("(", ", ", ")")
        app >> "clo[" >> fid >> "]"
        if (captured.isEmpty) app else app >> captured
      case ECont(fid) =>
        app >> "cont[" >> fid >> "]"
      case expr: AstExpr =>
        astExprRule(app, expr)
      case expr: AllocExpr =>
        allocExprRule(app, expr)
      case expr: Literal =>
        literalRule(app, expr)
    }

  // abstract syntax tree (AST) expressions
  lazy val astExprRule: Rule[AstExpr] = (app, ast) => {
    val AstExpr(name, args, rhsIdx, bits, children) = ast
    app >> "|" >> name >> "|"
    given Rule[Boolean] = (app, bool) => app >> (if (bool) "T" else "F")
    given Rule[List[Boolean]] = iterableRule("[", "", "]")
    if (!args.isEmpty) app >> args
    app >> "<" >> rhsIdx >> ", " >> bits >> ">"
    given el: Rule[List[Expr]] = iterableRule("(", ", ", ")")
    if (!children.isEmpty) app >> children
    app
  }

  // allocation expressions
  lazy val allocExprRule: Rule[AllocExpr] = (app, expr) =>
    expr match {
      case EMap(tname, fields, asite) =>
        given Rule[Iterable[(Expr, Expr)]] = iterableRule("(", ", ", ")")
        app >> "(new " >> tname >> fields >> ")[#" >> asite >> "]"
      case EList(exprs, asite) =>
        given Rule[Iterable[Expr]] = iterableRule("[", ", ", "]")
        app >> "(new " >> exprs >> ")[#" >> asite >> "]"
      case ESymbol(desc, asite) =>
        app >> "(new '" >> desc >> ")[#" >> asite >> "]"
      case ECopy(obj, asite) =>
        app >> "(copy " >> obj >> ")[#" >> asite >> "]"
      case EKeys(map, intSorted, asite) =>
        app >> "(keys" >> (if (intSorted) "-int" else "") >> " "
        app >> map >> ")[#" >> asite >> "]"
    }

  // literals
  lazy val literalRule: Rule[Literal] = (app, lit) =>
    lit match {
      case EMathVal(n)                      => app >> n
      case ENumber(Double.PositiveInfinity) => app >> "+INF"
      case ENumber(Double.NegativeInfinity) => app >> "-INF"
      case ENumber(n) if n.isNaN            => app >> "NaN"
      case ENumber(n)                       => app >> n >> "f"
      case EBigInt(n)                       => app >> n >> "n"
      case EStr(str)    => app >> "\"" >> normStr(str) >> "\""
      case EBool(b)     => app >> b
      case EUndef       => app >> "undefined"
      case ENull        => app >> "null"
      case EAbsent      => app >> "absent"
      case EConst(name) => app >> "~" >> name >> "~"
    }

  // unary operators
  given uopRule: Rule[UOp] = (app, uop) =>
    import UOp.*
    app >> (uop match {
      case Neg  => "-"
      case Not  => "!"
      case BNot => "~"
    })

  // binary operators
  given bopRule: Rule[BOp] = (app, bop) =>
    import BOp.*
    app >> (bop match
      case Plus    => "+"
      case Sub     => "-"
      case Mul     => "*"
      case Pow     => "**"
      case Div     => "/"
      case UMod    => "%%"
      case Mod     => "%"
      case Eq      => "="
      case Equal   => "=="
      case And     => "&&"
      case Or      => "||"
      case Xor     => "^^"
      case BAnd    => "&"
      case BOr     => "|"
      case BXOr    => "^"
      case LShift  => "<<"
      case Lt      => "<"
      case URShift => ">>>"
      case SRShift => ">>"
    )

  // conversion operators
  given copRule: Rule[COp] = (app, cop) =>
    import COp.*
    cop match {
      case ToBigInt => app >> "[bigint]"
      case ToNumber => app >> "[number]"
      case ToMath   => app >> "[math]"
      case ToStr(radix) =>
        app >> "[str"
        radix.map(app >> " " >> _)
        app >> "]"
    }

  // references
  lazy val inlineProp = "([_a-zA-Z][_a-zA-Z0-9]*)".r
  given refRule: Rule[Ref] = (app, ref) =>
    ref match {
      case Prop(ref, EStr(inlineProp(str))) => app >> ref >> "." >> str
      case Prop(ref, expr)                  => app >> ref >> "[" >> expr >> "]"
      case id: Id                           => idRule(app, id)
    }

  // identifiers
  given idRule: Rule[Id] = (app, id) =>
    id match {
      case Global(name) => app >> "@" >> name
      case Name(name)   => app >> name
      case Temp(id)     => app >> "%" >> id
    }

  // types
  given tyRule: Rule[Type] = (app, ty) => app >> ty.name
}
