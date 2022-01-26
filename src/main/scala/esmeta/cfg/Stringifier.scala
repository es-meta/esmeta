package esmeta.cfg

import esmeta.LINE_SEP
import esmeta.cfg.Utils.*
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*
import scala.collection.mutable.ListBuffer

/** stringifier for CFG */
case class Stringifier(detail: Boolean, location: Boolean) {
  // elements
  given elemRule: Rule[CFGElem] = (app, elem) =>
    elem match {
      case elem: CFG         => cfgRule(app, elem)
      case elem: Func        => funcRule(app, elem)
      case elem: Func.Kind   => funcKindRule(app, elem)
      case elem: Param       => paramRule(app, elem)
      case elem: Param.Kind  => paramKindRule(app, elem)
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
    val CFG(_, funcs) = cfg
    given Rule[Iterable[Func]] = iterableRule(sep = LINE_SEP)
    given Ordering[Func] = Ordering.by(_.id)
    app >> cfg.funcs.sorted

  // functions
  given funcRule: Rule[Func] = (app, func) =>
    val Func(id, main, kind, name, params, _) = func
    given Rule[Iterable[Param]] = iterableRule("(", ", ", ")")
    app >> id >> ": " >> (if (main) "@main " else "") >> kind
    app >> name >> params >> " "
    app.wrap {
      given Ordering[Node] = Ordering.by(_.id)
      for (node <- func.nodes.toList.sorted) app :> node
    }

  // function kinds
  given funcKindRule: Rule[Func.Kind] = (app, kind) =>
    import Func.Kind.*
    app >> (kind match {
      case AbsOp        => ""
      case NumMeth      => "<NUM>:"
      case SynDirOp     => "<SYNTAX>:"
      case ConcMeth     => "<CONC>:"
      case InternalMeth => "<INTERNAL>:"
      case Builtin      => "<BUILTIN>:"
      case Clo          => "<CLO>:"
      case Cont         => "<CONT>:"
    })

  // function parameters
  given paramRule: Rule[Param] = (app, param) =>
    val Param(name, kind, ty) = param
    app >> name >> kind >> ": " >> ty

  // function parameter kinds
  given paramKindRule: Rule[Param.Kind] = (app, kind) =>
    import Param.Kind.*
    app >> (kind match {
      case Normal   => ""
      case Optional => "?"
    })

  // nodes
  given nodeRule: Rule[Node] = withLoc { (app, node) =>
    app >> node.id >> ": "
    node match
      case Block(_, insts, next) =>
        app.wrap(for (inst <- insts) app :> inst)
        next.map(x => app >> " -> " >> x.id)
      case Call(_, lhs, fexpr, args, next) =>
        given Rule[Iterable[Expr]] = iterableRule[Expr]("(", ", ", ")")
        app >> "call " >> lhs >> " = " >> fexpr >> args
        next.map(x => app >> " -> " >> x.id)
      case Branch(_, kind, cond, thenNode, elseNode) =>
        app >> kind >> " " >> cond
        thenNode.map(x => app >> " then " >> x.id)
        elseNode.map(x => app >> " else " >> x.id)
    app
  }

  // branch kinds
  given branchKindRule: Rule[Branch.Kind] = (app, kind) =>
    import Branch.Kind.*
    app >> (kind match {
      case If        => "if"
      case Loop(str) => s"loop[$str]"
    })

  // instructions
  given instRule: Rule[Inst] = withLoc { (app, inst) =>
    inst match
      case IExpr(expr) =>
        app >> expr
      case ILet(lhs, expr) =>
        app >> "let " >> lhs >> " = " >> expr
      case IAssign(ref, expr) =>
        app >> ref >> " = " >> expr
      case IDelete(ref) =>
        app >> "delete " >> ref
      case IPush(from, to, front) =>
        app >> "push "
        if (front) app >> from >> " > " >> to
        else app >> to >> " < " >> from
      case IReturn(expr) =>
        app >> "return " >> expr
      case IAssert(expr) =>
        app >> "assert " >> expr
      case IPrint(expr) =>
        app >> "print " >> expr
  }

  // expressions
  given exprRule: Rule[Expr] = withLoc { (app, expr) =>
    expr match
      case EComp(tyExpr, valExpr, tgtExpr) =>
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
  lazy val astExprRule: Rule[AstExpr] = (app, ast) =>
    ast match {
      case ESyntactic(name, args, rhsIdx, bits, children) =>
        app >> "|" >> name >> "|"
        given Rule[Boolean] = (app, bool) => app >> (if (bool) "T" else "F")
        given Rule[List[Boolean]] = iterableRule("[", "", "]")
        if (!args.isEmpty) app >> args
        app >> "<" >> rhsIdx >> ", " >> bits >> ">"
        given el: Rule[List[Expr]] = iterableRule("(", ", ", ")")
        if (!children.isEmpty) app >> children
        app
      case ELexical(name, expr) =>
        app >> "|" >> name >> "|(" >> expr >> ")"
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

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  // append locations
  private def withLoc[T <: Locational](rule: Rule[T]): Rule[T] = (app, elem) =>
    given Rule[Option[Loc]] = (app, locOpt) =>
      locOpt.fold(app)(app >> " @ " >> _.toString)
    rule(app, elem)
    if (location) app >> elem.loc else app
}
