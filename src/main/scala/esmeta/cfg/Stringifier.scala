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
      case elem: CFG   => cfgRule(app, elem)
      case elem: Func  => funcRule(app, elem)
      case elem: Param => paramRule(app, elem)
      case elem: Node  => nodeRule(app, elem)
      case elem: Inst  => instRule(app, elem)
      case elem: Expr  => exprRule(app, elem)
      case elem: UOp   => uopRule(app, elem)
      case elem: BOp   => bopRule(app, elem)
      case elem: COp   => copRule(app, elem)
      case elem: Ref   => refRule(app, elem)
      case elem: Type  => tyRule(app, elem)
    }

  // control-flow graphs (CFGs)
  given cfgRule: Rule[CFG] = (app, cfg) =>
    given Ordering[Func] = Ordering.by(_.id)
    for (func <- cfg.funcs.toList.sorted) app >> func >> LINE_SEP >> LINE_SEP
    app

  // functions
  given funcRule: Rule[Func] = (app, func) =>
    val Func(_, kind, params, _, _, nodes) = func
    given Rule[Iterable[Param]] = iterableRule("(", ", ", ")")
    app >> func.simpleString >> " " >> kind >> params >> " "
    given Ordering[Node] = Ordering.by(_.id)
    app.wrap(for (node <- nodes.toList.sorted) app :> node)

  // function kinds
  given funcKindRule: Rule[Func.Kind] = (app, kind) =>
    import Func.Kind.*
    app >> (kind match {
      case AbsOp       => "abs-op"
      case NumMeth     => "num-meth"
      case SynDirOp    => "syn-dir-op"
      case ConcMeth    => "conc-meth"
      case BuiltinMeth => "builtin-meth"
      case Clo         => "clo"
      case Cont        => "cont"
    })

  // function parameters
  given paramRule: Rule[Param] = (app, param) =>
    val Param(name, ty) = param
    app >> name >> ": " >> ty

  // nodes
  given nodeRule: Rule[Node] = (app, node) =>
    app >> node.simpleString
    node match {
      case Entry(_, _, next) =>
        app >> " -> " >> next.id
      case Exit(_, _) =>
        app
      case Block(_, _, insts, next) =>
        app >> " -> " >> next.id >> " "
        app.wrap(for (inst <- insts) app :> inst)
      case Branch(_, _, kind, cond, thenNode, elseNode) =>
        app >> " -> " >> kind >> "(" >> cond >> ")" >> thenNode.id
        app >> " else " >> elseNode.id
      case Call(_, _, lhs, fexpr, args) =>
        given Rule[Iterable[Expr]] = iterableRule[Expr]("(", ", ", ")")
        app >> " " >> lhs >> " = " >> fexpr >> args
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
      case ILet(_, lhs, expr) =>
        app >> "let " >> lhs >> " = " >> expr
      case IAssign(_, ref, expr) =>
        app >> ref >> " = " >> expr
      case IDelete(_, ref) =>
        app >> "delete " >> ref
      case IPush(_, from, to, front) =>
        app >> "push"
        if (front) app >> from >> " > " >> to
        else app >> to >> " < " >> from
      case IReturn(_, expr) =>
        app >> "return " >> expr
      case IAssert(_, expr) =>
        app >> "assert " >> expr
      case IPrint(_, expr) =>
        app >> "print " >> expr
    }
    app >> " @ " >> inst.loc

  // locations
  given locRule: Rule[Loc] = (app, loc) =>
    val Loc(step, line, start, end) = loc
    given Rule[Iterable[Int]] = iterableRule("[", ">", "]")
    app >> step >> " " >> line >> ":" >> start >> "-" >> end

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
      case expr: AllocExpr =>
        allocExprRule(app, expr)
      case expr: Literal =>
        literalRule(app, expr)
    }

  // allocation expressions
  given allocExprRule: Rule[AllocExpr] = (app, expr) =>
    expr match {
      case EMap(tname, props, asite) =>
        given Rule[Iterable[(Expr, Expr)]] = iterableRule("(", ", ", ")")
        app >> "(new " >> tname >> props >> ")[#" >> asite >> "]"
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
  given literalRule: Rule[Literal] = (app, lit) =>
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
      case EClo(fid, captured) =>
        given Rule[Iterable[Local]] = iterableRule("(", ", ", ")")
        app >> "clo[" >> fid >> "]"
        if (captured.isEmpty) app else app >> captured
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
      case ToStr(radixOpt) =>
        app >> "[str"
        radixOpt.map(app >> " " >> _)
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
      case Local(name)  => app >> name
      case Temp(id)     => app >> "%" >> id
    }

  // types
  given tyRule: Rule[Type] = (app, ty) => app >> ty.name
}
