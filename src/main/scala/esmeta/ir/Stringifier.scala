package esmeta.ir

import esmeta.LINE_SEP
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*

/** stringifier for IR */
case class Stringifier(detail: Boolean) {
  // ////////////////////////////////////////////////////////////////////////////
  // Syntax
  // ////////////////////////////////////////////////////////////////////////////
  import Appender.given

  // elements
  given elemRule: Rule[IRElem] = (app, elem) =>
    elem match {
      case elem: Program  => progRule(app, elem)
      case elem: Inst     => instRule(app, elem)
      case elem: Expr     => exprRule(app, elem)
      case elem: Ref      => refRule(app, elem)
      case elem: Ty       => tyRule(app, elem)
      case elem: Id       => idRule(app, elem)
      case elem: UOp      => uopRule(app, elem)
      case elem: BOp      => bopRule(app, elem)
      case elem: COp      => copRule(app, elem)
      case elem: State    => stateRule(app, elem)
      case elem: Context  => ctxtRule(app, elem)
      case elem: Cursor   => cursorRule(app, elem)
      case elem: Heap     => heapRule(app, elem)
      case elem: Obj      => objRule(app, elem)
      case elem: Value    => valRule(app, elem)
      case elem: RefValue => refValRule(app, elem)
    }

  // programs
  given progRule: Rule[Program] =
    (app, program) => program.insts.foldLeft(app)(_ :> _ >> LINE_SEP)

  // instructions
  given instRule: Rule[Inst] = (app, inst) =>
    inst match
      case IExpr(expr)         => app >> expr
      case ILet(id, expr)      => app >> "let " >> id >> " = " >> expr
      case IAssign(ref, expr)  => app >> ref >> " = " >> expr
      case IDelete(ref)        => app >> "delete " >> ref
      case IAppend(expr, list) => app >> "append " >> expr >> " -> " >> list
      case IPrepend(expr, list) =>
        app >> "prepend " >> expr >> " -> " >> list
      case IReturn(expr)       => app >> "return " >> expr
      case ithrow @ IThrow(id) => app >> "throw " >> id
      case IIf(cond, thenInst, elseInst) =>
        app >> "if " >> cond >> " "
        app >> thenInst >> " else "
        app >> elseInst
      case IWhile(cond, body) => app >> "while " >> cond >> " " >> body
      case ISeq(insts)        => app.wrapIterable(insts, detail)
      case IAssert(expr)      => app >> "assert " >> expr
      case IPrint(expr)       => app >> "print " >> expr
      case iapp @ IApp(id, fexpr, args) =>
        given Rule[Iterable[Expr]] = iterableRule[Expr](sep = " ")
        app >> "app " >> id >> " = (" >> fexpr
        if (!args.isEmpty) app >> " " >> args
        app >> ")"
      case IWithCont(id, params, inst) =>
        given Rule[Inst] = instDetailRule
        given Rule[Iterable[Id]] = iterableRule[Id]("(", ", ", ")")
        app >> "withcont " >> id >> " " >> params >> " = " >> inst

  // instrctions without detail information
  lazy val instDetailRule: Rule[Inst] = (app, inst) =>
    if (detail) app >> inst else app >> "..."

  // expressions
  given exprRule: Rule[Expr] = (app, expr) =>
    expr match
      case ENum(n)      => app >> s"$n"
      case EINum(n)     => app >> s"${n}i"
      case EBigINum(b)  => app >> s"${b}n"
      case EStr(str)    => app >> "\"" >> normStr(str) >> "\""
      case EBool(b)     => app >> s"$b"
      case EUndef       => app >> "undefined"
      case ENull        => app >> "null"
      case EAbsent      => app >> "absent"
      case EConst(name) => app >> "~" >> name >> "~"
      case EClo(params, captured, body) =>
        given Rule[Iterable[Id]] = iterableRule[Id](sep = ", ")
        given Rule[Inst] = instDetailRule
        app >> "(clo (" >> params >> ")[" >> captured >> "] => " >> body >> ")"
      case ECont(params, body) =>
        given Rule[Iterable[Id]] = iterableRule[Id]("(", ", ", ")")
        given Rule[Inst] = instDetailRule
        app >> "(cont " >> params >> " [=>] " >> body >> ")"
      case EComp(ty, value, target) =>
        app >> "(comp[" >> ty >> "] " >> value >> " => " >> target >> ")"
      case EMap(ty, props) =>
        given Rule[Iterable[(Expr, Expr)]] =
          iterableRule[(Expr, Expr)]("(", ", ", ")")
        app >> "(new " >> ty >> props >> ")"
      case EList(exprs) =>
        given Rule[Iterable[Expr]] = iterableRule[Expr]("[", ", ", "]")
        app >> "(new " >> exprs >> ")"
      case ESymbol(desc)   => app >> "(new '" >> desc >> ")"
      case EPop(list, idx) => app >> "(pop " >> list >> " " >> idx >> ")"
      case ERef(ref)       => app >> ref
      case EUOp(uop, expr) => app >> "(" >> uop >> " " >> expr >> ")"
      case EBOp(bop, left, right) =>
        app >> "(" >> bop >> " " >> left >> " " >> right >> ")"
      case ETypeOf(expr)       => app >> "(typeof " >> expr >> ")"
      case EIsCompletion(expr) => app >> "(is-completion " >> expr >> ")"
      case EIsInstanceOf(base, name) =>
        app >> "(is-instance-of " >> base >> " " >> name >> ")"
      case EGetElems(base, name) =>
        app >> "(get-elems " >> base >> " " >> name >> ")"
      case EGetSyntax(base) => app >> "(get-syntax " >> base >> ")"
      case EParseSyntax(code, rule, parserParams) =>
        given Rule[Iterable[Boolean]] =
          iterableRule[Boolean](sep = " ")
        app >> "(parse-syntax " >> code >> " " >> rule
        if (!parserParams.isEmpty) app >> " " >> parserParams
        app >> ")"
      case EConvert(expr, cop, list) =>
        given Rule[Iterable[Expr]] = iterableRule[Expr](sep = " ")
        app >> "(convert " >> expr >> " " >> cop
        for (l <- list) app >> " " >> l
        app >> ")"
      case EContains(list, elem) =>
        app >> "(contains " >> list >> " " >> elem >> ")"
      case EReturnIfAbrupt(expr, check) =>
        app >> "[" >> (if (check) "?" else "!") >> " " >> expr >> "]"
      case ECopy(obj) => app >> "(copy-obj " >> obj >> ")"
      case EKeys(obj, intSorted) =>
        app >> "(map-keys " >> obj
        if (intSorted) app >> " [int-sorted]"
        app >> ")"
      case ENotSupported(msg) => app >> "??? \"" >> normStr(msg) >> "\""

  // ref
  given refRule: Rule[Ref] = (app, ref) =>
    ref match
      case RefId(id) => app >> id
      case RefProp(ref, EStr(str)) if "[_a-zA-Z0-9]+".r.matches(str) =>
        app >> ref >> "." >> str
      case RefProp(ref, expr) => app >> ref >> "[" >> expr >> "]"

  // types
  given tyRule: Rule[Ty] = (app, ty) => app >> ty.name

  // identifiers
  given idRule: Rule[Id] = (app, id) => app >> id.name

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

  // convert operators
  given copRule: Rule[COp] = (app, cop) =>
    import COp.*
    app >> (cop match
      case StrToNum    => "str2num"
      case StrToBigInt => "str2bigint"
      case NumToStr    => "num2str"
      case NumToInt    => "num2int"
      case NumToBigInt => "num2bigint"
      case BigIntToNum => "bigint2num"
    )

  // ////////////////////////////////////////////////////////////////////////////
  // States
  // ////////////////////////////////////////////////////////////////////////////
  // states
  given stateRule: Rule[State] = (app, st) =>
    app.wrap("{", "}") {
      val State(_, context, ctxtStack, globals, heap) = st
      app :> "context: " >> context
      given Rule[Iterable[String]] =
        iterableRule[String]("[", ", ", "]")
      app :> "context-stack: " >> ctxtStack.map(_.name)
      app :> "globals: "
      app.wrapIterable(globals, detail)
      app :> "heap: " >> heap
    }

  // contexts
  given ctxtRule: Rule[Context] = (app, context) =>
    app.wrap("{", "}") {
      val Context(cursorOpt, retId, name, locals) = context
      app :> "name: " >> name
      app :> "return: " >> retId
      app :> "cursor: "
      cursorOpt match {
        case None         => app >> "[EMPTY]"
        case Some(cursor) => app >> cursor
      }
      app :> "local-vars: "
      app.wrapIterable(locals)
    }

  // cursors
  given cursorRule: Rule[Cursor] = (app, cursor) =>
    cursor match
      case InstCursor(cur, rest) =>
        app >> cur >> " [# rest: " >> rest.size >> "]"

  // heaps
  given heapRule: Rule[Heap] = (app, heap) =>
    val Heap(map, size) = heap
    app >> s"(SIZE = " >> size.toString >> "): "
    app.wrapIterable(map)

  // objects
  given objRule: Rule[Obj] = (app, obj) =>
    obj match
      case IRSymbol(desc) => app >> "(Symbol " >> desc >> ")"
      case map @ IRMap(ty, _, _) => {
        app >> "(TYPE = " >> ty >> ") "
        app.wrapIterable(map.props)
      }
      case IRList(values) => {
        given Rule[Iterable[Value]] =
          iterableRule[Value]("[", ", ", "]")
        app >> values.toList
      }
      case IRNotSupported(tyname, msg) =>
        app >> "(NotSupported \"" >> tyname >> "\" \"" >> msg >> "\")"

  // values for IRMap
  // TODO support detailed string with the creation time
  given irMapValueRule: Rule[IRMapValue] = (app, value) => app >> value.value

  // values
  given valRule: Rule[Value] = (app, v) =>
    v match
      case Num(double)       => app >> double.toString
      case INum(long)        => app >> long.toString >> "i"
      case BigINum(bigint)   => app >> bigint.toString >> "n"
      case Str(str)          => app >> "\"" >> normStr(str) >> "\""
      case Bool(bool)        => app >> bool.toString
      case Undef             => app >> "undefined"
      case Null              => app >> "null"
      case Absent            => app >> "absent"
      case Const(name)       => app >> "~" >> name >> "~"
      case NamedAddr(name)   => app >> "#" >> name
      case DynamicAddr(long) => app >> "#" >> long.toString
      case clo: Clo          => app >> clo
      case cont: Cont        => app >> cont
      case comp: CompValue   => app >> comp

  // completions
  given compValRule: Rule[CompValue] = (app, c) =>
    c match
      case CompValue(CONST_NORMAL, value, CONST_EMPTY) =>
        app >> "N(" >> value >> ")"
      case CompValue(ty, value, target) =>
        app >> "Completion[" >> ty >> "]" >> "(" >> value
        app >> " => " >> target
        app >> ")"

  // closures
  given cloRule: Rule[Clo] = (app, clo) =>
    given Rule[Iterable[Id]] = iterableRule[Id]("(", ", ", ")")
    given ps: Rule[Iterable[(Id, Value)]] =
      iterableRule[(Id, Value)]("[", ", ", "]")
    val Clo(_, params, locals, _) = clo
    // TODO handle detail option
    app >> clo.ctxtName >> ":closure" >> params >> locals.toList >> " => ..."

  // continuations
  given contRule: Rule[Cont] = (app, cont) =>
    given Rule[Iterable[Id]] = iterableRule[Id]("(", ", ", ")")
    val Cont(params, context, ctxtStack) = cont
    // TODO handle detail option
    app >> context.name >> params >> " [=>] ..."

  // reference values
  given refValRule: Rule[RefValue] = (app, refV) =>
    refV match
      case RefValueId(id)           => app >> id
      case RefValueProp(base, prop) => app >> base >> "[" >> prop >> "]"
}
