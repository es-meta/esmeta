package esmeta.analyzer

import esmeta.DEBUG
import esmeta.analyzer.domain.*
import esmeta.cfg.*
import esmeta.error.*
import esmeta.interp.*
import esmeta.ir.{Func => IRFunc, *}
import esmeta.js.Ast
import esmeta.js.util.ESValueParser
import scala.annotation.tailrec

// abstract transfer function
case class AbsTransfer(sem: AbsSemantics) {
  type AbsRet = sem.AbsRet.Elem
  type AbsState = sem.AbsState.Elem
  val AbsState: sem.AbsState.type = sem.AbsState
  val AbsRet: sem.AbsRet.type = sem.AbsRet

  // loading monads
  import AbsState.monad._
//
// // loading operators in abstract domains
//   import AbsStr._
//
// // math value to numeric
//   import NumericConverter._
//
  // transfer function for control points
  def apply(cp: ControlPoint): Unit = cp match
    case (np: NodePoint[_]) => this(np)
    case (rp: ReturnPoint)  => this(rp)

// // transfer function for node points
//   def apply[T <: Node](np: NodePoint[T]): Unit = {
//     val st = sem(np)
//     val NodePoint(node, view) = np
//     val helper = new Helper(np)
//
//     import helper._
//     node match {
//       case (entry: Entry) =>
//         sem += getNextNp(np, cfg.nextOf(entry)) -> st
//       case (exit: Exit) =>
//         doReturn(AbsValue.undef)(st)
//       case (normal: Normal) =>
//         val newSt = transfer(normal.inst)(st)
//         sem += getNextNp(np, cfg.nextOf(normal)) -> newSt
//       case (call: Call) =>
//         val newSt = transfer(call)(st)
//         sem += getNextNp(np, cfg.nextOf(call)) -> newSt
//       case arrow @ Arrow(_, inst, fid) =>
//         val newSt = transfer(arrow, np)(st)
//         sem += getNextNp(np, cfg.nextOf(arrow)) -> newSt
//       case branch @ Branch(_, inst) =>
//         (for {
//           v <- escape(transfer(inst.cond))
//           b = v.bool
//           st <- get
//         } yield {
//           val (thenNode, elseNode) = cfg.branchOf(branch)
//           if (b contains T) sem += getNextNp(np, thenNode) -> st
//           if (b contains F) sem += getNextNp(np, elseNode, true) -> st
//         })(st)
//       case (cont: LoopCont) =>
//         sem += getNextNp(np, cfg.nextOf(cont)) -> st
//     }
//   }

  // get next node points
  def getNextNp(
    fromCp: NodePoint[Node],
    to: Node,
    loopOut: Boolean = false,
  ): NodePoint[Node] =
    val NodePoint(func, from, view) = fromCp
    // val toView = (from, to) match
    //   case (_: LoopCont, _: Loop)  => sem.loopNext(view)
    //   case (_, loop: Loop)         => sem.loopEnter(view, loop)
    //   case (_: Loop, _) if loopOut => sem.loopExit(view)
    //   case _                       => view
    NodePoint(func, to, ???)

// // transfer function for return points
//   def apply(rp: ReturnPoint): Unit = {
//     var ret @ AbsRet(value, st) = sem(rp)
//
//     // proper type handle
//     Interp.setTypeMap
//       .get(rp.func.name)
//       .map(ty => {
//         if (!value.loc.isBottom) st = st.setType(value.loc, ty)
//       })
//
//     // debugging message
//     if (DEBUG) println(s"<RETURN> $ret")
//
//     // return wrapped values
//     for (np @ NodePoint(call, view) <- sem.getRetEdges(rp)) {
//       val callerSt = sem.callInfo(np)
//       val nextNode = cfg.nextOf(call)
//       val nextNp = NodePoint(
//         nextNode,
//         nextNode match {
//           case loop: Loop => sem.loopEnter(view, loop)
//           case _          => view
//         },
//       )
//
//       val newSt = st.doReturn(
//         callerSt,
//         call.inst.id -> value.wrapCompletion,
//       )
//
//       sem += nextNp -> newSt
//     }
//   }

  // transfer function for expressions
  def apply(cp: ControlPoint, expr: Expr): AbsValue = {
    val st = sem.getState(cp)
    val helper = new Helper(cp)
    helper.transfer(expr)(st)._1
  }

  // internal transfer function with a specific view
  private class Helper(val cp: ControlPoint) {
    lazy val func = cp.func
    lazy val view = cp.view
    lazy val rp = ReturnPoint(func, view)

    // transfer function for normal instructions
    def transfer(inst: NormalInst): Updater = inst match {
      case IExpr(expr) =>
        for {
          v <- transfer(expr)
        } yield v
      case ILet(id, expr) =>
        for {
          v <- transfer(expr)
          _ <- modify(_.defineLocal(id -> v))
        } yield ()
      case IAssign(ref, expr) =>
        for {
          rv <- transfer(ref)
          v <- transfer(expr)
          _ <- modify(_.update(rv, v))
        } yield ()
      case IDelete(ref) =>
        for {
          rv <- transfer(ref)
          _ <- modify(_.delete(rv))
        } yield ()
      case IPush(expr, list, front) =>
        for {
          l <- transfer(list)
          v <- transfer(expr)
          _ <- modify((st) =>
            if (front) st.prepend(l.loc, v) else st.append(l.loc, v),
          )
        } yield ()
      case IRemoveElem(list, elem) =>
        for {
          l <- transfer(list)
          v <- transfer(elem)
          _ <- modify(_.remove(l.loc, v))
        } yield ()
      case IReturn(expr) =>
        for {
          v <- transfer(expr)
          _ <- doReturn(v)
          _ <- put(AbsState.Bot)
        } yield ()
      case IAssert(expr) =>
        for {
          v <- transfer(expr)
        } yield ()
      case IPrint(expr) => st => st
      case INop()       => st => st
    }

    // transfer function for calls
    def transfer(call: Call): Updater = ???

    // transfer function for expressions
    def transfer(expr: Expr): Result[AbsValue] = ???

    // transfer function for references
    def transfer(ref: Ref): Result[AbsRefValue] = ref match
      case id: Id => AbsRefId(id)
      case Prop(ref, expr) =>
        for {
          rv <- transfer(ref)
          b <- transfer(rv)
          p <- transfer(expr)
        } yield AbsRefProp(b, p)

    // transfer function for reference values
    def transfer(rv: AbsRefValue): Result[AbsValue] = for {
      v <- get(_(rv, cp))
    } yield v

    // transfer function for unary operators
    def transfer(
      st: AbsState,
      uop: UOp,
      operand: AbsValue,
    ): AbsValue = ???

    // operand.simple.getSingle match {
    //   case FlatBot => AbsValue.Bot
    //   case FlatElem(ASimple(x)) =>
    //     optional(AbsValue(Interp.interp(uop, x))).getOrElse(AbsValue.Bot)
    //   case FlatTop =>
    //     uop match {
    //       case ONeg  => exploded(s"uop: ($uop $operand)")
    //       case ONot  => AbsValue(bool = !operand.bool)
    //       case OBNot => exploded(s"uop: ($uop $operand)")
    //     }
    // }

    // transfer function for binary operators
    def transfer(
      st: AbsState,
      bop: BOp,
      left: AbsValue,
      right: AbsValue,
    ): AbsValue = ???

    // (left.getSingle, right.getSingle) match {
    //   case (FlatBot, _) | (_, FlatBot) => AbsValue.Bot
    //   case (FlatElem(ASimple(l)), FlatElem(ASimple(r))) =>
    //     optional(AbsValue(Interp.interp(bop, l, r))).getOrElse(AbsValue.Bot)
    //   case (FlatElem(l), FlatElem(r)) if bop == OEq || bop == OEqual =>
    //     (l, r) match {
    //       case (lloc: Loc, rloc: Loc) =>
    //         if (lloc == rloc) {
    //           if (st.isSingle(lloc)) AVT
    //           else AVB
    //         } else AVF
    //       case _ => AbsValue(l == r)
    //     }
    //   case _ =>
    //     bop match {
    //       case OAnd    => AbsValue(bool = left.bool && right.bool)
    //       case OBAnd   => exploded(s"bop: ($bop $left $right)")
    //       case OBOr    => exploded(s"bop: ($bop $left $right)")
    //       case OBXOr   => exploded(s"bop: ($bop $left $right)")
    //       case ODiv    => exploded(s"bop: ($bop $left $right)")
    //       case OEq     => AbsValue(bool = left =^= right)
    //       case OEqual  => exploded(s"bop: ($bop $left $right)")
    //       case OLShift => exploded(s"bop: ($bop $left $right)")
    //       case OLt     => exploded(s"bop: ($bop $left $right)")
    //       case OMod    => exploded(s"bop: ($bop $left $right)")
    //       case OMul =>
    //         AbsValue(
    //           num = (
    //             (left.num mul right.num) ⊔
    //               (right.num mulInt left.int) ⊔
    //               (left.num mulInt right.int)
    //           ),
    //           int = left.int mul right.int,
    //           bigint = left.bigint mul right.bigint,
    //         )
    //       case OOr => AbsValue(bool = left.bool || right.bool)
    //       case OPlus =>
    //         AbsValue(
    //           str = (
    //             (left.str plus right.str) ⊔
    //               (left.str plusNum right.num)
    //           ),
    //           num = (
    //             (left.num plus right.num) ⊔
    //               (right.num plusInt left.int) ⊔
    //               (left.num plusInt right.int)
    //           ),
    //           int = left.int plus right.int,
    //           bigint = left.bigint plus right.bigint,
    //         )
    //       case OPow     => exploded(s"bop: ($bop $left $right)")
    //       case OSRShift => exploded(s"bop: ($bop $left $right)")
    //       case OSub     => exploded(s"bop: ($bop $left $right)")
    //       case OUMod    => exploded(s"bop: ($bop $left $right)")
    //       case OURShift => exploded(s"bop: ($bop $left $right)")
    //       case OXor     => exploded(s"bop: ($bop $left $right)")
    //     }
    // }

    // return specific value
    def doReturn(v: AbsValue): Result[Unit] = for {
      st <- get
      ret = AbsRet(v, st.copy(locals = Map()))
      _ = sem.doReturn(rp, ret)
    } yield ()

    // return if abrupt completion
    def returnIfAbrupt(
      value: AbsValue,
      check: Boolean,
    ): Result[AbsValue] = {
      val comp = value.comp
      val checkReturn: Result[Unit] =
        if (check) doReturn(AbsValue(comp = comp.removeNormal))
        else ()
      val newValue = comp.normal.value ⊔ value.pure
      for (_ <- checkReturn) yield newValue
    }

    // short circuit evaluation
    def shortCircuit(
      bop: BOp,
      left: Expr,
      right: Expr,
    ): Result[AbsValue] = for {
      l <- transfer(left)
      v <- (bop, l.bool.getSingle) match {
        case (BOp.And, FlatElem(Bool(false))) => pure(AVF)
        case (BOp.Or, FlatElem(Bool(true)))   => pure(AVT)
        case _ =>
          for {
            r <- transfer(right)
            v <- get(transfer(_, bop, l, r))
          } yield v
      }
    } yield v

    // get initial local variables
    import IRFunc.Param
    def getLocals(
      params: List[Param],
      args: List[AbsValue],
      cont: Boolean = false,
    ): Map[Local, AbsValue] = {
      var map = Map[Local, AbsValue]()

      @tailrec
      def aux(ps: List[Param], as: List[AbsValue]): Unit = (ps, as) match {
        case (Nil, Nil) =>
        case (Param(lhs, optional, _) :: pl, Nil) =>
          if (optional) {
            map += lhs -> AbsValue(Absent)
            aux(pl, Nil)
          } else throw AnalysisRemainingParams(ps)
        case (Nil, args) =>
          // XXX Handle GeneratorStart <-> GeneratorResume arith mismatch
          if (!cont) throw AnalysisRemainingArgs(args)
        case (param :: pl, arg :: al) =>
          map += param.lhs -> arg
          aux(pl, al)
      }
      aux(params, args)
      map
    }
  }
//     // transfer function for calls
//     def transfer(call: Call): Updater = call.inst match {
//       case IApp(id, ERef(RefId(Id(name))), args)
//           if simpleFuncs contains name => {
//         for {
//           as <- join(args.map(transfer))
//           vs = if (name == "IsAbruptCompletion") as else as.map(_.escaped)
//           st <- get
//           v <- simpleFuncs(name)(vs)
//           _ <- modify(_.defineLocal(id -> v))
//         } yield ()
//       }
//       case IApp(id, fexpr, args) =>
//         for {
//           value <- transfer(fexpr)
//           vs <- join(args.map(transfer))
//           st <- get
//           v = {
//             // return values
//             var returnValue: AbsValue = AbsValue.Bot
//
//             // algorithms
//             for (AFunc(algo) <- value.func) if (algo.name == "GLOBAL.__ABS__") {
//               optional {
//                 val args = vs(1) // get arguments
//                 val obj = st(args.loc.head)
//                 val name = obj(ASimple(INum(0))).str.head.str
//                 returnValue = name match {
//                   case "Number"  => AbsValue.num
//                   case "Integer" => AbsValue.int
//                   case "BigInt"  => AbsValue.bigint
//                   case "String"  => AbsValue.str
//                   case "Boolean" => AbsValue.bool
//                   case s"INTERVAL:[$x, $y]" if !optional {
//                         x.toDouble; y.toDouble
//                       }.isEmpty =>
//                     AbsValue(num = AbsNum.getInterval(x.toDouble, y.toDouble))
//                   case _ =>
//                     warn(s"invalid abstract value: $name")
//                     AbsValue.Bot
//                 }
//               }.getOrElse(warn("invalid use of __ABS__"))
//             } else {
//               val newLocals = getLocals(algo.head.params, vs)
//               val newSt = st.copy(locals = newLocals)
//               sem.doCall(call, view, st, algo.func, newSt)
//             }
//
//             // closures
//             for (AClo(params, locals, func) <- value.clo) {
//               val newLocals =
//                 locals ++ getLocals(params.map(x => Param(x.name)), vs)
//               val newSt = st.copy(locals = newLocals)
//               sem.doCall(call, view, st, func, newSt)
//             }
//
//             // continuations
//             for (ACont(params, locals, target) <- value.cont)
//               target.node match {
//                 // start/resume sub processes
//                 case _: Entry =>
//                   val newLocals = locals ++ (params zip vs)
//                   val locs = vs.foldLeft(Set[Loc]())(_ ++ _.reachableLocs)
//                   val fixed = st.heap.reachableLocs(locs)
//                   val newSt = st
//                     .copy(locals = newLocals)
//                     .doProcStart(fixed)
//                   sem += target -> newSt
//
//                 // stop/pause sub processes
//                 case arrow: Arrow =>
//                   val nextNp = getNextNp(target, cfg.nextOf(arrow))
//                   val targetSt = sem(target)
//                   sem += nextNp -> st.doProcEnd(targetSt, params zip vs)
//
//                 // othe kinds of continuations
//                 case _ =>
//                   sem += target -> st.copy(locals = locals ++ (params zip vs))
//               }
//
//             returnValue
//           }
//           _ <- {
//             if (v.isBottom) put(AbsState.Bot)
//             else modify(_.defineLocal(id -> v))
//           }
//         } yield ()
//       case access @ IAccess(id, bexpr, expr, args) => {
//         val loc: AllocSite = AllocSite(access.asite, cp.view)
//         for {
//           origB <- transfer(bexpr)
//           b = origB.escaped
//           p <- escape(transfer(expr))
//           astV <- (b.ast.getSingle, p.str.getSingle) match {
//             case (FlatElem(AAst(ast)), FlatElem(Str(name))) =>
//               (ast, name) match {
//                 case (Lexical(kind, str), name) =>
//                   pure(AbsValue(Interp.getLexicalValue(kind, name, str)))
//                 case (ast, "parent") =>
//                   pure(ast.parent.map(AbsValue(_)).getOrElse(AbsValue.absent))
//                 case (ast, "children") =>
//                   for {
//                     _ <- modify(_.allocList(ast.children.map(AbsValue(_)))(loc))
//                   } yield AbsValue(loc)
//                 case (ast, "kind") =>
//                   pure(AbsValue(ast.kind))
//                 case _ =>
//                   ast.semantics(name) match {
//                     case Some((algo, asts)) =>
//                       for {
//                         as <- join(args.map(transfer))
//                         head = algo.head
//                         body = algo.body
//                         vs = asts.map(AbsValue(_)) ++ as
//                         locals = getLocals(head.params, vs)
//                         st <- get
//                         newSt <- get(_.copy(locals = locals))
//                         astOpt =
//                           (
//                             if (
//                               name == "Evaluation" || name == "NamedEvaluation"
//                             ) Some(ast)
//                             else None
//                           )
//                         _ = sem.doCall(call, view, st, algo.func, newSt, astOpt)
//                       } yield AbsValue.Bot
//                     case None =>
//                       val v = AbsValue(ast.subs(name).getOrElse {
//                         error(s"unexpected semantics: ${ast.name}.$name")
//                       })
//                       pure(v)
//                   }
//               }
//             case (FlatBot, _) | (_, FlatBot) => pure(AbsValue.Bot)
//             case _ => exploded("impossible to handle generic access of ASTs")
//           }
//           otherV <- get(_(origB, p))
//           value = astV ⊔ otherV
//           _ <- {
//             if (!value.isBottom) modify(_.defineLocal(id -> value))
//             else put(AbsState.Bot)
//           }
//         } yield ()
//       }
//     }
//
//     // transfer function for arrow instructions
//     def transfer(arrow: Arrow, np: NodePoint[Node]): Updater =
//       arrow.inst match {
//         case IClo(id, params, captured, body) =>
//           for {
//             st <- get
//             _ <- modify(
//               _.defineLocal(
//                 id -> AbsValue(
//                   AClo(
//                     params,
//                     captured.map(x => x -> st(x, cp)).toMap,
//                     cfg.bodyFuncMap(body.uid),
//                   ),
//                 ),
//               ),
//             )
//           } yield ()
//         case ICont(id, params, body) =>
//           for {
//             locals <- get(_.locals)
//             _ <- modify(
//               _.defineLocal(
//                 id -> AbsValue(
//                   ACont(
//                     params,
//                     locals,
//                     NodePoint(cfg.bodyFuncMap(body.uid).entry, view),
//                   ),
//                 ),
//               ),
//             )
//           } yield ()
//         case IWithCont(id, params, body) =>
//           for {
//             locals <- get(_.locals)
//             _ <- modify(
//               _.defineLocal(
//                 id -> AbsValue(
//                   ACont(
//                     params,
//                     locals,
//                     np,
//                   ),
//                 ),
//               ),
//             )
//             st <- get
//             _ = sem += NodePoint(cfg.bodyFuncMap(body.uid).entry, view) -> st
//             _ <- put(AbsState.Bot)
//           } yield ()
//       }
//
//     // transfer function for expressions
//     def transfer(expr: Expr): Result[AbsValue] = expr match {
//       case ENum(n)      => AbsValue(Num(n))
//       case EINum(l)     => AbsValue(l)
//       case EBigINum(b)  => AbsValue(b)
//       case EStr(str)    => AbsValue(str)
//       case EBool(b)     => AbsValue(b)
//       case EUndef       => AbsValue.undef
//       case ENull        => AbsValue.nullv
//       case EAbsent      => AbsValue.absent
//       case EConst(name) => AbsValue(AConst(name))
//       case EComp(ty, value, target) =>
//         for {
//           y <- escape(transfer(ty))
//           v <- escape(transfer(value))
//           origT <- escape(transfer(target))
//           t = AbsValue(str = origT.str, const = origT.const)
//         } yield AbsValue(comp = AbsComp((for {
//           AConst(name) <- y.const.toList
//         } yield name -> AbsComp.Result(v, t)).toMap))
//       case map @ EMap(ty, props) => {
//         val loc: AllocSite = AllocSite(map.asite, cp.view)
//         for {
//           pairs <- join(props.map {
//             case (kexpr, vexpr) =>
//               for {
//                 k <- transfer(kexpr)
//                 v <- transfer(vexpr)
//               } yield (k, v)
//           })
//           _ <- modify(_.allocMap(ty, pairs)(loc))
//         } yield AbsValue(loc)
//       }
//       case list @ EList(exprs) => {
//         val loc: AllocSite = AllocSite(list.asite, cp.view)
//         for {
//           vs <- join(exprs.map(transfer))
//           _ <- modify(_.allocList(vs.map(_.escaped))(loc))
//         } yield AbsValue(loc)
//       }
//       case symbol @ ESymbol(desc) => {
//         val loc: AllocSite = AllocSite(symbol.asite, cp.view)
//         for {
//           v <- transfer(desc)
//           newV = AbsValue(str = v.str, undef = v.undef)
//           _ <- modify(_.allocSymbol(newV)(loc))
//         } yield AbsValue(loc)
//       }
//       case EPop(list, idx) =>
//         for {
//           l <- escape(transfer(list))
//           loc = l.loc
//           k <- escape(transfer(idx))
//           v <- id(_.pop(loc, k))
//         } yield v
//       case ERef(ref) =>
//         for {
//           rv <- transfer(ref)
//           v <- transfer(rv)
//         } yield v
//       case EUOp(uop, expr) =>
//         for {
//           x <- escape(transfer(expr))
//           v <- get(transfer(_, uop, x))
//         } yield v
//       case EBOp(OAnd, left, right) => shortCircuit(OAnd, left, right)
//       case EBOp(OOr, left, right)  => shortCircuit(OOr, left, right)
//       case EBOp(OEq, ERef(ref), EAbsent) =>
//         for {
//           rv <- transfer(ref)
//           b <- get(_.exists(rv))
//         } yield AbsValue(bool = !b)
//       case EBOp(bop, left, right) =>
//         for {
//           l <- escape(transfer(left))
//           r <- escape(transfer(right))
//           v <- get(transfer(_, bop, l, r))
//         } yield v
//       case ETypeOf(expr) =>
//         for {
//           value <- escape(transfer(expr))
//           st <- get
//         } yield {
//           var set = Set[String]()
//           if (!value.comp.isBottom) set += "Completion"
//           if (!value.const.isBottom) set += "Constant"
//           if (!value.loc.isBottom) for (loc <- value.loc) {
//             set += (st(loc).getTy.name match {
//               case name if name endsWith "Object" => "Object"
//               case name                           => name
//             })
//           }
//           if (!value.func.isBottom) set += "Function"
//           if (!value.clo.isBottom) set += "Closure"
//           if (!value.cont.isBottom) set += "Continuation"
//           if (!value.ast.isBottom) set += "AST"
//           if (!value.num.isBottom || !value.int.isBottom) set += "Number"
//           if (!value.bigint.isBottom) set += "BigInt"
//           if (!value.str.isBottom) set += "String"
//           if (!value.bool.isBottom) set += "Boolean"
//           if (!value.undef.isBottom) set += "Undefined"
//           if (!value.nullv.isBottom) set += "Null"
//           if (!value.absent.isBottom) set += "Absent"
//           AbsValue(str = AbsStr(set.map(Str(_))))
//         }
//       case EIsCompletion(expr) =>
//         for {
//           v <- transfer(expr)
//         } yield AbsValue(bool = v.isCompletion)
//       case EIsInstanceOf(base, name) =>
//         for {
//           origB <- transfer(base)
//           b = origB.escaped
//           st <- get
//         } yield AbsValue(bool = AbsBool((for {
//           Bool(bool) <- origB.isAbruptCompletion.toSet
//           resB <-
//             if (bool) Set(false)
//             else {
//               var set = Set[Boolean]()
//               for (AAst(ast) <- b.ast.toList) {
//                 set += ast.name == name || ast.getKinds.contains(name)
//               }
//               // XXX for (Str(str) <- b.str.toList) set += str == name
//               for (loc <- b.loc.toList) set += st(loc).getTy < Ty(name)
//               val otherV = b.copy(
//                 // XXX simple = b.simple.copy(str = AbsStr.Bot),
//                 ast = AbsAST.Bot,
//                 loc = AbsLoc.Bot,
//               )
//               if (!otherV.isBottom) set += false
//               set
//             }
//         } yield resB).map(Bool(_))))
//       case elems @ EGetElems(base, name) => {
//         val loc: AllocSite = AllocSite(elems.asite, cp.view)
//         for {
//           bv <- escape(transfer(base))
//           _ <- bv.getSingle match {
//             case FlatBot => put(AbsState.Bot)
//             case FlatElem(AAst(ast)) =>
//               val vs = ast.getElems(name).map(AbsValue(_))
//               modify(_.allocList(vs)(loc))
//             case _ => exploded("get-elems")
//           }
//         } yield AbsValue(loc)
//       }
//       case EGetSyntax(base) =>
//         for {
//           v <- escape(transfer(base))
//           s = AbsStr(v.ast.toList.map(x => Str(x.ast.toString)))
//         } yield AbsValue(str = s)
//       case EParseSyntax(code, rule, parserParams) =>
//         for {
//           v <- escape(transfer(code))
//           ruleV <- escape(transfer(rule))
//           p = ruleV.str.getSingle match {
//             case FlatElem(Str(str)) =>
//               ESParser.rules.getOrElse(
//                 str,
//                 error(s"not exist parse rule: $rule"),
//               )
//             case _ => exploded("get-syntax")
//           }
//           st <- get
//         } yield AbsValue(v.getSingle match {
//           case FlatElem(AAst(ast)) =>
//             Interp.doParseAst(p(ast.parserParams))(ast)
//           case FlatElem(ASimple(Str(str))) => {
//             Interp.doParseStr(p(parserParams))(str)
//           }
//           case v => error(s"not an AST value or a string: $v")
//         })
//       case EConvert(source, target, flags) =>
//         for {
//           s <- escape(transfer(source))
//           fs <- join(for (flag <- flags) yield escape(transfer(flag)))
//         } yield {
//           var newV: AbsValue = AbsValue.Bot
//           for (Str(str) <- s.str) newV ⊔= (target match {
//             case CStrToNum    => AbsValue(Num(ESValueParser.str2num(str)))
//             case CStrToBigInt => AbsValue(ESValueParser.str2bigint(str))
//             case _            => AbsValue.Bot
//           })
//           var doubleSet = Set[Double]()
//           for (INum(long) <- s.int) doubleSet += long.toDouble
//           for (Num(double) <- s.num) doubleSet += double
//           for (n <- doubleSet) newV ⊔= (target match {
//             case CNumToStr =>
//               AbsValue(
//                 toStringHelper(
//                   n,
//                   fs.map(_.getSingle) match {
//                     case FlatElem(ASimple(INum(n))) :: _ => n.toInt
//                     case FlatElem(ASimple(Num(n))) :: _  => n.toInt
//                     case _                               => 10
//                   },
//                 ),
//               )
//             case CNumToInt =>
//               AbsValue((math.signum(n) * math.floor(math.abs(n))).toLong)
//             case CNumToBigInt =>
//               AbsValue(BigInt(new java.math.BigDecimal(n).toBigInteger))
//             case _ =>
//               AbsValue.Bot
//           })
//           for (BigINum(b) <- s.bigint) newV ⊔= (target match {
//             case CNumToBigInt => AbsValue(b)
//             case CNumToStr    => AbsValue(b.toString)
//             case CBigIntToNum => AbsValue(Num(b.toDouble))
//             case _            => AbsValue.Bot
//           })
//           newV
//         }
//       case EContains(list, elem) =>
//         for {
//           l <- escape(transfer(list))
//           v <- escape(transfer(elem))
//           b <- get(_.contains(l.loc, v))
//         } yield AbsValue(bool = b)
//       case EReturnIfAbrupt(rexpr @ ERef(ref), check) =>
//         for {
//           rv <- transfer(ref)
//           v <- transfer(rv)
//           newV <- returnIfAbrupt(v, check)
//           _ <- modify(_.update(rv, newV))
//         } yield newV
//       case EReturnIfAbrupt(expr, check) =>
//         for {
//           v <- transfer(expr)
//           newV <- returnIfAbrupt(v, check)
//         } yield newV
//       case copy @ ECopy(obj) => {
//         val loc: AllocSite = AllocSite(copy.asite, cp.view)
//         for {
//           v <- escape(transfer(obj))
//           _ <- modify(_.copyObj(v.loc)(loc))
//         } yield AbsValue(loc)
//       }
//       case keys @ EKeys(mobj, intSorted) => {
//         val loc: AllocSite = AllocSite(keys.asite, cp.view)
//         for {
//           v <- escape(transfer(mobj))
//           _ <- modify(_.keys(v.loc, intSorted)(loc))
//         } yield AbsValue(loc)
//       }
//       case ENotSupported(msg) => AbsValue.Bot
//     }
//
//
//
//
//
//
//

}
