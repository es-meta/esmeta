package esmeta.analyzer

import esmeta.DEBUG
import esmeta.analyzer.domain.*
import esmeta.cfg.*
import esmeta.error.*
import esmeta.interp.*
import esmeta.interp.util.*
import esmeta.ir.{Func => IRFunc, *}
import esmeta.js.*
import esmeta.js.util.ESValueParser
import esmeta.util.BaseUtils.*
import scala.annotation.tailrec

/** abstract transfer function */
case class AbsTransfer(sem: AbsSemantics) {
  // loading monads
  import AbsState.monad._

  /** transfer function for control points */
  def apply(cp: ControlPoint): Unit = cp match
    case (np: NodePoint[_]) => this(np)
    case (rp: ReturnPoint)  => this(rp)

  /** transfer function for node points */
  def apply[T <: Node](np: NodePoint[T]): Unit = {
    val st = sem(np)
    val NodePoint(func, node, view) = np
    val helper = new Helper(np)

    import helper._
    node match {
      case Block(_, insts, next) =>
        val newSt = insts.foldLeft(st) {
          case (nextSt, inst) =>
            if (!nextSt.isBottom) transfer(inst)(nextSt)
            else nextSt
        }
        next.foreach(to => sem += getNextNp(np, to) -> newSt)
      case call: Call =>
        val (_, newSt) = (for {
          v <- transfer(call)
          _ <-
            if (v.isBottom) put(AbsState.Bot)
            else modify(_.defineLocal(call.lhs -> v))
        } yield ())(st)
        call.next.foreach(to => sem += getNextNp(np, to) -> newSt)
      case br @ Branch(_, kind, cond, thenNode, elseNode) =>
        (for {
          v <- transfer(cond)
          b = v.bool
          newSt <- get
        } yield {
          // TODO prune
          if (AT ⊑ b)
            thenNode.foreach(to =>
              sem += getNextNp(np, to) -> prune(cond, true)(newSt),
            )
          if (AF ⊑ b)
            elseNode.foreach(to =>
              sem += getNextNp(np, to, br.isLoop) -> prune(cond, false)(newSt),
            )
        })(st)
    }
  }

  /** get next node point */
  private def getNextNp(
    fromCp: NodePoint[Node],
    to: Node,
    loopOut: Boolean = false,
  ): NodePoint[Node] =
    val NodePoint(func, from, view) = fromCp

    // handle loop sensitivity
    val fromView = if (loopOut) sem.loopExit(view) else view
    val toView = to match
      case br: Branch if br.isLoop =>
        if (from.isLoopPred) sem.loopEnter(view, br)
        else sem.loopNext(view)
      case _ => fromView

    // next node point
    NodePoint(func, to, toView)

  // transfer function for return points
  def apply(rp: ReturnPoint): Unit = {
    var ret @ AbsRet(value, st) = sem(rp)

    // proper type handle
    Interp.setTypeMap
      .get(rp.func.name)
      .map(ty => {
        if (!value.loc.isBottom) st = st.setType(value.loc, ty)
      })

    // debugging message
    if (DEBUG) println(s"<RETURN> $ret")

    // return wrapped values
    for {
      np @ NodePoint(func, call, view) <- sem.getRetEdges(rp)
      nextNode <- call.next
    } {
      val callerSt = sem.callInfo(np)
      val nextNp = NodePoint(
        func,
        nextNode,
        nextNode match {
          case br: Branch if br.isLoop => sem.loopEnter(view, br)
          case _                       => view
        },
      )

      val newSt = st.doReturn(
        callerSt,
        // wrap completion by conditions specified in
        // [5.2.3.5 Implicit Normal Completion]
        // (https://tc39.es/ecma262/#sec-implicit-normal-completion)
        call.lhs -> (if (rp.func.isReturnComp) value.wrapCompletion else value),
      )

      sem += nextNp -> newSt
    }
  }

  // transfer function for expressions
  def apply(cp: ControlPoint, expr: Expr): AbsValue = {
    val st = sem.getState(cp)
    val helper = new Helper(cp)
    helper.transfer(expr)(st)._1
  }

  /** sdo with default case */
  val defaultCases = List(
    "Contains",
    "AllPrivateIdentifiersValid",
    "ContainsArguments",
  )

  /** get syntax-directed operation(SDO) */
  private val getSDO = cached[(Ast, String), Option[(Ast, Func)]] {
    case (ast, operation) =>
      val fnameMap = cfg.fnameMap
      ast.chains.foldLeft[Option[(Ast, Func)]](None) {
        case (None, ast0) =>
          val subIdx = getSubIdx(ast0)
          val fname = s"${ast0.name}[${ast0.idx},${subIdx}].$operation"
          fnameMap.get(fname) match
            case Some(sdo) => Some(ast0, sdo)
            case None if defaultCases contains operation =>
              Some(ast0, fnameMap(s"<DEFAULT>.$operation"))
            case _ => None
        case (res: Some[_], _) => res
      }
  }

  /** get sub index of parsed Ast */
  private val getSubIdx = cached[Ast, Int] {
    case lex: Lexical => 0
    case Syntactic(name, _, rhsIdx, children) =>
      val rhs = cfg.grammar.nameMap(name).rhsList(rhsIdx)
      val optionals = (for {
        (opt, child) <- rhs.nts.map(_.optional) zip children if opt
      } yield !child.isEmpty)
      optionals.reverse.zipWithIndex.foldLeft(0) {
        case (acc, (true, idx)) => acc + scala.math.pow(2, idx).toInt
        case (acc, _)           => acc
      }
  }

  /** internal prune function */
  private trait PruneHelper { this: Helper =>

    /** prune condition */
    def prune(cond: Expr, positive: Boolean): Updater = cond match {
      case EUnary(UOp.Not, e) => prune(e, !positive)
      // TODO debugging
      // case EBinary(BOp.Eq, ERef(ref), target) =>
      //   for {
      //     rv <- transfer(ref)
      //     v <- transfer(rv)
      //     targetV <- transfer(target)
      //     newV = if (positive) v ⊓ targetV else v - targetV
      //     _ <- modify(_.update(rv, newV))
      //   } yield ()
      case EBinary(BOp.Eq, ETypeOf(ERef(ref)), tyRef: ERef) =>
        for {
          rv <- transfer(ref)
          tv <- transfer(tyRef)
          _ <- modify(pruneType(rv, tv, positive))
        } yield ()
      case EBinary(BOp.Or, l, r) =>
        st =>
          val lst = prune(l, positive)(st)
          val rst = prune(r, positive)(st)
          if (positive) lst ⊔ rst else lst ⊓ rst
      case EBinary(BOp.And, l, r) =>
        st =>
          val lst = prune(l, positive)(st)
          val rst = prune(r, positive)(st)
          if (positive) lst ⊓ rst else lst ⊔ rst
      case _ => st => st
    }

    /** prune type */
    def pruneType(l: AbsRefValue, r: AbsValue, positive: Boolean): Updater =
      for {
        lv <- transfer(l)
        st <- get
        tyV = {
          var newV = AbsValue.Bot
          for (Str(tyStr) <- r.str) {
            if (tyStr == "Number") newV ⊔= AbsValue(num = AbsNum.Top)
            if (tyStr == "BigInt") newV ⊔= AbsValue(bigint = AbsBigInt.Top)
            if (tyStr == "String") newV ⊔= AbsValue(str = AbsStr.Top)
            if (tyStr == "Boolean") newV ⊔= AbsValue(bool = AbsBool.Top)
            if (tyStr == "Undefined") newV ⊔= AbsValue(undef = AbsUndef.Top)
            if (tyStr == "Null") newV ⊔= AbsValue(nullv = AbsNull.Top)
            // TODO symbol, object
          }
          newV
        }
        // TODO fix loc
        updatedV =
          if (positive) (lv ⊓ tyV) ⊔ AbsValue(loc = lv.loc) else lv - tyV
        _ <- modify(_.update(l, updatedV))
      } yield ()
  }

  /** internal transfer function with a specific view */
  private class Helper(val cp: ControlPoint) extends PruneHelper {
    lazy val func = cp.func
    lazy val view = cp.view
    lazy val rp = ReturnPoint(func, view)

    /** transfer function for normal instructions */
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

    /** transfer function for call instructions */
    def transfer(call: Call): Result[AbsValue] = call.callInst match {
      case ICall(_, fexpr, args) =>
        for {
          fv <- transfer(fexpr)
          as <- join(args.map(transfer))
          st <- get
        } yield {
          for (AClo(func, captured) <- fv.clo) {
            val newLocals = getLocals(func.irFunc.params, as) ++ captured
            val newSt = st.copied(locals = newLocals)
            sem.doCall(call, view, st, func, newSt)
          }
          for (ACont(target, captured) <- fv.cont) {
            val as0 =
              as.map(v => if (func.isReturnComp) v.wrapCompletion else v)
            val newLocals =
              getLocals(target.func.irFunc.params, as0, cont = true) ++ captured
            sem += target -> st.copied(locals = newLocals)
          }
          AbsValue.Bot
        }
      case IMethodCall(_, base, method, args) =>
        for {
          rv <- transfer(base)
          bv <- transfer(rv)
          // TODO do not explicitly store methods in object but use a type model when
          // accessing methods
          fv <- get(_(bv, AbsValue(method)))
          as <- join(args.map(transfer))
          st <- get
        } yield {
          for (AClo(func, _) <- fv.clo) {
            val newLocals = getLocals(func.irFunc.params, bv :: as)
            val newSt = st.copied(locals = newLocals)
            sem.doCall(call, view, st, func, newSt)
          }
          AbsValue.Bot
        }
      case ISdoCall(_, base, method, args) =>
        for {
          bv <- transfer(base)
          as <- join(args.map(transfer))
          st <- get
        } yield {
          var newV: AbsValue = AbsValue.Bot
          bv.getSingle match
            case FlatElem(AAst(syn: Syntactic)) =>
              getSDO((syn, method)) match
                case Some((ast0, sdo)) =>
                  val newLocals =
                    getLocals(sdo.irFunc.params, AbsValue(ast0) :: as)
                  val newSt = st.copied(locals = newLocals)
                  sem.doCall(call, view, st, sdo, newSt)
                case None => error("invalid sdo")
            case FlatElem(AAst(lex: Lexical)) =>
              newV = AbsValue(Interp.interp(lex, method))
            case FlatTop => exploded("sdo call")
            case _       => /* do nothing */
          newV
        }
    }

    /** transfer function for expressions */
    def transfer(expr: Expr): Result[AbsValue] = expr match {
      case EComp(ty, value, target) =>
        for {
          y <- transfer(ty)
          v <- transfer(value)
          origT <- transfer(target)
          t = AbsValue(str = origT.str, const = origT.const)
        } yield AbsValue(comp = AbsComp((for {
          AConst(name) <- y.const.toList
        } yield name -> AbsComp.Result(v, t)).toMap))
      case EIsCompletion(expr) =>
        for {
          v <- transfer(expr)
        } yield AbsValue(bool = v.isCompletion)
      case EReturnIfAbrupt(ERef(ref), check) =>
        for {
          rv <- transfer(ref)
          v <- transfer(rv)
          newV <- returnIfAbrupt(v, check)
          _ <-
            if (!newV.isBottom) modify(_.update(rv, newV))
            else put(AbsState.Bot)
        } yield newV
      case EReturnIfAbrupt(expr, check) =>
        for {
          v <- transfer(expr)
          newV <- returnIfAbrupt(v, check)
        } yield newV
      case EPop(list, front) =>
        for {
          v <- transfer(list)
          pv <- id(_.pop(v.loc, front))
        } yield pv
      case EParse(code, rule) =>
        for {
          c <- transfer(code)
          r <- transfer(rule)
        } yield {
          var newV: AbsValue = AbsValue.Bot

          // codes
          var codes: Set[(String, List[Boolean])] = Set()
          for (Str(s) <- c.str) codes += (s, List())
          for (AAst(ast) <- c.ast) {
            val code = ast.toString(grammar = Some(cfg.grammar))
            val args = ast match
              case syn: Syntactic => syn.args
              case _              => List()
            codes += (code, args)
          }

          // parse
          for {
            AGrammar(name, params) <- r.grammar
            (str, args) <- codes
            parseArgs = if (params.isEmpty) args else params
          } newV ⊔= AbsValue(
            (name, sem.cachedAst) match {
              case ("Script", Some(cached)) if str == sem.sourceText =>
                cached
              case _ => cfg.jsParser(name, parseArgs).from(str),
            },
          )

          // result
          newV
        }
      case EGrammar(name, params) => AbsValue(Grammar(name, params))
      case ESourceText(expr) =>
        for {
          v <- transfer(expr)
          s = AbsStr(
            v.ast.toList.map(x =>
              Str(x.ast.toString(grammar = Some(cfg.grammar)).trim),
            ),
          )
        } yield AbsValue(str = s)
      case e @ EGetChildren(kindOpt, ast) =>
        val loc: AllocSite = AllocSite(e.asite, cp.view)
        for {
          kOpt <- id(st => {
            kindOpt match
              case Some(kind) => transfer(kind).map(Some(_))(st)
              case None       => (None, st)
          })
          a <- transfer(ast)
          _ <- (kOpt.map(_.getSingle), a.getSingle) match
            case (Some(FlatBot), _) | (_, FlatBot) => put(AbsState.Bot)
            case (Some(FlatTop), _) | (_, FlatTop) => exploded("EGetChildren")
            case (Some(FlatElem(AGrammar(name, _))), FlatElem(AAst(ast))) =>
              val vs = ast.getChildren(name).map(AbsValue(_))
              modify(_.allocList(vs)(loc))
            case (None, FlatElem(AAst(syn: Syntactic))) =>
              val vs = syn.children.flatten.map(AbsValue(_))
              modify(_.allocList(vs)(loc))
            case _ => put(AbsState.Bot)
        } yield AbsValue(loc)
      case EYet(_) => AbsValue.Bot
      case EContains(list, elem, field) =>
        for {
          l <- transfer(list)
          v <- transfer(elem)
          st <- get
        } yield field match
          case Some((_, f)) => ??? // TODO
          case None         => AbsValue(bool = st.contains(l.loc, v))
      case ESubstring(expr, from, to) =>
        for {
          v <- transfer(expr)
          f <- transfer(from)
          t <- transfer(to)
        } yield (v.getSingle, f.getSingle, t.getSingle) match
          case (FlatBot, _, _) | (_, FlatBot, _) | (_, _, FlatBot) =>
            AbsValue.Bot
          case (FlatTop, _, _) | (_, FlatTop, _) | (_, _, FlatTop) =>
            exploded("ESubstring")
          case (
                FlatElem(ASimple(Str(s))),
                FlatElem(AMath(f)),
                FlatElem(AMath(t)),
              ) if f.isValidInt =>
            if (s.length < t) AbsValue(s.substring(f.toInt))
            else if (t.isValidInt) AbsValue(s.substring(f.toInt, t.toInt))
            else AbsValue.Bot
          case _ => AbsValue.Bot
      case ERef(ref) =>
        for {
          rv <- transfer(ref)
          v <- transfer(rv)
        } yield v
      case EUnary(uop, expr) =>
        for {
          v <- transfer(expr)
          v0 <- get(transfer(_, uop, v))
        } yield v0
      case EBinary(BOp.And, left, right) =>
        shortCircuit(BOp.And, left, right)
      case EBinary(BOp.Or, left, right) => shortCircuit(BOp.Or, left, right)
      case EBinary(BOp.Eq, ERef(ref), EAbsent) =>
        for {
          rv <- transfer(ref)
          b <- get(_.exists(rv))
        } yield AbsValue(bool = !b)
      case EBinary(bop, left, right) =>
        for {
          lv <- transfer(left)
          rv <- transfer(right)
          v <- get(transfer(_, bop, lv, rv))
        } yield v
      case EVariadic(vop, exprs) =>
        for {
          vs <- join(exprs.map(transfer))
        } yield transfer(vop, vs)
      case EConvert(cop, expr) =>
        import COp.*
        for {
          v <- transfer(expr)
          r <- cop match
            case ToStr(Some(radix)) => transfer(radix)
            case ToStr(None)        => pure(AbsValue(Math(10)))
            case _                  => pure(AbsValue.Bot)
        } yield {
          var newV: AbsValue = AbsValue.Bot
          for (Str(s) <- v.str) newV ⊔= (cop match
            case ToNumber => AbsValue(Number(ESValueParser.str2Number(s)))
            case ToBigInt => AbsValue(ESValueParser.str2bigint(s))
            case _        => AbsValue.Bot
          )
          for (AMath(n) <- v.math) newV ⊔= (cop match
            case ToNumber => AbsValue(Number(n.toDouble))
            case ToBigInt => AbsValue(BigInt(n.toBigInt))
            case _        => AbsValue.Bot
          )
          for (BigInt(b) <- v.bigint) newV ⊔= (cop match
            case ToMath => AbsValue(Math(BigDecimal.exact(b)))
            case _      => AbsValue.Bot
          )
          for (ACodeUnit(cu) <- v.codeunit) newV ⊔= (cop match
            case ToMath => AbsValue(Math(BigDecimal.exact(cu.toInt)))
            case _      => AbsValue.Bot
          )
          for (Number(d) <- v.num)
            newV ⊔= (cop match
              case ToNumber | ToMath if d.isInfinity => AbsValue(d)
              case ToMath => AbsValue(Math(BigDecimal.exact(d)))
              case _: ToStr =>
                var newV0 = AbsValue.Bot
                for (AMath(n) <- r.math if n.isValidInt) {
                  newV0 ⊔= AbsValue(toStringHelper(d, n.toInt))
                }
                for (Number(n) <- r.num if n.isValidInt) {
                  newV0 ⊔= AbsValue(toStringHelper(d, n.toInt))
                }
                newV0
              case _ => AbsValue.Bot
            )
          newV
        }
      case ETypeOf(base) =>
        for {
          v <- transfer(base)
          st <- get
        } yield {
          var set = Set[String]()
          if (!v.num.isBottom) set += "Number"
          if (!v.bigint.isBottom) set += "BigInt"
          if (!v.str.isBottom) set += "String"
          if (!v.bool.isBottom) set += "Boolean"
          if (!v.undef.isBottom) set += "Undefined"
          if (!v.nullv.isBottom) set += "Null"
          if (!v.loc.isBottom) for (loc <- v.loc) {
            val tname = st(loc).getTy match
              case tname if cfg.typeModel.subType(tname, "Object") =>
                "Object"
              case tname => tname
            set += tname
          }
          AbsValue(str = AbsStr(set.map(Str.apply)))
        }
      case ETypeCheck(expr, tyExpr) =>
        for {
          v <- transfer(expr)
          tv <- transfer(tyExpr)
          st <- get
          tname <- tv.getSingle match
            case FlatElem(ASimple(Str(s))) => pure(s)
            case FlatElem(AGrammar(n, _))  => pure(n)
            case _                         => exploded("ETypeCheck")
        } yield {
          var bv: AbsBool = AbsBool.Bot
          if (!v.num.isBottom) bv ⊔= AbsBool(Bool(tname == "Number"))
          if (!v.bigint.isBottom) bv ⊔= AbsBool(Bool(tname == "BigInt"))
          if (!v.str.isBottom) bv ⊔= AbsBool(Bool(tname == "String"))
          if (!v.bool.isBottom) bv ⊔= AbsBool(Bool(tname == "Boolean"))
          if (!v.const.isBottom)
            bv ⊔= AbsBool(Bool(tname == "Constant"))
          if (!v.comp.isBottom)
            bv ⊔= AbsBool(Bool(tname == "CompletionRecord"))
          if (!v.undef.isBottom)
            bv ⊔= AbsBool(Bool(tname == "Undefined"))
          if (!v.nullv.isBottom) bv ⊔= AbsBool(Bool(tname == "Null"))
          if (!v.clo.isBottom)
            bv ⊔= AbsBool(Bool(tname == "AbstractClosure"))
          v.ast.getSingle match
            case FlatBot => /* do nothing */
            case FlatTop => bv = AB
            case FlatElem(AAst(ast)) =>
              bv ⊔= AbsBool(
                Bool(tname == "ParseNode" || (ast.types contains tname)),
              )
          for (loc <- v.loc) {
            val tname0 = st(loc).getTy
            bv ⊔= AbsBool(
              Bool(
                tname0 == tname || cfg.typeModel.subType(tname0, tname),
              ),
            )
          }
          AbsValue(bool = bv)
        }
      case EClo(fname, cap) =>
        for {
          st <- get
          func = cfg.fnameMap(fname)
          captured = cap.map(x => x -> st.lookupLocal(x)).toMap
        } yield AbsValue(AClo(func, captured))
      case ECont(fname) =>
        for {
          st <- get
          func = cfg.fnameMap(fname)
          target = NodePoint(func, func.entry.get, cp.view)
          captured = st.locals.collect { case (x: Name, av) => x -> av }
          // return edges for resumed evaluation
          currRp = ReturnPoint(cp.func, cp.view)
          contRp = ReturnPoint(func, cp.view)
          _ = sem.retEdges += (contRp -> sem.retEdges.getOrElse(currRp, Set()))
        } yield AbsValue(ACont(target, captured))
      case ESyntactic(name, args, rhsIdx, children) =>
        for {
          cs <- join(children.map {
            case Some(child) => transfer(child).map(Some(_))
            case None        => pure(None)
          })
        } yield {
          val cs0 = cs.map(cOpt => cOpt.map(_.ast))
          if (cs.exists(cOpt => cOpt.fold(false)(_.isBottom))) AbsValue.Bot
          else {
            val cs1 = cs0
              .map(cOpt =>
                cOpt.map(_.getSingle match {
                  case FlatTop               => exploded("ESyntactic")
                  case FlatElem(AAst(child)) => child
                  case FlatBot               => ??? // impossible
                }),
              )
            AbsValue(Syntactic(name, args, rhsIdx, cs1))
          }
        }
      case ELexical(name, expr) => ??? // TODO
      case e @ EMap(ty, props) =>
        val loc: AllocSite = AllocSite(e.asite, cp.view)
        for {
          pairs <- join(props.map {
            case (kexpr, vexpr) =>
              for {
                k <- transfer(kexpr)
                v <- transfer(vexpr)
              } yield (k, v)
          })
          _ <- modify(_.allocMap(ty.name, pairs)(loc))
        } yield AbsValue(loc)
      case e @ EList(exprs) =>
        val loc: AllocSite = AllocSite(e.asite, cp.view)
        for {
          vs <- join(exprs.map(transfer))
          _ <- modify(_.allocList(vs)(loc))
        } yield AbsValue(loc)
      case e @ EListConcat(exprs) =>
        import AbsObj.*
        val loc: AllocSite = AllocSite(e.asite, cp.view)
        for {
          ls <- join(exprs.map(transfer))
          st <- get
          vs = ls.foldLeft(List[AbsValue]()) {
            case (acc, l) =>
              l.getSingle match
                case FlatElem(loc: Loc) =>
                  st(loc) match
                    case KeyWiseList(vs) => acc ++ vs
                    case _               => ???
                case _ => ???
          }
          _ <- modify(_.allocList(vs)(loc))
        } yield AbsValue(loc)
      case e @ ESymbol(desc) =>
        val loc: AllocSite = AllocSite(e.asite, cp.view)
        for {
          v <- transfer(desc)
          _ <- modify(
            _.allocSymbol(AbsValue(str = v.str, undef = v.undef))(loc),
          )
        } yield AbsValue(loc)
      case e @ ECopy(obj) =>
        val loc: AllocSite = AllocSite(e.asite, cp.view)
        for {
          v <- transfer(obj)
          _ <- modify(_.copyObj(v.loc)(loc))
        } yield AbsValue(loc)
      case e @ EKeys(map, intSorted) =>
        val loc: AllocSite = AllocSite(e.asite, cp.view)
        for {
          v <- transfer(map)
          _ <- modify(_.keys(v.loc, intSorted)(loc))
        } yield AbsValue(loc)
      case EDuplicated(expr) =>
        import AbsObj.*
        for {
          v <- transfer(expr)
          st <- get
        } yield AbsValue(bool = v.loc.foldLeft(AbsBool.Bot: AbsBool) {
          case (avb, loc) =>
            avb ⊔ (st(loc) match {
              case _: MergedList => AT
              case KeyWiseList(vs) if vs.forall(_.isSingle) =>
                val values = vs.map(_.getSingle).flatMap {
                  case FlatElem(v) => Some(v)
                  case _           => None
                }
                AbsBool(Bool(values.toSet.size != values.size))
              case _: KeyWiseList => AT
              case _              => AbsBool.Bot
            })
        })
      case EIsArrayIndex(expr) =>
        for {
          v <- transfer(expr)
        } yield v.getSingle match
          case FlatBot => AbsValue.Bot
          case FlatElem(ASimple(Str(s))) =>
            val d = ESValueParser.str2Number(s)
            val ds = toStringHelper(d)
            val UPPER = (1L << 32) - 1
            val l = d.toLong
            AbsValue(ds == s && 0 <= l && d == l && l < UPPER)
          case FlatElem(_) => AVF
          case FlatTop     => exploded("EIsArrayIndex")
      case EMathVal(n)           => AbsValue(Math(n))
      case ENumber(n) if n.isNaN => AbsValue(Double.NaN)
      case ENumber(n)            => AbsValue(n)
      case EBigInt(n)            => AbsValue(BigInt(n))
      case EStr(str)             => AbsValue(Str(str))
      case EBool(b)              => AbsValue(Bool(b))
      case EUndef                => AbsValue(Undef)
      case ENull                 => AbsValue(Null)
      case EAbsent               => AbsValue(Absent)
      case EConst(name)          => AbsValue(Const(name))
      case ECodeUnit(c)          => AbsValue(CodeUnit(c))
    }

    /** transfer function for references */
    def transfer(ref: Ref): Result[AbsRefValue] = ref match
      case id: Id => AbsRefId(id)
      case Prop(ref, expr) =>
        for {
          rv <- transfer(ref)
          b <- transfer(rv)
          p <- transfer(expr)
        } yield AbsRefProp(b, p)

    /** transfer function for reference values */
    def transfer(rv: AbsRefValue): Result[AbsValue] = for {
      v <- get(_(rv, cp))
    } yield v

    /** transfer function for unary operators */
    def transfer(
      st: AbsState,
      uop: UOp,
      operand: AbsValue,
    ): AbsValue =
      import UOp.*
      operand.getSingle match
        case FlatBot => AbsValue.Bot
        case FlatElem(ASimple(x)) =>
          optional(AbsValue(Interp.interp(uop, x))).getOrElse(AbsValue.Bot)
        case FlatElem(AMath(x)) =>
          optional(AbsValue(Interp.interp(uop, Math(x))))
            .getOrElse(AbsValue.Bot)
        case FlatElem(_) => AbsValue.Bot
        case FlatTop =>
          uop match
            case Neg   => exploded(s"uop: ($uop $operand)")
            case Not   => AbsValue(bool = !operand.bool)
            case BNot  => exploded(s"uop: ($uop $operand)")
            case Abs   => exploded(s"uop: ($uop $operand)")
            case Floor => exploded(s"uop: ($uop $operand)")

    /** transfer function for binary operators */
    def transfer(
      st: AbsState,
      bop: BOp,
      left: AbsValue,
      right: AbsValue,
    ): AbsValue =
      import BOp.*
      (left.getSingle, right.getSingle) match {
        case (FlatBot, _) | (_, FlatBot) => AbsValue.Bot
        case (FlatElem(ASimple(l)), FlatElem(ASimple(r))) =>
          optional(AbsValue(Interp.interp(bop, l, r))).getOrElse(AbsValue.Bot)
        case (FlatElem(AMath(l)), FlatElem(AMath(r))) =>
          optional(AbsValue(Interp.interp(bop, Math(l), Math(r))))
            .getOrElse(AbsValue.Bot)
        case (FlatElem(lloc: Loc), FlatElem(rloc: Loc))
            if bop == Eq || bop == Equal =>
          if (lloc == rloc) {
            if (st.isSingle(lloc)) AVT
            else AVB
          } else AVF
        case (FlatElem(l), FlatElem(r)) if bop == Eq || bop == Equal =>
          AbsValue(l == r)
        case _ =>
          bop match {
            case BAnd   => exploded(s"bop: ($bop $left $right)")
            case BOr    => exploded(s"bop: ($bop $left $right)")
            case BXOr   => exploded(s"bop: ($bop $left $right)")
            case Div    => exploded(s"bop: ($bop $left $right)")
            case Eq     => AbsValue(bool = left =^= right)
            case Equal  => exploded(s"bop: ($bop $left $right)")
            case LShift => exploded(s"bop: ($bop $left $right)")
            case Lt     =>
              // println(cp.func)
              exploded(s"bop: ($bop $left $right)")
            case Mod => exploded(s"bop: ($bop $left $right)")
            case Mul => exploded(s"bop: ($bop $left $right)")
            // TODO
            // AbsValue(
            //   num = (
            //     (left.num mul right.num) ⊔
            //       (right.num mulInt left.int) ⊔
            //       (left.num mulInt right.int)
            //   ),
            //   int = left.int mul right.int,
            //   bigint = left.bigint mul right.bigint,
            // )
            case And => exploded(s"bop: ($bop $left $right)")
            // TODO
            // AbsValue(bool = left.bool && right.bool)
            case Or   => AbsValue(bool = left.bool || right.bool)
            case Plus => exploded(s"bop: ($bop $left $right)")
            // TODO
            // AbsValue(
            //   str = (
            //     (left.str plus right.str) ⊔
            //       (left.str plusNum right.num)
            //   ),
            //   num = (
            //     (left.num plus right.num) ⊔
            //       (right.num plusInt left.int) ⊔
            //       (left.num plusInt right.int)
            //   ),
            //   int = left.int plus right.int,
            //   bigint = left.bigint plus right.bigint,
            // )
            case Pow     => exploded(s"bop: ($bop $left $right)")
            case SRShift => exploded(s"bop: ($bop $left $right)")
            case Sub     => exploded(s"bop: ($bop $left $right)")
            case UMod    => exploded(s"bop: ($bop $left $right)")
            case URShift => exploded(s"bop: ($bop $left $right)")
            case Xor     => exploded(s"bop: ($bop $left $right)")
          }
      }

    /** transfer for variadic operators */
    def transfer(vop: VOp, vs: List[AbsValue]): AbsValue =
      import VOp.*

      // helpers
      def asMath(av: AbsValue): Option[BigDecimal] = av.getSingle match
        case FlatTop            => exploded("vop transfer")
        case FlatElem(AMath(n)) => Some(n)
        case _                  => None
      def asStr(av: AbsValue): Option[String] = av.getSingle match
        case FlatTop                   => exploded("vop transfer")
        case FlatElem(ASimple(Str(s))) => Some(s)
        case FlatElem(ACodeUnit(cu))   => Some(cu.toString)
        case _                         => None

      // transfer body
      if (vs.exists(_.isBottom)) AbsValue.Bot
      vop match
        case Min =>
          val set = scala.collection.mutable.Set[AbsValue]()
          if (vs.exists(AbsValue(NEG_INF) ⊑ _)) set += AbsValue(NEG_INF)
          val filtered = vs.filter((v) => !(AbsValue(POS_INF) ⊑ v))
          if (filtered.isEmpty) set += AbsValue(POS_INF)
          set += vopInterp(asMath, _ min _, AbsValue.apply, filtered)
          set.foldLeft(AbsValue.Bot)(_ ⊔ _)
        case Max =>
          val set = scala.collection.mutable.Set[AbsValue]()
          if (vs.exists(AbsValue(POS_INF) ⊑ _)) set += AbsValue(POS_INF)
          val filtered = vs.filter((v) => !(AbsValue(NEG_INF) ⊑ v))
          if (filtered.isEmpty) set += AbsValue(NEG_INF)
          set += vopInterp(asMath, _ min _, AbsValue.apply, filtered)
          set.foldLeft(AbsValue.Bot)(_ ⊔ _)
        case Concat => vopInterp[String](asStr, _ + _, AbsValue.apply, vs)

    /** helpers for make transition for variadic operators */
    private def vopInterp[T](
      f: AbsValue => Option[T],
      op: (T, T) => T,
      g: T => AbsValue,
      vs: List[AbsValue],
    ): AbsValue = {
      val vst = vs.map(f).flatten
      if (vst.size != vs.size) AbsValue.Bot
      else g(vst.reduce(op))
    }

    // return specific value
    def doReturn(v: AbsValue): Result[Unit] = for {
      st <- get
      ret = AbsRet(v, st.copied(locals = Map()))
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
}
