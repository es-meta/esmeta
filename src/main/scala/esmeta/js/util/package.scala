package esmeta.js.util

import esmeta.js.*

/** extensions for functions */
extension (ast: Ast) {

  /** flatten statements */
  // TODO refactoring
  def flattenStmtList(
    s: Ast,
    list: List[Ast] = Nil,
  ): List[Ast] = s match
    case Syntactic("StatementList", _, 0, List(Some(x0))) =>
      x0 :: list
    case Syntactic("StatementList", _, 1, List(Some(x0), Some(x1))) =>
      flattenStmtList(x0, x1 :: list)
    case _ => Nil
  def flattenStmt: List[Ast] = ast match
    case Syntactic(
          "Script",
          _,
          0,
          List(Some(Syntactic("ScriptBody", _, 0, List(Some(stlist))))),
        ) =>
      flattenStmtList(stlist)
    case _ => Nil

  /** merge statements to script */
  // TODO refactoring
  def mergeStmt(l: List[Ast]): Ast =
    val params = List(false, false, false)
    val bodyOpt = l match
      case a :: rest =>
        val init: Ast = Syntactic("StatementList", params, 0, List(Some(a)))
        val list = rest.foldLeft(init) {
          case (x, y) =>
            Syntactic("StatementList", Nil, 1, List(Some(x), Some(y)))
        }
        Some(Syntactic("ScriptBody", Nil, 0, List(Some(list))))
      case Nil => None
    Syntactic("Script", Nil, 0, List(bodyOpt))
}
