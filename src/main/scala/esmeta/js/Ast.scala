package esmeta.js

/** abstract syntax tree (AST) values */
sealed trait Ast extends JsElem { val name: String }

// AST constructed by syntatic productions
case class Syntactic(
  name: String,
  args: List[Boolean],
  rhsIdx: Int,
  bits: Int,
  children: List[Ast],
) extends Ast

// AST constructed by lexical productions
case class Lexical(
  name: String,
  str: String,
) extends Ast
