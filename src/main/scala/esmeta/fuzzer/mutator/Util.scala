package esmeta.fuzzer.mutator

import esmeta.es.*
import esmeta.es.util.*
import esmeta.es.util.Coverage.*
import esmeta.spec.Grammar
import esmeta.util.*

object Util {
  class AstCounter(pred: Ast => Boolean) extends UnitWalker {
    def apply(ast: Ast): Int = {
      _cnt = 0
      walk(ast)
      _cnt
    }
    private var _cnt = 0

    override def walk(ast: Ast): Unit = {
      if pred(ast) then _cnt += 1
      super.walk(ast)
    }
  }
  val simpleAstCounter = new AstCounter(_ => true)

  trait ListWalker {
    def walkOpt(opt: Option[Ast]): List[Option[Ast]] = opt match {
      case None      => List(None)
      case Some(ast) => walk(ast).map(ast => Some(ast))
    }
    def walk(ast: Ast): List[Ast] = ast match
      case ast: Lexical   => walk(ast)
      case ast: Syntactic => walk(ast)
      case ast: Hole      => walk(ast)
    def walk(ast: Lexical): List[Lexical] = List(ast)
    def walk(ast: Syntactic): List[Syntactic]
    def walk(ast: Hole): List[Hole] = List(ast)
  }

  private type Childrens = List[Vector[Option[Ast]]]

  // should be used carefully because this can explode the size of created program so easily
  trait MultiplicativeListWalker extends ListWalker {
    def preChild(ast: Syntactic, i: Int): Unit = ()
    def postChild(ast: Syntactic, i: Int): Unit = ()

    def walk(ast: Syntactic): List[Syntactic] =
      val Syntactic(name, args, rhsIdx, children) = ast
      val newChildrens = children.zipWithIndex
        .map((childOpt, i) => {
          preChild(ast, i)
          val result = walkOpt(childOpt)
          postChild(ast, i)
          result
        })
        .foldLeft[Childrens](List(Vector()))((childrens, childs) => {
          for {
            childrens <- childrens
            child <- childs
          } yield (childrens :+ child)
        })
      newChildrens.map(newChildren =>
        Syntactic(name, args, rhsIdx, newChildren),
      )

    // Calculate the most efficient parameter for the multiplicative calculator.
    // n: number of mutants to make
    // k: number of candidate to make change
    // ->
    // (k1, c1): Make c1 mutants for k1 locations.
    // (k2, c2): Make c2 mutants for k2 locations.
    // Spec: c1 ^ k1 * c2 ^ k2 >= n, k1 + k2 == k
    def calcParam(n: Int, k: Int): ((Int, Int), (Int, Int)) =
      var c = 2
      while (math.pow(c, k) < n)
        c = c + 1
      var k1 = 0
      var c1 = c - 1
      var k2 = k
      var c2 = c
      while (math.pow((c - 1), k1 + 1) * math.pow(c, k2 - 1) >= n)
        k1 = k1 + 1
        k2 = k2 - 1
      ((k1, c1), (k2, c2))
  }

  trait AdditiveListWalker extends ListWalker {
    def walk(ast: Syntactic): List[Syntactic] =
      val Syntactic(name, args, rhsIdx, children) = ast
      // pair of processed childrens and unprocessed childrens
      val initStat: (Childrens, Childrens) = (List(), List(Vector()))
      val newStat = children
        .foldLeft[(Childrens, Childrens)](initStat)((stat, child) => {
          val (done, yet) = stat
          val done1 = done.map(_ :+ child)
          val done2: Childrens = for {
            child <- walkOpt(child)
            children <- yet
          } yield (children :+ child)
          (done1 ++ done2, yet.map(_ :+ child))
        })
      newStat._1.map(newChildren => Syntactic(name, args, rhsIdx, newChildren))
  }

}
