package esmeta.es.util.fuzzer

import scala.annotation.tailrec
import scala.collection.mutable.Map as MMap

// TODO: Add tests for FSTrie

object FSTrie {

  val config = FSTrieConfig()

  def root[K]: FSTrie[K] = new FSTrie[K](status = FSTrieStatus.Noticed)

}

/** A trie that stores the status of each node in the trie, and calculates the
  * score of each node based on the hits and misses of the node and its
  * children. The score is used to determine the promotion or demotion of a
  * node. The fields are mutable for (maybe) better performance than using
  * immutable case classes and copying.
  *
  * @param children
  * @param status
  * @param hits
  * @param misses
  * @param promotables
  *   number of Promotable descendants of this node.
  * @param avgScore
  *   average score of its descendants which are Promotable. If this node is
  *   Promotable, the score is calculated based on the hits and misses of this
  *   node.
  * @param avgScoreSq
  *   average of the square of the scores of its descendants which are
  *   Promotable If this node is Promotable, the scoreSquared is calculated
  *   based on the hits and misses of this node. This is used to calculate the
  *   standard deviation of the scores
  */
class FSTrie[K](
  private val children: MMap[K, FSTrie[K]] = MMap.empty[K, FSTrie[K]],
  private var status: FSTrieStatus,
  private var hits: Int = 0,
  private var misses: Int = 0,
  private var dirty: Boolean = false,
  private var promotables: Int = 0,
  private var avgScore: Double = 0,
  private var avgScoreSq: Double = 0,
) {
  import FSTrieStatus.*
  import FSTrie.config.*

  /** How many features do we need to take from the stack?
    *
    * @param stack
    *   the stack of features' names
    * @return
    *   the number of features we need to take
    */
  def apply(stack: List[K]): Int =
    math.min(
      foldByStack(stack, 0) {
        case (acc, node) =>
          if node.status == Noticed then acc + 1
          else acc
      },
      maxSensitivity,
    )

  /** Insert feature stacks from a single script into the trie. The script
    * succeeded to invoke some non-trivial minifier operations. Increment the
    * hits of each node in the trie.
    *
    * @param stacks
    *   the feature stacks generated from the successful script
    */
  def touchWithHit(stacks: Iterable[List[K]]): Unit =
    stacks.foreach(touchByStack(_, isHit = true))
    writeback()
    updateStatus()

  /** Insert feature stacks from a single script into the trie. The script
    * failed to invoke some non-trivial minifier operations. Increment the
    * misses of each node in the trie.
    *
    * @param stacks
    *   the feature stacks generated from the failed script
    */
  def touchWithMiss(stacks: Iterable[List[K]]): Unit =
    stacks.foreach(touchByStack(_, isHit = false))
    writeback()
    updateStatus()

  /** Insert a feature stack into the trie. Increment the hits or misses of each
    * node in the trie based on whether the node is hit or miss.
    *
    * @param stack
    *   the stack of features' names
    * @param isHit
    *   whether the node is hit or miss
    */
  @tailrec
  private final def touchByStack(stack: List[K], isHit: Boolean): Unit =
    if isHit then hits += 1 else misses += 1
    dirty = true
    stack match {
      case Nil =>
      case head :: tail =>
        children.get(head) match {
          case Some(child) => child.touchByStack(tail, isHit)
          case None =>
            val child = new FSTrie[K](
              MMap.empty[K, FSTrie[K]],
              status match {
                case Noticed => Promotable
                case _       => Ignored
              },
            )
            children(head) = child
            child.touchByStack(tail, isHit)
        }
    }

  /** Write back the scores and number of promotables of each node if it's dirty
    */
  private def writeback(): Unit = foreachFromLeaf { node =>
    // write back the if it's dirty
    if node.dirty then
      node.dirty = false
      node.status match {
        case Promotable => // calculate the score based on hits and misses
          node.avgScore = scoringFunction(node.hits, node.misses)
          node.avgScoreSq = node.avgScore * node.avgScore
          node.promotables = 1
        case Noticed => // calculate the average score and number of the node's promotables
          node.promotables = node.children.valuesIterator
            .map(child => child.promotables)
            .sum
          node.avgScore = node.children.valuesIterator
            .map(child => child.avgScore * child.promotables)
            .sum / node.promotables
          node.avgScoreSq = node.children.valuesIterator
            .map(child => child.avgScoreSq * child.promotables)
            .sum / node.promotables
        case Ignored => ()
      }
  }

  /** Promote or demote each node in the trie based on the score of this node.
    * Assume that the children have already been written back.
    */
  private def updateStatus(): Unit =
    val promotionScore = avgScore + promotionCriteria * stdev
    val demotionScore = avgScore - demotionCriteria * stdev
    // demote to Ignored from the leaf first
    foreachFromLeaf { node =>
      node.status match {
        case Noticed if node.avgScore < demotionScore =>
          node.status = Ignored
        case _ => node.status
      }
    }
    // promote from Promotable to Noticed and Ignored to Promotable
    foreachFromRoot { node =>
      node.status match {
        case Noticed =>
          children.valuesIterator.foreach { child =>
            child.status match
              case Ignored => child.status = Promotable
              case _       =>
          }
        case Promotable if node.avgScore > promotionScore =>
          node.status = Noticed
          children.valuesIterator.foreach(_.status = Promotable)
        case _ => node.status
      }
    }

  /** Recursively do something to each node in the trie, starting from the root
    *
    * @param op
    *   the operation to be done to each node
    */
  private def foreachFromRoot(
    op: FSTrie[K] => Unit,
    iterIgnored: Boolean = false,
  ): Unit =
    op(this)
    children.valuesIterator.foreach { child =>
      if iterIgnored || child.status != Ignored then
        child.foreachFromRoot(op, iterIgnored)
    }

  /** Recursively do something to each node in the trie, starting from the leaf
    *
    * @param op
    *   the operation to be done to each node
    */
  private def foreachFromLeaf(
    op: FSTrie[K] => Unit,
    iterIgnored: Boolean = false,
  ): Unit =
    children.valuesIterator.foreach { child =>
      if iterIgnored || child.status != Ignored then
        child.foreachFromRoot(op, iterIgnored)
    }
    op(this)

  @tailrec
  private final def iterByStack(stack: List[K])(op: FSTrie[K] => Unit): Unit =
    op(this)
    stack match {
      case Nil =>
      case head :: tail =>
        children.get(head) match {
          case Some(child) => child.iterByStack(tail)(op)
          case None        =>
        }
    }

  @tailrec
  private final def foldByStack[A](stack: List[K], z: A)(
    op: (A, FSTrie[K]) => A,
  ): A =
    stack match {
      case Nil => op(z, this)
      case head :: tail =>
        children.get(head) match {
          case Some(child) => child.foldByStack(tail, op(z, this))(op)
          case None        => z
        }
    }

  private def get(stack: List[K]): Option[FSTrie[K]] =
    foldByStack(stack, Option.empty[FSTrie[K]])((_, node) => Some(node))

  private def touches: Int = hits + misses

  private def stdev: Double = Math.sqrt(avgScoreSq - avgScore * avgScore)
}

/** Configuration for the FSTrie
  *
  * @param scoringFunction
  *   A function that calculates the score of based on hits and misses, used to
  *   determine the promotion or demotion of a node
  * @param promotionCriteria
  *   The number of standard deviations above the mean score calculated by the
  *   scoring function, at which a node is promoted from Promotable to Noticed
  * @param demotionCriteria
  *   The number of standard deviations below the mean score calculated by the
  *   scoring function, at which a node is demoted from Demotable to Ignored
  */
case class FSTrieConfig(
  scoringFunction: (Int, Int) => Double = (hits, misses) =>
    hits.toDouble / (hits + misses),
  promotionCriteria: Int = 3,
  demotionCriteria: Int = 3,
  maxSensitivity: Int = 3,
)

/** Status of a node in the trie
  *   - Noticed: actively noticed; will not be demoted
  *   - Promotable: ignored but might be promoted to Noticed
  *   - Ignored: not noticed; will not be promoted
  */
enum FSTrieStatus:
  case Noticed
  case Promotable
  case Ignored
