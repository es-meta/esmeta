package esmeta.util

case class Trie[V](value: Option[V], children: Map[Char, Trie[V]]) {
  def insert(key: String, value: V): Trie[V] = Trie.insert(this, key, value, 0)
  def delete(key: String): Trie[V] = Trie.delete(this, key, 0)
  def search(key: String): Option[V] = Trie.search(this, key, 0)
  def collectPath(key: String): Set[String] =
    Trie.collectPath(this, key, "", 0)
}

object Trie {
  def empty[V]: Trie[V] = new Trie[V](None, Map.empty[Char, Trie[V]])
  def apply[V]: Trie[V] = empty[V]

  private def search[V](node: Trie[V], key: String, step: Int): Option[V] =
    if (key.length == step) node.value
    else
      node.children.get(key.charAt(step)) match
        case Some(nextItem) => search(nextItem, key, step + 1)
        case None           => None

  private def insert[V](
    node: Trie[V],
    key: String,
    value: V,
    step: Int,
  ): Trie[V] =
    if (key.length == step) node.copy(value = Some(value))
    else
      val char = key.charAt(step)
      val nextItem = node.children.getOrElse(char, Trie.empty[V])
      val newNode = insert(nextItem, key, value, step + 1)

      node.copy(children = node.children + (char -> newNode))

  private def delete[V](node: Trie[V], key: String, step: Int): Trie[V] =
    if (key.length == step) node.copy(value = None)
    else
      val char = key.charAt(step)
      node.children.get(char) match
        case None => node
        case Some(nextItem) =>
          val newNode = delete(nextItem, key, step + 1)
          if (newNode.value.isEmpty && newNode.children.isEmpty)
            node.copy(children = node.children - char)
          else
            node.copy(children = node.children + (char -> newNode))

  private def collectPath[V](
    node: Trie[V],
    key: String,
    current: String,
    step: Int,
  ): Set[String] = {
    if (key.length == step)
      if (node.value.isDefined) Set(current) else Set.empty
    else
      node.children.get(key.charAt(step)) match
        case Some(nextItem) =>
          val newCurrent = current + key.charAt(step)
          val resultSet =
            if (nextItem.value.isDefined) Set(newCurrent) else Set.empty
          resultSet ++ collectPath(nextItem, key, newCurrent, step + 1)
        case None => Set.empty
  }
}

/* Driver function */
@main def test() =
  val tokens = List("*", "+", "?", "{", ",", ",}") // Sample tokens
  val trie = tokens.zipWithIndex.foldLeft(Trie[Int]) { (cur, item) =>
    val (token, index) = item
    cur.insert(token, index)
  }

  for (x <- trie.collectPath(",}"))
    println(x) // Output should be List(",", ",}")
