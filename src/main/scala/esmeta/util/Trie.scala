package esmeta.util

/** A trie is a tree-like data structure whose nodes store the letters of an
  * alphabet. By structuring the nodes in a particular way, words and strings
  * can be retrieved from the structure by traversing down a branch path of the
  * tree.
  *
  * @param exist
  *   whether the current node is the end of a word
  * @param children
  *   the children of the current node
  */
case class Trie(exist: Boolean, children: Map[Char, Trie]) {
  def insert(key: String): Trie = Trie.insert(this, key, 0)
  def delete(key: String): Trie = Trie.delete(this, key, 0)
  def search(key: String): Boolean = Trie.search(this, key, 0)
  def collectPath(key: String): Set[String] =
    Trie.collectPath(this, key, "", 0)
}
object Trie {
  def empty: Trie = new Trie(false, Map.empty[Char, Trie])
  def apply: Trie = empty
  def apply(vs: String*): Trie = apply(vs)
  def apply(vs: Iterable[String]): Trie = vs.foldLeft(empty)(_.insert(_))

  private def search(node: Trie, key: String, step: Int): Boolean =
    if (key.length == step) node.exist
    else
      node.children.get(key.charAt(step)) match
        case Some(nextItem) => search(nextItem, key, step + 1)
        case None           => false

  private def insert(
    node: Trie,
    key: String,
    step: Int,
  ): Trie =
    if (key.length == step) node.copy(exist = true)
    else
      val char = key.charAt(step)
      val nextItem = node.children.getOrElse(char, Trie.empty)
      val newNode = insert(nextItem, key, step + 1)

      node.copy(children = node.children + (char -> newNode))

  private def delete(node: Trie, key: String, step: Int): Trie =
    if (key.length == step) node.copy(exist = false)
    else
      val char = key.charAt(step)
      node.children.get(char) match
        case None => node
        case Some(nextItem) =>
          val newNode = delete(nextItem, key, step + 1)
          if (!newNode.exist && newNode.children.isEmpty)
            node.copy(children = node.children - char)
          else
            node.copy(children = node.children + (char -> newNode))

  private def collectPath(
    node: Trie,
    key: String,
    current: String,
    step: Int,
  ): Set[String] = {
    if (key.length == step)
      if (node.exist) Set(current) else Set.empty
    else
      node.children.get(key.charAt(step)) match
        case Some(nextItem) =>
          val newCurrent = current + key.charAt(step)
          val resultSet =
            if (nextItem.exist) Set(newCurrent) else Set.empty
          resultSet ++ collectPath(nextItem, key, newCurrent, step + 1)
        case None => Set.empty
  }

  import Appender.*
  given trieRule: Rule[Trie] = (app, trie) =>
    app >> "[" >> (if (trie.exist) "T" else "F") >> "] "
    app.wrap(for ((char, subTrie) <- trie.children.toList.sortBy(_._1)) {
      app :> char >> ": " >> subTrie
    })
}
