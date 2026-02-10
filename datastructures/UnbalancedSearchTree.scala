package datastructures

import scala.annotation.tailrec

import datastructures.UnbalancedSearchTree.*


object UnbalancedSearchTree {
  private final case class Node[K, V](
    var key: K,
    var value: V,
    var left: Option[Node[K, V]],
    var right: Option[Node[K, V]]
  )

  private def leaf[K, V](key: K, value: V): Node[K, V] = Node(key, value, None, None)

  def apply[K, V](ord: Ordering[K]): UnbalancedSearchTree[K, V] = {
    new UnbalancedSearchTree[K, V](ord)
  }

  def apply[K, V](elements: Iterable[(K, V)])(ord: Ordering[K]): UnbalancedSearchTree[K, V] = {
    val set = new UnbalancedSearchTree[K, V](ord)
    elements.foreach(set.addOrUpdate)
    set
  }
}


class UnbalancedSearchTree[K, V](ord: Ordering[K]) {

  private var root: Option[Node[K, V]] = None

  /** Counts the number of nodes in the subtree rooted at the given node. */
  private def countNodes(node: Option[Node[K, V]]): Int = node match {
    case None => 0
    case Some(n) => 1 + countNodes(n.left) + countNodes(n.right)
  }

  def size: Int = countNodes(root)

  def clear(): Unit = {
    root = None
  }

  /** Adds a new key-value pair or updates the value if key already exists.
   *  
   *  Uses the standard BST insertion algorithm:
   *  - If tree is empty, create new root node
   *  - Otherwise, traverse down: go left if key < current, right if key > current
   *  - If key equals current, update the value
   *  - If we reach a None, insert a new leaf
   */
  def addOrUpdate(key: K, value: V): Unit = {
    root match {
      case None =>
        root = Some(leaf(key, value))
      case Some(rootNode) =>
        addOrUpdateRecursive(rootNode, key, value)
    }
  }

  /** Helper method for addOrUpdate - traverses and inserts/updates. */
  private def addOrUpdateRecursive(node: Node[K, V], key: K, value: V): Unit = {
    val cmp = ord.compare(key, node.key)
    if (cmp < 0) {
      // Key is smaller, go left
      node.left match {
        case None => node.left = Some(leaf(key, value))
        case Some(leftChild) => addOrUpdateRecursive(leftChild, key, value)
      }
    } else if (cmp > 0) {
      // Key is larger, go right
      node.right match {
        case None => node.right = Some(leaf(key, value))
        case Some(rightChild) => addOrUpdateRecursive(rightChild, key, value)
      }
    } else {
      // Key already exists, update the value
      node.value = value
    }
  }

  /** Convenience method to add from a tuple. */
  def addOrUpdate(pair: (K, V)): Unit = addOrUpdate(pair._1, pair._2)

  /** Searches for a key and returns its value if found.
   *  
   *  Uses standard BST search: compare with current node,
   *  go left if smaller, right if larger, return value if equal.
   */
  def get(key: K): Option[V] = {
    @tailrec
    def searchRecursive(node: Option[Node[K, V]]): Option[V] = node match {
      case None => None
      case Some(n) =>
        val cmp = ord.compare(key, n.key)
        if (cmp < 0) searchRecursive(n.left)
        else if (cmp > 0) searchRecursive(n.right)
        else Some(n.value)
    }
    searchRecursive(root)
  }

  /** Returns the value for a key, throws if not found. */
  def apply(key: K): V = get(key).getOrElse(throw new NoSuchElementException(s"key not found: $key"))


}
