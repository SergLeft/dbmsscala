package datastructures

import scala.collection.mutable

class Counter[A](initialElements: Iterable[A]) {

  /** Internal mutable map storing the count for each key.
   *  We use a HashMap for O(1) average access time.
   */
  private val counts: mutable.Map[A, Long] = mutable.HashMap()

  // Initialize counts from the initial elements
  incrementCounts(initialElements)

  /** Sets the count for the specified key to the specified value.
    *
    * @throws IllegalArgumentException if the specified count is negative
    */
  def setCount(key: A, count: Long): Unit = {
    if (count < 0)
      throw new IllegalArgumentException("count cannot be negative")
    if (count == 0)
      counts.remove(key)  // Remove zero counts to keep map clean
    else
      counts.update(key, count)
  }

  /** Modifies the count for the specified key by adding the specified amount. Count cannot drop below zero. */
  def modifyCount(key: A, amount: Long): Unit = {
    val currentCount = getCount(key)
    val newCount = math.max(0, currentCount + amount)  // Cannot drop below zero
    setCount(key, newCount)
  }

  /** Returns the count for the specified key. */
  def getCount(key: A): Long = counts.getOrElse(key, 0L)

  /** Increments the count by one for each of the specified keys. */
  def incrementCounts(keys: Iterable[A]): Unit = {
    keys.foreach(key => modifyCount(key, 1))
  }

  /** Decrements the count by one for each of the specified keys. Counts cannot drop below zero. */
  def decrementCounts(keys: Iterable[A]): Unit = {
    keys.foreach(key => modifyCount(key, -1))
  }


  override def toString: String = counts.mkString("Counter(", ", ", ")")

  override def equals(obj: Any): Boolean = obj match {
    case other: Counter[?] => this.counts == other.counts
    case _ => false
  }

  override def hashCode: Int = counts.hashCode
}
