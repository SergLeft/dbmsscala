package dbms.v2.indexing

import datastructures.UnbalancedSearchTree
import dbms.v2.misc.{DBType, RecordID, Variant}
import dbms.v2.store.Table


// use this Ordering for Variant
private val variantOrdering: Ordering[Variant] = Ordering.fromLessThan((l: Variant, r: Variant) => l < r)

/** An index that uses an UnbalancedSearchTree as its backing data structure.
 *  
 *  Unlike HashIndex and TreeIndex which use mutable maps, this index
 *  uses the custom UnbalancedSearchTree data structure.
 *  
 *  @param table the table on which the index is built
 *  @param attribute the name of the attribute to build the index on
 */
class UnbalancedTreeIndex(table: Table, attribute: String) extends IsIndex {
  
  /** The internal unbalanced search tree storing (key -> list of record IDs). */
  private val index: UnbalancedSearchTree[Variant, Seq[RecordID]] = {
    // Build the tree from the table data
    val tree = UnbalancedSearchTree[Variant, Seq[RecordID]](variantOrdering)
    
    // Group record IDs by their attribute value and add to tree
    val groupedRecords: Map[Variant, Seq[RecordID]] = 
      (0 until table.numRecords)
        .groupBy(recordID => table.getRecord(recordID).getValue(attribute))
    
    groupedRecords.foreach { case (key, recordIDs) =>
      tree.addOrUpdate(key, recordIDs)
    }
    
    tree
  }
  
  /** The data type stored in this index. */
  override def dataType: DBType = table.schema.getDataType(attribute)
  
  /** Returns the number of distinct keys currently indexed. */
  override def numEntries: Int = index.size
  
  /** Adds a key and a recordID to the index. */
  override def add(key: Variant, recordID: RecordID): Unit = {
    val currentRecordIDs = index.get(key).getOrElse(Seq())
    val updatedRecordIDs = currentRecordIDs.appended(recordID)
    index.addOrUpdate(key, updatedRecordIDs)
  }
  
  /** Clears the index from all elements. */
  override def clear(): Unit = index.clear()
  
  /** Returns all recordIDs associated with the given key. */
  override def get(key: Variant): Seq[RecordID] = {
    if (this.dataType != key.dataType)
      throw new IllegalArgumentException("The datatype of the passed key differs from the datatype of the index.")
    
    index.get(key).getOrElse(Seq())
  }
  
  /** Returns a string representation of this index. */
  override def toString: String = s"UnbalancedTreeIndex(entries=$numEntries)"
}
