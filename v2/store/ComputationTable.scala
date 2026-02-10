package dbms.v2.store

import dbms.v2.misc.*
import expressions.{Expression, referencedVariables, evaluate}


/** A [[Table]] with the ability to evaluate expressions.
 *
 *  @param schema the schema of the table
 */
class ComputationTable(schema: Schema) extends Table(schema) {

  def this(schema: Schema, initialRecords: Iterable[TableRecord]) = {
    this(schema)
    initialRecords.foreach((r: TableRecord) => appendRecord(r))
  }

  /** Returns a sequence containing the result of evaluating the specified computation for each row of this [[Table]].
   *
   * @param computation the computation to run for each row
   * @throws IllegalArgumentException if the computation references an unknown column
   * @throws IllegalArgumentException if the computation references a non-numeric column
   */
  def compute(computation: Expression): Seq[Double] = {
    // Step 1: Get all variables referenced in the expression
    val variables = referencedVariables(computation)
    
    // Step 2: Validate that all referenced variables are columns in the schema
    variables.foreach { varName =>
      if (!schema.contains(varName))
        throw new IllegalArgumentException(s"Unknown column: $varName")
    }
    
    // Step 3: Validate that all referenced columns are numeric (Long or Double)
    variables.foreach { varName =>
      schema.getDataType(varName) match {
        case DBType.Long | DBType.Double => // OK, numeric
        case DBType.String => 
          throw new IllegalArgumentException(s"Non-numeric column: $varName")
      }
    }
    
    // Step 4: Evaluate the expression for each record
    records.map { record =>
      // Build a Map[String, Double] from the record's attributes
      val variableBindings: Map[String, Double] = variables.map { varName =>
        val value = record.getValue(varName)
        val doubleValue = value match {
          case LongType(l) => l.toDouble
          case DoubleType(d) => d
          case StringType(_) => 
            throw new IllegalArgumentException(s"Cannot convert string to double")
        }
        varName -> doubleValue
      }.toMap
      
      // Evaluate the expression with these bindings
      evaluate(computation, variableBindings)
    }.toSeq
  }

}
