package expressions

/** Represents a mathematical expression that can be evaluated.
 *  
 *  This is a sealed trait, meaning all subclasses must be defined in the same file.
 *  This allows exhaustive pattern matching in the evaluate and other functions.
 */
sealed trait Expression

/** Represents a named variable that can be looked up in a variable binding map.
 *  
 *  @param name the name of the variable (e.g., "x", "y", "grade")
 */
case class Variable(name: String) extends Expression

/** Represents a numeric constant value.
 *  
 *  @param value the numeric value of the constant
 */
case class Constant(value: Double) extends Expression

/** Represents a unary operation (operation with one operand).
 *  
 *  @param op the operator (e.g., "-" for negation, "abs" for absolute value, "invert" for 1/x)
 *  @param expr the expression to apply the operation to
 */
case class UnaryOperation(op: String, expr: Expression) extends Expression

/** Represents a binary operation (operation with two operands).
 *  
 *  @param op the operator (e.g., "+" for addition, "*" for multiplication)
 *  @param left the left operand expression
 *  @param right the right operand expression
 */
case class BinaryOperation(op: String, left: Expression, right: Expression) extends Expression
