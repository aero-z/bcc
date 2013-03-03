package abstractSyntaxTree

import abstractSyntaxTree.Operator._

//Every possible expression
trait Expression extends AstNode

case class UnaryOperation(operation : Operator, term : Expression) extends Expression {
  val children = term :: Nil
}
case class BinaryOperation(first: Expression, operation: Operator, second: Expression) extends Expression {
  val children = first :: second :: Nil
}
case class CastExpression(typeCast: Type, target: Expression) extends Expression {
  val children = typeCast :: target :: Nil
}
case class ArrayAccess(array : Expression, index: Expression) extends Expression {
  val children = array :: index :: Nil
}
case class ArrayCreation(typeName : Type, size: Expression) extends Expression {
  val children = typeName :: size :: Nil
}
case class Assignment(leftHandSide: Expression, rightHandSide: Expression) extends Expression {
  val children = leftHandSide :: rightHandSide :: Nil
}
case class FieldAccess(accessed : Expression, field: String) extends Expression {
  val children = accessed :: Nil
}
case class ClassCreation(constructor: RefTypeUnlinked, parameters: List[Expression]) extends Expression {
  val children = constructor :: parameters
}
case class MethodInvocation(accessed: Option[Expression], method : String, arguments: List[Expression]) extends Expression {
  val children = accessed ::: arguments
}
case class InstanceOfCall(exp: Expression, typeChecked: Type) extends Expression {
  val children = exp :: typeChecked :: Nil
}
case object This extends Expression {
  val children = Nil
}
case class VariableAccess(str: String) extends Expression {
  val children = Nil
}
