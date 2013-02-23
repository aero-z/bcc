package abstractSyntaxTree

import abstractSyntaxTree.Operator._

//Every possible expression
abstract class Expression //TODO some stuff

case class UnaryOperation(operation : Operator, term : Expression) extends Expression
case class BinaryOperation(first: Expression, operation: Operator, second: Expression) extends Expression
case class CastExpression(typeCast: Type, toAnArray: Boolean, target: Expression) extends Expression
case class ArrayAccess(arrayId : Expression, place: Expression) extends Expression
case class ArrayCreation(typeName : Type, size: Expression) extends Expression
case class Assignment(leftHandSide: Expression, rightHandSide: Expression) extends Expression
case class ClassCreation(constructor: RefType, parameters: List[Expression]) extends Expression
case class FieldAccess(accessed : Expression, field: String) extends Expression
case class MethodInvocation(accessed: Option[Expression], method : String, arguments: List[Expression])
