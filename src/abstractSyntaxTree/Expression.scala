package abstractSyntaxTree

import abstractSyntaxTree.Operator._

//Every possible expression
trait Expression extends AstNode

case class UnaryOperation(operation : Operator, term : Expression) extends Expression
case class BinaryOperation(first: Expression, operation: Operator, second: Expression) extends Expression
case class CastExpression(typeCast: Type, target: Expression) extends Expression
case class ArrayAccess(array : Expression, index: Expression) extends Expression
case class ArrayCreation(typeName : Type, size: Expression) extends Expression
case class Assignment(leftHandSide: Expression, rightHandSide: Expression) extends Expression
case class FieldAccess(accessed : Expression, field: String) extends Expression
case class ClassCreation(constructor: RefTypeUnlinked, parameters: List[Expression]) extends Expression
case class MethodInvocation(accessed: Option[Expression], method : String, arguments: List[Expression]) extends Expression
case class InstanceOfCall(exp: Expression, typeChecked: Type) extends Expression
case object This extends Expression
case class LocalVariableOrField(str: String) extends Expression
