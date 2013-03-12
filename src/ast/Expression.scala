package ast

import ast.Operator._
import scanner.IntegerToken

//Every possible expression
trait Expression extends AstNode{
  val getType : Type
}

trait LinkedExpression extends Expression


case class UnaryOperation(operation : Operator, term : Expression) extends Expression{
  lazy val getType: Type = term.getType
}
case class BinaryOperation(first: Expression, operation: Operator, second: Expression) extends Expression{
  lazy val getType: Type = ???
}

case class CastExpression(typeCast: Type, target: Expression) extends Expression{
  lazy val getType: Type = typeCast
}

case class ArrayAccess(array : Expression, index: Expression) extends Expression{
  lazy val getType: Type = array.getType.asInstanceOf[ArrayType].elementType
}

case class ArrayCreation(typeName : Type, size: Expression) extends Expression{
  lazy val getType: Type = ArrayType(typeName)
}

case class Assignment(leftHandSide: Expression, rightHandSide: Expression) extends Expression{
  lazy val getType: Type = leftHandSide.getType // if(Type.max(leftHandSide.getType, rightHandSide.getType) == leftHandSide.getType) leftHandSide.getType else throw new CompilerError(s"Loss of precision: $leftHandSide from $rightHandSide") //TODO could be good to have an error is such a case...
}
case class FieldAccess(accessed : Expression, field: String) extends Expression{
  lazy val getType: Type = sys.error("getType is not supposed to be called on type FieldAccess")
}

case class ClassCreation(constructor: RefType, arguments: List[Expression]) extends Expression{
  lazy val getType: Type = constructor
}

/**
 * method()
 */
case class ThisMethodInvocation(thisType: RefType, method : String, arguments: List[Expression]) extends Expression{
  lazy val getType: Type = ???
}

/**
 * (expression).method()
 */
case class ExprMethodInvocation(accessed: Expression, method : String, arguments: List[Expression]) extends Expression{
  lazy val getType: Type = ???
}

case class InstanceOfCall(exp: Expression, typeChecked: Type) extends Expression{
  lazy val getType: Type = BooleanType
}

case class This(thisType: RefType) extends Expression{
  lazy val getType: Type = thisType
}

case class VariableAccess(str: String) extends Expression{
  lazy val getType: Type = sys.error("getType is not supposed to be called on type VariableAccess")
}
