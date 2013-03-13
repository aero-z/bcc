package ast

import ast.Operator._
import scanner.IntegerToken

//Every possible expression
trait Expression extends AstNode{
  def getType(implicit cus: List[CompilationUnit]): Type
}

trait LinkedExpression extends Expression


case class UnaryOperation(operation : Operator, term : Expression) extends Expression{
  def getType(implicit cus: List[CompilationUnit]): Type = term.getType
}
case class BinaryOperation(first: Expression, operation: Operator, second: Expression) extends Expression{
  def getType(implicit cus: List[CompilationUnit]): Type = ???
}

case class CastExpression(typeCast: Type, target: Expression) extends Expression{
  def getType(implicit cus: List[CompilationUnit]): Type = typeCast
}

case class ArrayAccess(array : Expression, index: Expression) extends Expression{
  def getType(implicit cus: List[CompilationUnit]): Type = array.getType.asInstanceOf[ArrayType].elementType
}

case class ArrayCreation(typeName : Type, size: Expression) extends Expression{
  def getType(implicit cus: List[CompilationUnit]): Type = ArrayType(typeName)
}

case class Assignment(leftHandSide: Expression, rightHandSide: Expression) extends Expression{
  def getType(implicit cus: List[CompilationUnit]): Type = leftHandSide.getType // if(Type.max(leftHandSide.getType, rightHandSide.getType) == leftHandSide.getType) leftHandSide.getType else throw new CompilerError(s"Loss of precision: $leftHandSide from $rightHandSide") //TODO could be good to have an error is such a case...
}
case class FieldAccess(accessed : Expression, field: String) extends Expression{
  def getType(implicit cus: List[CompilationUnit]): Type = sys.error("getType is not supposed to be called on type FieldAccess")
}

case class ClassCreation(constructor: RefType, arguments: List[Expression]) extends Expression{
  def getType(implicit cus: List[CompilationUnit]): Type = constructor
}

private object Util {
  def findMethod(refType: RefType, arguments: List[Expression])(implicit cus: List[CompilationUnit]): Type = {
    def compParams(params: List[Parameter], arguments: List[Expression]) = {
      true
    }
    refType match {
      case t: RefTypeLinked => 
        t.getTypeDef match {
          case ClassDefinition(_, parent, _, _, _, _, methods) =>
            val matchingMethods = methods.filter(_ match {
              case MethodDeclaration(_, _, _, params, _) => compParams(params, arguments)
            })
            matchingMethods match {
              case Nil => parent match {
                case Some(parentType) => findMethod(parentType, arguments)
                case _ => sys.error("hierarchy checking did something bad")
              }
              case m => matchingMethods.head.returnType
            }
          case _ => sys.error("type linking did something bad")
        }
      case _ => sys.error("type linking did something bad")
    }
  }
}
import Util._

/**
 * method()
 */
case class ThisMethodInvocation(thisType: RefType, method : String, arguments: List[Expression]) extends Expression{
  def getType(implicit cus: List[CompilationUnit]): Type = Util.findMethod(thisType, arguments)
}

/**
 * (expression).method()
 */
case class ExprMethodInvocation(accessed: Expression, method : String, arguments: List[Expression]) extends Expression{
  def getType(implicit cus: List[CompilationUnit]): Type = ???
}

case class InstanceOfCall(exp: Expression, typeChecked: Type) extends Expression{
  def getType(implicit cus: List[CompilationUnit]): Type = BooleanType
}

case class This(thisType: RefType) extends Expression{
  def getType(implicit cus: List[CompilationUnit]): Type = thisType
}

case class VariableAccess(str: String) extends Expression{
  def getType(implicit cus: List[CompilationUnit]): Type = sys.error("getType is not supposed to be called on type VariableAccess")
}
