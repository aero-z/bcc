package ast

import ast.Operator._
import scanner.IntegerToken
import Util._
import main.CompilerError

//Every possible expression
trait Expression extends AstNode{
  /**
   * get type of expression AND check for type errors
   */
  def getType(implicit cus: List[CompilationUnit]): Type
}

trait LinkedExpression extends Expression


case class UnaryOperation(operation : Operator, term : Expression) extends Expression {
  def getType(implicit cus: List[CompilationUnit]): Type = term.getType
}
case class BinaryOperation(first: Expression, operation: Operator, second: Expression) extends Expression {
  def getType(implicit cus: List[CompilationUnit]): Type = ???
}

case class CastExpression(typeCast: Type, target: Expression) extends Expression {
  def getType(implicit cus: List[CompilationUnit]): Type = typeCast
}

case class ArrayAccess(array : Expression, index: Expression) extends Expression {
  def getType(implicit cus: List[CompilationUnit]): Type = array.getType.asInstanceOf[ArrayType].elementType
}

case class ArrayCreation(typeName : Type, size: Expression) extends Expression {
  def getType(implicit cus: List[CompilationUnit]): Type = ArrayType(typeName)
}

case class Assignment(leftHandSide: Expression, rightHandSide: Expression) extends Expression {
  def getType(implicit cus: List[CompilationUnit]): Type =
    if (leftHandSide.getType != rightHandSide.getType && rightHandSide.getType != NullType) throw CompilerError("types don't match")
    else leftHandSide.getType
}
case class FieldAccess(accessed : Expression, field: String) extends Expression {
  def getType(implicit cus: List[CompilationUnit]): Type = accessed.getType match {
    case r:RefType => Util.findField(r, field)
    case _ => throw CompilerError("trying access member of non-reference type")
  }
}

case class ClassCreation(constructor: RefType, arguments: List[Expression]) extends Expression {
  def getType(implicit cus: List[CompilationUnit]): Type = constructor
}

private object Util {
  def findField(refType: RefType, name: String)(implicit cus: List[CompilationUnit]): Type = {
    refType match {
      case t: RefTypeLinked => 
        t.getTypeDef match {
          case ClassDefinition(_, parent, _, _, fields, _, _) =>
            fields.find(_.fieldName == name) match {
              case None => parent match {
                case Some(parentType) => findField(parentType, name)
                case None => throw CompilerError("no matching method found")
              }
              case Some(field) => field.fieldType
            }
           case _ => sys.error("type linking did something bad")
        }
    }
  }
  def findMethod(refType: RefType, name: String, arguments: List[Expression])(implicit cus: List[CompilationUnit]): Type = {
    def compParams(params: List[Parameter], arguments: List[Expression]) = {
      true
    }
    refType match {
      case t: RefTypeLinked => 
        t.getTypeDef match {
          case ClassDefinition(_, parent, _, _, _, _, methods) =>
            val matchingMethods = methods.filter(_ match {
              case MethodDeclaration(mname, _, _, params, _) =>
                (name == mname) && compParams(params, arguments)
            })
            matchingMethods match {
              case Nil => parent match {
                case Some(parentType) => findMethod(parentType, name, arguments)
                case None => throw CompilerError("no matching method found")
              }
              case m => matchingMethods.head.returnType
            }
          case _ => sys.error("type linking did something bad")
        }
      case _ => sys.error("type linking did something bad")
    }
  }
}

/**
 * method()
 */
case class ThisMethodInvocation(thisType: RefType, method : String, arguments: List[Expression]) extends Expression {
  def getType(implicit cus: List[CompilationUnit]): Type = Util.findMethod(thisType, method, arguments)
}

/**
 * (expression).method()
 */
case class ExprMethodInvocation(accessed: Expression, method : String, arguments: List[Expression]) extends Expression {
  def getType(implicit cus: List[CompilationUnit]): Type = accessed.getType match {
    case r:RefType => Util.findMethod(r, method, arguments)
    case _ => throw CompilerError("trying access member of non-reference type")
  }
}

case class InstanceOfCall(exp: Expression, typeChecked: Type) extends Expression {
  def getType(implicit cus: List[CompilationUnit]): Type = BooleanType
}

case class This(thisType: RefType) extends Expression {
  def getType(implicit cus: List[CompilationUnit]): Type = thisType
}

case class VariableAccess(str: String) extends Expression {
  def getType(implicit cus: List[CompilationUnit]): Type = sys.error("getType is not supposed to be called on type VariableAccess")
}
