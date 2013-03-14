package ast

import ast.Operator._
import scanner.IntegerToken
//import Util._ //todo uncomment!
import main.CompilerError

class TypeCheckingError(message:String) extends main.CompilerError(message)

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
  val Str = RefTypeLinked(Some(Name("java"::"lang"::Nil)), "String")
  def getType(implicit cus: List[CompilationUnit]): Type = (first.getType, operation, second.getType) match {
    //only reference types
    case (_:RefTypeLinked, PlusOperator ,Str) => Str
    case (Str, PlusOperator, _:RefTypeLinked) => Str
    case (_:RefTypeLinked, x, _:RefTypeLinked) => throw new TypeCheckingError("Two reference types do not support the operation:"+x)
    //primitive types mixed with reference types
    case (_:PrimitiveType, PlusOperator ,Str) => Str
    case (Str, PlusOperator ,_:PrimitiveType) => Str
    case (_:PrimitiveType, x ,_:RefTypeLinked) => throw new TypeCheckingError("operation:"+x+" not possible between a primitive type and a reference type")
    case (_:RefTypeLinked, x ,_:PrimitiveType) => throw new TypeCheckingError("operation:"+x+" not possible between a primitive type and a reference type")
    //only primitives types -> includes widening
    case (_:ByteTrait, op:ArithmeticOperator  ,_:ByteTrait) => ByteType
    case (_:ByteTrait, op:CompareOperator  ,_:ByteTrait) => BooleanType
    case (_:ShortTrait, op:ArithmeticOperator  ,_:ShortTrait) => ShortType //includes widening!
    case (_:ShortTrait, op:CompareOperator  ,_:ShortTrait) => BooleanType
    case (_:IntegerTrait, op:ArithmeticOperator  ,_:IntegerTrait) => IntType //includes widening!
    case (_:IntegerTrait, op:CompareOperator  ,_:IntegerTrait) => BooleanType
    case (BooleanType, op:BooleanOperator, BooleanType) => BooleanType
    case _ => throw new TypeCheckingError("operation:"+operation+" not possible between:"+first+" and "+second)
  }
}
    //TODO: treat other cases
    // T < super of T

case class CastExpression(typeCast: Type, target: Expression) extends Expression {
  def getType(implicit cus: List[CompilationUnit]): Type = (typeCast, target.getType) match {
    case (ByteType, _:ByteTrait) => ByteType
    case (CharType, _:CharTrait) => CharType
    case (ShortType, _:ShortTrait) => ShortType
    case (IntType, _:IntegerTrait) => IntType
    case (BooleanType, BooleanType) => BooleanType
    //all other primitive casts are imposible!
    case (_:PrimitiveType, _:PrimitiveType) => throw new TypeCheckingError("impossile cast: ("+typeCast+") "+target.getType)
    case _ => typeCast//TODO: reference types -> give errors
  }
  //check if cast is possible!
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
  def getType(implicit cus: List[CompilationUnit]): Type = (exp.getType, typeChecked) match {
    case (p:PrimitiveType, _) => throw CompilerError("instanceOf incompatible with primitive type: "+p)
    case (_, p:PrimitiveType) => throw CompilerError("instanceOf incompatible with primitive type: "+p)
    case (typeChecked, _) => typeChecked //instanceof himself
    case (NullType, _) => BooleanType //always false btw
    case (_, NullType) => throw CompilerError("instanceOf impossible with nulltype and ")
    case (VoidType, _) => throw CompilerError("instanceOf: void is not the instance of anything ")
    case (_, VoidType) => throw CompilerError("instanceOf: nothing can be a subtype of void ")
    case (_:ArrayType, _:ArrayType) => BooleanType
    case (parent:RefTypeLinked, child:RefTypeLinked) => BooleanType
    case _ => throw CompilerError("Cannot instanceOf with:"+exp.getType+" and "+typeChecked)
  }
}

case class This(thisType: RefType) extends Expression {
  def getType(implicit cus: List[CompilationUnit]): Type = thisType
}

case class VariableAccess(str: String) extends Expression {
  def getType(implicit cus: List[CompilationUnit]): Type = sys.error("getType is not supposed to be called on type VariableAccess")
}
