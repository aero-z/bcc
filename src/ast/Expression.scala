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
    case (_:RefTypeLinked, _:PlusOperator ,Str) => Str
    case (Str, _:PlusOperator, _:RefTypeLinked) => Str
    case (_:RefTypeLinked, x, _:RefTypeLinked) => throw new TypeCheckingError("Two reference types do not support the operation:"+x)
    //primitive types mixed with reference types
    case (_:PrimitiveType, _:PlusOperator ,Str) => Str
    case (Str, _:PlusOperator ,_:PrimitiveType) => Str
    case (_:PrimitiveType, x ,_:RefTypeLinked) => throw new TypeCheckingError("operation:"+x+" not possible between a primitive type and a reference type")
    case (_:RefTypeLinked, x ,_:PrimitiveType) => throw new TypeCheckingError("operation:"+x+" not possible between a primitive type and a reference type")
    //only primitives types -> includes widening
    case (_:ByteTrait, op:ByteOperator  ,_:ByteTrait) => ByteType
    case (_:ByteTrait, op:CompareOperator  ,_:ByteTrait) => BooleanType
    case (_:ShortTrait, op:ShortOperator  ,_:ShortTrait) => ShortType //includes widening!
    case (_:ShortTrait, op:CompareOperator  ,_:ShortTrait) => BooleanType
    case (_:IntegerTrait, op:IntegerOperator  ,_:IntegerTrait) => IntType //includes widening!
    case (_:IntegerTrait, op:CompareOperator  ,_:IntegerTrait) => BooleanType
  }
}
    //TODO: treat other cases
    // T < super of T
    /*
     *  The comparison operators, which result in a value of type boolean:
 The numerical comparison operators <, <=, >, and >= (¤15.20.1)
 The numerical equality operators == and != (¤15.21.1)
 The numerical operators, which result in a value of type int or long:
The unary plus and minus operators + and - (¤15.15.3, ¤15.15.4)
The multiplicative operators *, /, and % (¤15.17)
 The additive operators + and - (¤15.18)
 The increment operator ++, both prefix (¤15.15.1) and postfix (¤15.14.2)
 The decrement operator --, both prefix (¤15.15.2) and postfix (¤15.14.3)
 The signed and unsigned shift operators <<, >>, and >>> (¤15.19)
 The bitwise complement operator ~ (¤15.15.5)
 The integer bitwise operators &, ^, and | (¤15.22.1)
 The conditional operator ? : (¤15.25)
 
    PrimitiveType
   		"int" => IntType
    	"boolean" => BooleanType
    	"byte" => ByteType
    	"short" => ShortType
    	"char" => CharType
    	"void" => VoidType
    ArrayType
    RefType(path:Name)
    	RefTypeUnlinked(path: Name)
    	RefTypeLinked(pkgName: Option[Name], className:String)
    	* 
    	case "+" => plus
        case  "-" => minus
        case "^"=> bitXor
        case "&"=> bitAnd
        case "|"=> bitOr
        case "!"=> comp
        case "*"=> star
        case "/"=> div
        case "%"=> mod
        case "<"=> less
        case ">"=> more
        case "=="=> eq
        case "!="=> neq
        case "&&"=> condAnd
        case "||"=> condOr
        case "<="=> leq
        case ">="=> geq
    */

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
