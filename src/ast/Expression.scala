package ast

import ast.Operator._
import scanner.IntegerToken
import typecheck.TypeCheckingError
import typecheck.TypeCheckingError
import typecheck.TypeChecker

//Every possible expression
trait Expression extends AstNode {
  /**
   * get type of expression AND check for type errors
   */
  def getType(implicit cus: List[CompilationUnit]): Type
}

trait LinkedExpression extends Expression

case class UnaryOperation(operation: Operator, term: Expression) extends Expression {
  def getType(implicit cus: List[CompilationUnit]): Type = {
    (operation, term.getType) match {
      case (InverseOperator, BooleanType) => BooleanType
      case (MinusOperator, t:IntegerTrait) => t
      case (op,t) => throw new TypeCheckingError(s"invalid unary operation ($op, $t)")
    }
  }
}

case class BinaryOperation(first: Expression, operation: Operator, second: Expression) extends Expression {
  val Str = RefTypeLinked(Some(Name("java" :: "lang" :: Nil)), "String")
  def getType(implicit cus: List[CompilationUnit]): Type = (first.getType, operation, second.getType) match {
    case (t,   PlusOperator, Str) => if (t != VoidType) Str else throw new TypeCheckingError("void cannot be converted to String!")
    case (Str, PlusOperator, t)   => if (t != VoidType) Str else throw new TypeCheckingError("void cannot be converted to String!")
    case (_: ByteTrait,    _: ArithmeticOperator, _: ByteTrait)    => ByteType
    case (_: ByteTrait,    _: CompareOperator,    _: ByteTrait)    => BooleanType
    case (_: ShortTrait,   _: ArithmeticOperator, _: ShortTrait)   => ShortType //includes widening!
    case (_: ShortTrait,   _: CompareOperator,    _: ShortTrait)   => BooleanType
    case (_: CharTrait,    _: ArithmeticOperator, _: CharTrait)    => CharType
    case (_: CharTrait,    _: CompareOperator,    _: CharTrait)    => BooleanType
    case (_: IntegerTrait, _: ArithmeticOperator, _: IntegerTrait) => IntType //includes widening!
    case (_: IntegerTrait, _: CompareOperator,    _: IntegerTrait) => BooleanType

    case (BooleanType,     _: BooleanOperator,       BooleanType) => BooleanType

    case (_, EqualOperator | NotEqualOperator, _) if (first.getType == second.getType && first.getType != VoidType) => BooleanType

    case (_: RefType,   EqualOperator | NotEqualOperator,    NullType)  => BooleanType
    case (   NullType,  EqualOperator | NotEqualOperator, _: RefType)   => BooleanType
    case (_: ArrayType, EqualOperator | NotEqualOperator,    NullType)  => BooleanType
    case (   NullType,  EqualOperator | NotEqualOperator, _: ArrayType) => BooleanType
    case (   NullType,  EqualOperator | NotEqualOperator,    NullType)  => BooleanType

    case (x, op, y) => throw new TypeCheckingError(s"no operation $op found for arguments $x and $y")
  }
}

case class CastExpression(typeCast: Type, target: Expression) extends Expression {
  def getType(implicit cus: List[CompilationUnit]): Type = (typeCast, target.getType) match {
    case (x, y) if (TypeChecker.checkTypeMatch(x, y)) => typeCast
    case (x, y) if (TypeChecker.checkTypeMatch(y, x)) => typeCast
    case _ => throw new TypeCheckingError("impossile cast: ("+typeCast+") "+target.getType)
  }
}

case class ArrayAccess(array: Expression, index: Expression) extends Expression {
  def getType(implicit cus: List[CompilationUnit]): Type = (array.getType, index.getType) match {
    case (ArrayType(e), _: IntegerTrait) => e
    case (at, it) => throw new TypeCheckingError(s"type error in array access $at[$it]")
  }
}

case class ArrayCreation(typeName: Type, size: Expression) extends Expression {
  def getType(implicit cus: List[CompilationUnit]): Type = typeName
}

case class Assignment(leftHandSide: Expression, rightHandSide: Expression) extends Expression {
  def getType(implicit cus: List[CompilationUnit]): Type =
    if (!TypeChecker.checkTypeMatch(leftHandSide.getType, rightHandSide.getType)) throw new TypeCheckingError(s"assignment: types don't match (expected ${leftHandSide.getType}, found ${rightHandSide.getType})\nLHS expression: $leftHandSide\nRHS expression: $rightHandSide")
    else leftHandSide.getType
}
case class FieldAccess(accessed: Expression, field: String) extends Expression {
  def getType(implicit cus: List[CompilationUnit]): Type = accessed.getType match {
    case r: RefType => Util.findField(r, field)
    case a: ArrayType if (field == "length") => IntType
    case x => throw new TypeCheckingError(s"trying access member of non-reference type ($x)")
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
                case None => throw new TypeCheckingError("no matching method found")
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
                case None => throw new TypeCheckingError("no matching method found")
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
case class ThisMethodInvocation(thisType: RefType, method: String, arguments: List[Expression]) extends Expression {
  def getType(implicit cus: List[CompilationUnit]): Type = Util.findMethod(thisType, method, arguments)
}

/**
 * (expression).method()
 */
case class ExprMethodInvocation(accessed: Expression, method: String, arguments: List[Expression]) extends Expression {
  def getType(implicit cus: List[CompilationUnit]): Type = accessed.getType match {
    case r: RefType => Util.findMethod(r, method, arguments)
    case x => throw new TypeCheckingError(s"trying access member of non-reference type ($accessed of type $x)")
  }
}

case class InstanceOfCall(exp: Expression, typeChecked: Type) extends Expression {
  def getType(implicit cus: List[CompilationUnit]): Type = (exp.getType, typeChecked) match {
      
    case (p:PrimitiveType, _) => throw new TypeCheckingError("instanceof incompatible with primitive type: "+p)
    case (_, p:PrimitiveType) => throw new TypeCheckingError("instanceof incompatible with primitive type: "+p)
    case (_, NullType|VoidType) => throw new TypeCheckingError("cannot cast to void or null")

    case (x, y) if (TypeChecker.checkTypeMatch(x, y)) => BooleanType
    case (x, y) if (TypeChecker.checkTypeMatch(y, x)) => BooleanType
    case _ => throw new TypeCheckingError("Cannot instance of with:"+exp.getType+" and "+typeChecked)
  }
}

case class This(thisType: RefType) extends Expression {
  def getType(implicit cus: List[CompilationUnit]): Type = thisType
}

case class VariableAccess(str: String) extends Expression {
  def getType(implicit cus: List[CompilationUnit]): Type = sys.error(s"getType is not supposed to be called on type VariableAccess ($str)")
}
