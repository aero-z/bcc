package ast

import ast.Operator._
import scanner.IntegerToken
import typecheck.TypeCheckingError
import typecheck.TypeCheckingError
import typecheck.TypeChecker
import nameResolution.PathToField
import com.sun.org.apache.xalan.internal.xsltc.compiler.util.TypeCheckError
import typecheck.TypeCheckingError
import nameResolution.PathToDeclaration

//Every possible expression
trait Expression extends AstNode {
  /**
   * get type of expression AND check for type errors
   */
  def getType(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): Type
}

trait LinkedExpression extends Expression

private object Util {
  def compParams(params: List[Parameter], arguments: List[Expression])(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked) = {
    params.map(_.paramType) == arguments.map(_.getType)
  }
  def findField(refType: RefType, name: String)(implicit cus: List[CompilationUnit]): FieldDeclaration = {
    refType match {
      case t: RefTypeLinked =>
        t.getTypeDef match {
          case ClassDefinition(_, parent, _, _, fields, _, _) =>
            fields.find(_.fieldName == name) match {
              case None => parent match {
                case Some(parentType) => findField(parentType, name)
                case None => throw new TypeCheckingError("no matching method found")
              }
              case Some(field) => field
            }
          case _ => sys.error("type linking did something bad")
        }
    }
  }
  def findMethodThrows(refType: RefType, name: String, arguments: List[Expression])(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): MethodDeclaration = {
    findMethod(refType, name, arguments) match {
      case None => throw new TypeCheckingError("no matching method found")
      case Some(m) => m
    }
  }

  def findMethod(refType: RefType, name: String, arguments: List[Expression])(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): Option[MethodDeclaration] = {
    refType match {
      case t: RefTypeLinked =>
        val (parents, methods) = t.getTypeDef match {
          case ClassDefinition(_, parent, _, _, _, _, methods) => (parent.toList, methods)
          case InterfaceDefinition(_, parents, _, methods) => (parents, methods)
        }
        
        val matchingMethods = methods.filter(_ match {
          case MethodDeclaration(mname, _, _, params, _) =>
            (name == mname) && compParams(params, arguments)
        })
        def findMethodParents(parents: List[RefType]): Option[MethodDeclaration] = {
          parents match {
            case Nil => None
            case p :: ps =>
              findMethod(p, name, arguments) match {
                case Some(m) => Some(m)
                case None => findMethodParents(ps)
              }
          }
        }
        matchingMethods match {
          case Nil => findMethodParents(parents)
          case m => Some(matchingMethods.head)
        }
      case _ => sys.error("type linking did something bad")
    }
  }
}

/**
 * operation term
 * e.g. -5
 */
case class UnaryOperation(operation: Operator, term: Expression) extends Expression {
  def getType(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): Type = {
    (operation, term.getType) match {
      case (InverseOperator, BooleanType) => BooleanType
      case (InverseOperator, _: IntegerTrait) => BooleanType
      case (MinusOperator, t: IntegerTrait) => t
      case (op, t) => throw new TypeCheckingError(s"invalid unary operation ($op, $t)")
    }
  }
}

/**
 * first operation second
 * e.g. 5 + "hey"
 */
case class BinaryOperation(first: Expression, operation: Operator, second: Expression) extends Expression {
  val Str = RefTypeLinked(Some(Name("java" :: "lang" :: Nil)), "String")
  def getType(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): Type =
    (first.getType, operation, second.getType) match {
      case (t, PlusOperator, Str) => if (t != VoidType) Str else throw new TypeCheckingError("void cannot be converted to String!")
      case (Str, PlusOperator, t) => if (t != VoidType) Str else throw new TypeCheckingError("void cannot be converted to String!")
      case (_: ByteTrait, _: ArithmeticOperator, _: ByteTrait) => ByteType
      case (_: ByteTrait, _: CompareOperator, _: ByteTrait) => BooleanType
      case (_: ShortTrait, _: ArithmeticOperator, _: ShortTrait) => ShortType //includes widening!
      case (_: ShortTrait, _: CompareOperator, _: ShortTrait) => BooleanType
      case (_: CharTrait, _: ArithmeticOperator, _: CharTrait) => CharType
      case (_: CharTrait, _: CompareOperator, _: CharTrait) => BooleanType
      case (_: IntegerTrait, _: ArithmeticOperator, _: IntegerTrait) => IntType //includes widening!
      case (_: IntegerTrait, _: CompareOperator, _: IntegerTrait) => BooleanType

      case (BooleanType, _: BooleanOperator, BooleanType) => BooleanType

      //case (_, EqualOperator | NotEqualOperator, _) if (first.getType == second.getType && first.getType != VoidType) => BooleanType

      case (x: RefType, EqualOperator | NotEqualOperator, y: RefType) if (TypeChecker.checkTypeMatch(x, y)) => BooleanType
      case (x: RefType, EqualOperator | NotEqualOperator, y: RefType) if (TypeChecker.checkTypeMatch(y, x)) => BooleanType
      case (_: RefType, EqualOperator | NotEqualOperator, NullType) => BooleanType
      case (NullType, EqualOperator | NotEqualOperator, _: RefType) => BooleanType
      case (_: ArrayType, EqualOperator | NotEqualOperator, NullType) => BooleanType
      case (NullType, EqualOperator | NotEqualOperator, _: ArrayType) => BooleanType
      case (NullType, EqualOperator | NotEqualOperator, NullType) => BooleanType

      case (x, op, y) => throw new TypeCheckingError(s"no operation $op found for arguments $x and $y")
    }
}

/**
 * (typeChecked) exp
 */
case class CastExpression(typeCast: Type, target: Expression) extends Expression {
  def getType(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): Type =
    (typeCast, target.getType) match {
      case (x, y) if (TypeChecker.checkTypeMatch(x, y)) => typeCast
      case (x, y) if (TypeChecker.checkTypeMatch(y, x)) => typeCast
      case (_: IntegerTrait, _: IntegerTrait) => typeCast
      case _ => throw new TypeCheckingError("impossile cast: (" + typeCast + ") " + target.getType)
    }
}

/**
 * array[index]
 */
case class ArrayAccess(array: Expression, index: Expression) extends Expression {
  def getType(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): Type =
    (array.getType, index.getType) match {
      case (ArrayType(e), _: IntegerTrait) => e
      case (at, it) => throw new TypeCheckingError(s"type error in array access $at[$it]")
    }
}

/**
 * new typeName[size]
 */
case class ArrayCreation(typeName: Type, size: Expression) extends Expression {
  def getType(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): Type =
    size.getType match {
      case _: IntegerTrait => typeName
      case _ => throw new TypeCheckingError(s"type error in array size ($size)")
    }
}

/**
 * leftHandSide = rightHandSide
 */
case class Assignment(leftHandSide: Expression, rightHandSide: Expression) extends Expression {
  def getType(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): Type = {
    leftHandSide match {
      case FieldAccess(LinkedVariableOrField(_, _: ArrayType, _), "length") => throw new TypeCheckingError("length field of an array is final")
      case _ =>
    }
    if (!TypeChecker.checkTypeMatch(leftHandSide.getType, rightHandSide.getType)) throw new TypeCheckingError(s"assignment: types don't match (expected ${leftHandSide.getType}, found ${rightHandSide.getType})\nLHS expression: $leftHandSide\nRHS expression: $rightHandSide")
    else leftHandSide.getType
  }
}

/**
 * accessed.field
 */
case class FieldAccess(accessed: Expression, field: String) extends Expression {
  def getType(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): Type =
    accessed.getType match {
      case r: RefType =>
        val f = Util.findField(r, field)
        val fIsProtected = f.modifiers.contains(Modifier.protectedModifier)
        val fIsStatic    = f.modifiers.contains(Modifier.staticModifier)
        if (fIsProtected && !TypeChecker.checkTypeMatch(r, myType))
          throw new TypeCheckingError("trying to access protected field in a different class")
        if (accessed.isInstanceOf[RefType] && !fIsStatic)
          throw new TypeCheckingError("trying to access non-static field in an type")
        else if (!accessed.isInstanceOf[RefType] && fIsStatic)
          throw new TypeCheckingError("trying to access static field in an instance")
        else
          f.fieldType
      case a: ArrayType if (field == "length") => IntType
      case x => throw new TypeCheckingError(s"trying access member of non-reference type ($x)")
    }
}

/**
 * new Class(arguments)
 */
case class ClassCreation(constructor: RefType, arguments: List[Expression]) extends Expression {
  def getType(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): Type = {
    constructor.asInstanceOf[RefTypeLinked].getTypeDef match {
      case ClassDefinition(_, _, _, mods, _, constructors, _) =>
        if (!constructors.exists(c => Util.compParams(c.parameters, arguments)))
          throw new TypeCheckingError("found no contructor that matches parameters")
        if(mods.contains(Modifier.abstractModifier))
          throw new TypeCheckingError("cannot instantiate abstract class")
      case _: InterfaceDefinition =>
        throw new TypeCheckingError("cannot instantiate interface")
    }
    constructor
  }
}

/**
 * method(arguments)
 */
case class ThisMethodInvocation(thisType: RefType, method: String, arguments: List[Expression]) extends Expression {
  def getType(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): Type = {
    val m = Util.findMethodThrows(thisType, method, arguments)
    val mIsStatic = m.modifiers.contains(Modifier.staticModifier)
    if (isStatic)
      throw new TypeCheckingError("trying to access non-static method in a static block (implicit this)")
    else
      m.returnType
  }
}

/**
 * (accessed).method(arguments)
 */
case class ExprMethodInvocation(accessed: Expression, method: String, arguments: List[Expression]) extends Expression {
  def getType(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): Type =
    accessed.getType match {
      case r: RefType =>
        val m = Util.findMethodThrows(r, method, arguments)
        if (accessed.isInstanceOf[RefType] && !m.modifiers.contains(Modifier.staticModifier))
          throw new TypeCheckingError("trying to access non-static method in an type")
        else if (!accessed.isInstanceOf[RefType] && m.modifiers.contains(Modifier.staticModifier))
          throw new TypeCheckingError("trying to access static method in an instance")
        else
          m.returnType
      case x => throw new TypeCheckingError(s"trying access member of non-reference type ($accessed of type $x)")
    }
}

/**
 * exp instanceof typeChecked
 */
case class InstanceOfCall(exp: Expression, typeChecked: Type) extends Expression {
  def getType(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): Type =
    (exp.getType, typeChecked) match {

      case (p: PrimitiveType, _) => throw new TypeCheckingError("instanceof incompatible with primitive type: " + p)
      case (_, p: PrimitiveType) => throw new TypeCheckingError("instanceof incompatible with primitive type: " + p)
      case (_, NullType | VoidType) => throw new TypeCheckingError("cannot cast to void or null")

      case (x, y) if (TypeChecker.checkTypeMatch(x, y)) => BooleanType
      case (x, y) if (TypeChecker.checkTypeMatch(y, x)) => BooleanType

      case _ => throw new TypeCheckingError("cannot instanceof with: " + exp.getType + " instanceof " + typeChecked)
    }
}

/**
 * this
 */
case class This(thisType: RefType) extends Expression {
  def getType(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): Type = {
    if (isStatic) throw new TypeCheckingError("cannot use this in a static block")
    thisType
  }
}

/**
 * x
 */
case class VariableAccess(str: String) extends Expression {
  def getType(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): Type = sys.error(s"getType is not supposed to be called on type VariableAccess ($str)")
}

case class LinkedVariableOrField(name: String, varType: Type, variablePath: PathToDeclaration) extends LinkedExpression {
  def getType(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): Type = {
    variablePath match {
      case PathToField(refType, _) =>
        val f = Util.findField(refType, name)
        val fIsStatic = f.modifiers.contains(Modifier.staticModifier)
        if (isStatic && !fIsStatic)
          throw new TypeCheckingError("trying to access non-static field in a static block (implicit this)")
        else if (!isStatic && fIsStatic)
          throw new TypeCheckingError("trying to access static field in a non-static block (implicit this)")
      case _ =>
    }
    varType
  }
  val children = Nil
}

case class ParenthesizedExpression(exp: Expression) extends Expression {
  def getType(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): Type = exp match {
    case _ : RefTypeLinked => throw new TypeCheckingError("illegal start of a type")
    case _ => exp.getType
  }
  val children = List(exp)
}
