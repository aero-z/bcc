package ast

import ast.Operator._
import scanner.IntegerToken
import typecheck.TypeCheckingError
import typecheck.TypeCheckingError
import typecheck.TypeChecker
import nameResolution.PathField
import com.sun.org.apache.xalan.internal.xsltc.compiler.util.TypeCheckError
import typecheck.TypeCheckingError
import nameResolution.PathToDeclaration
import main.Joosc
import codegen._

//Every possible expression
trait Expression extends AstNode {
  /**
   * get type of expression AND check for type errors
   */
  def getType(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): Type

  def generateCode(implicit current:List[Int], params:List[String], pathList:List[List[Int]]): List[X86Instruction]
  // TEMP!
  def generateCode2 = List(X86Add(X86eax, X86ebx), X86Mov(X86eax, X86Number(5)))

}

trait LeftHandSide extends Expression{
  def generateAccess: List[X86Instruction]//Save the register we need in the eax register
  def dest(reg: X86Reg) : X86Dest//The destination of the value in function of a register
  def generateCode(implicit current:List[Int], params:List[String], pathList:List[List[Int]]): List[X86Instruction] = generateAccess :+ X86Mov(X86eax, X86RegMemoryAccess(X86eax))
  
}

trait LinkedExpression extends Expression

private object Util {
  
  def compParams(params: List[Parameter], arguments: List[Expression])(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked) = {
    params.map(_.paramType) == arguments.map(_.getType)
  }

  def findMember[DeclType <: MemberDeclaration](refType: RefType, isStaticAccess: Boolean, getMemberFromType: TypeDefinition => Option[DeclType])(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): DeclType = {
    def findInParents(parents: List[RefType]): Option[(DeclType, RefType)] = {
      parents match {
        case Nil => None
        case p :: ps =>
          findMemberRec(p) match {
            case Some(m) => Some(m)
            case None => findInParents(ps)
          }
      }
    }
    
    def findMemberRec(refType: RefType): Option[(DeclType, RefType)] = {
      refType match {
        case t: RefTypeLinked =>
          val (parents, member) = t.getTypeDef match {
            case c @ ClassDefinition(_, parent, interfaces, _, _, _, _) => (interfaces ::: parent.toList, getMemberFromType(c))
            case i @ InterfaceDefinition(_, Nil,     _, _) if (Joosc.linkJavaLang) => (Java.Object :: Nil, getMemberFromType(i))
            case i @ InterfaceDefinition(_, parents, _, _) => (parents, getMemberFromType(i))
          }          
          member match {
            case None => findInParents(parents)
            case Some(m) => Some((m, t))
          }
        case _ => sys.error("RefType is not linked!")
      }
    }

    val (member, definedIn) = findMemberRec(refType) match {
      case None => throw new TypeCheckingError("no matching member found")
      case Some(m) => m
    }
    
    val fIsProtected = member.modifiers.contains(Modifier.protectedModifier)
    val fIsStatic = member.modifiers.contains(Modifier.staticModifier)
    if (fIsProtected && refType.asInstanceOf[RefTypeLinked].pkgName != myType.pkgName) {
      if (!TypeChecker.checkTypeMatch(definedIn, myType))
        throw new TypeCheckingError(s"trying to access protected member $member, refType = $refType, myType = $myType")

      if (!isStaticAccess && !TypeChecker.checkTypeMatch(myType, refType))
        throw new TypeCheckingError(s"trying to access protected member $member, refType = $refType, myType = $myType")

    }
    if (isStaticAccess && !fIsStatic)
      throw new TypeCheckingError("trying to access non-static member in an type")
    else if (!isStaticAccess && fIsStatic)
      throw new TypeCheckingError("trying to access static member in an instance")
    
    member
  }
  
  def findField(refType: RefType, isStaticAccess: Boolean, name: String)(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): FieldDeclaration = {   
    def getField(td: TypeDefinition): Option[FieldDeclaration] = {
      val fields = td match {
          case ClassDefinition(_, _, _, _, fields, _, _) => fields
          case _:InterfaceDefinition => Nil        
      }
      fields.find(_.fieldName == name)
    }
    findMember(refType, isStaticAccess, getField)
  }

  def findMethod(refType: RefType, isStaticAccess: Boolean, name: String, arguments: List[Expression])(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): MethodDeclaration = {
    def getMethod(td: TypeDefinition): Option[MethodDeclaration] = {
      val methods = td match {
          case ClassDefinition(_, _, _, _, _, _, methods) => methods
          case InterfaceDefinition(_, _, _, methods) => methods        
      }
      methods.find(md => (md.methodName == name) && compParams(md.parameters, arguments))
    }
    findMember(refType, isStaticAccess, getMethod)
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

  def generateCode(implicit current:List[Int], params:List[String], pathList:List[List[Int]]): List[X86Instruction] =  operation match {
    case InverseOperator => term.generateCode ::: List(X86Cmp( X86eax, X86Number(1)), X86Bcc(X86eax, X86eax))
    case MinusOperator => term.generateCode :+ X86Neg(X86eax)
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
      //case (_: CharTrait, _: ArithmeticOperator, _: CharTrait) => CharType
      case (_: CharTrait, _: CompareOperator, _: CharTrait) => BooleanType
      case (_: IntegerTrait, _: ArithmeticOperator, _: IntegerTrait) => IntType //includes widening!
      case (_: IntegerTrait, _: CompareOperator, _: IntegerTrait) => BooleanType

      case (BooleanType, _: BooleanOperator, BooleanType) => BooleanType

      case (x: RefType, EqualOperator | NotEqualOperator, y: RefType) if (TypeChecker.checkTypeMatch(x, y)) => BooleanType
      case (x: RefType, EqualOperator | NotEqualOperator, y: RefType) if (TypeChecker.checkTypeMatch(y, x)) => BooleanType
      case (_: RefType, EqualOperator | NotEqualOperator, NullType) => BooleanType
      case (NullType, EqualOperator | NotEqualOperator, _: RefType) => BooleanType
      case (_: ArrayType, EqualOperator | NotEqualOperator, NullType) => BooleanType
      case (NullType, EqualOperator | NotEqualOperator, _: ArrayType) => BooleanType
      case (NullType, EqualOperator | NotEqualOperator, NullType) => BooleanType

      case (x, op, y) => throw new TypeCheckingError(s"no operation $op found for arguments $x and $y")
    }

  def generateCode(implicit current:List[Int], params:List[String], pathList:List[List[Int]]): List[X86Instruction] = ??? //TODO: implementation

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
  def generateCode(implicit current:List[Int], params:List[String], pathList:List[List[Int]]): List[X86Instruction] = ??? //TODO: implementation
}

/**
 * array[index]
 */
case class ArrayAccess(array: Expression, index: Expression) extends LeftHandSide {
  def getType(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): Type =
    (array.getType, index.getType) match {
      case (ArrayType(e), _: IntegerTrait) => e
      case (at, it) => throw new TypeCheckingError(s"type error in array access $at[$it]")
    }

  def generateAccess(implicit current:List[Int], params:List[String], pathList:List[List[Int]]): List[X86Instruction] = ??? //TODO: implementation
  def dest(reg: X86Reg)(implicit current:List[Int], params:List[String], pathList:List[List[Int]]) = ???
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

  def generateCode(implicit current:List[Int], params:List[String], pathList:List[List[Int]]): List[X86Instruction] = ??? //TODO: implementation

}


/**
 * leftHandSide = rightHandSide
 */
case class Assignment(leftHandSide: LeftHandSide, rightHandSide: Expression) extends Expression {
  def getType(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): Type = {
    leftHandSide match {
      case FieldAccess(LinkedVariableOrField(_, _: ArrayType, _), "length") => throw new TypeCheckingError("length field of an array is final")
      case _ =>
    }
    if (!TypeChecker.checkTypeMatch(leftHandSide.getType, rightHandSide.getType)) throw new TypeCheckingError(s"assignment: types don't match (expected ${leftHandSide.getType}, found ${rightHandSide.getType})\nLHS expression: $leftHandSide\nRHS expression: $rightHandSide")
    else leftHandSide.getType
  }

  def generateCode(implicit current:List[Int], params:List[String], pathList:List[List[Int]]): List[X86Instruction] = leftHandSide.generateAccess ::: List(X86Push(X86eax)) :::rightHandSide.generateCode ::: List(X86Pop(X86ebx), X86Mov(X86eax, leftHandSide.dest(X86ebx)))

}

/**
 * accessed.field
 */
case class FieldAccess(accessed: Expression, field: String) extends LeftHandSide {
  def getType(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): Type =
    accessed.getType match {
      case r: RefType =>
        val f = Util.findField(r, accessed.isInstanceOf[RefType], field)        
        f.fieldType
      case a: ArrayType if (field == "length") => IntType
      case x => throw new TypeCheckingError(s"trying access member of non-reference type ($x)")
    }

  def generateAccess(implicit current:List[Int], params:List[String], pathList:List[List[Int]]): List[X86Instruction] = ??? //TODO: implementation
  def dest(reg: X86Reg)(implicit current:List[Int], params:List[String], pathList:List[List[Int]]) : X86Dest = ???
}

/**
 * new Class(arguments)
 */
case class ClassCreation(constructor: RefType, arguments: List[Expression]) extends Expression {
  def getType(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): Type = {
    val consLinked = constructor.asInstanceOf[RefTypeLinked]
    consLinked.getTypeDef match {
      case ClassDefinition(_, _, _, mods, _, constructors, _) =>
        if (!constructors.exists(c => {
          if (c.modifiers.contains(Modifier.protectedModifier) && myType.pkgName != consLinked.pkgName)
            throw new TypeCheckingError("cannot call protected constructor")
          Util.compParams(c.parameters, arguments)
        }))
          throw new TypeCheckingError("found no contructor that matches parameters")
        if(mods.contains(Modifier.abstractModifier))
          throw new TypeCheckingError("cannot instantiate abstract class")
      case _: InterfaceDefinition =>
        throw new TypeCheckingError("cannot instantiate interface")
    }
    constructor
  }

  def generateCode(implicit current:List[Int], params:List[String], pathList:List[List[Int]]): List[X86Instruction] = ??? //TODO: implementation

}

/**
 * method(arguments)
 */
case class ThisMethodInvocation(thisType: RefType, method: String, arguments: List[Expression]) extends Expression {
  def getType(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): Type =
    ExprMethodInvocation(This(thisType), method, arguments).getType

  def generateCode(implicit current:List[Int], params:List[String], pathList:List[List[Int]]): List[X86Instruction] = ExprMethodInvocation(This(thisType), method, arguments).generateCode

}

/**
 * (accessed).method(arguments)
 */
case class ExprMethodInvocation(accessed: Expression, method: String, arguments: List[Expression]) extends Expression {
  def getType(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): Type =
    accessed.getType match {
      case r: RefType =>
        val m = Util.findMethod(r, accessed.isInstanceOf[RefType], method, arguments)
        m.returnType
      case x => throw new TypeCheckingError(s"trying access member of non-reference type ($accessed of type $x)")
    }

  def generateCode(implicit current:List[Int], params:List[String], pathList:List[List[Int]]): List[X86Instruction] = ??? //TODO: implementation When we call a static function static, ebp should be null

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

  def generateCode(implicit current:List[Int], params:List[String], pathList:List[List[Int]]): List[X86Instruction] = ??? //TODO: implementation

}

/**
 * this
 */
case class This(thisType: RefType) extends Expression {
  def getType(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): Type = {
    if (isStatic) throw new TypeCheckingError("cannot use this in a static block")
    thisType
  }

  def generateCode(implicit current:List[Int], params:List[String], pathList:List[List[Int]]): List[X86Instruction] = ??? //TODO: implementation


}

/**
 * x
 */
case class VariableAccess(str: String) extends LeftHandSide {
  def getType(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): Type = sys.error(s"getType is not supposed to be called on type VariableAccess ($str)")

  def generateAccess(implicit current:List[Int], params:List[String], pathList:List[List[Int]]): List[X86Instruction] = sys.error("Trying to generate the code for an unlinked variable")
  def dest(reg: X86Reg)(implicit current:List[Int], params:List[String], pathList:List[List[Int]]) = sys.error("Trying to generate the code for an unlinked variable.")
}

case class LinkedVariableOrField(name: String, varType: Type, variablePath: PathToDeclaration) extends LinkedExpression with LeftHandSide {
  def getType(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): Type = {
    variablePath match {
      case _: PathField =>
        FieldAccess(This(myType), name).getType
      case _ =>
    }
    varType
  }
  val children = Nil

  def generateAccess(implicit current:List[Int], params:List[String], pathList:List[List[Int]]): List[X86Instruction] = ??? //TODO: implementation
  def dest(reg: X86Reg)(implicit current:List[Int], params:List[String], pathList:List[List[Int]]) = ???
}

case class ParenthesizedExpression(exp: Expression) extends Expression {
  def getType(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): Type = exp match {
    case _ : RefTypeLinked => throw new TypeCheckingError("illegal start of a type")
    case _ => exp.getType
  }
  val children = List(exp)
  def generateCode(implicit current:List[Int], params:List[String], pathList:List[List[Int]]): List[X86Instruction] = exp.generateCode
}
