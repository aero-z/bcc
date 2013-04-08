package ast

import ast.Operator._
import scanner.IntegerToken
import typecheck.TypeCheckingError
import typecheck.TypeCheckingError
import typecheck.TypeChecker
import nameResolution.PathField
import typecheck.TypeCheckingError
import nameResolution._
import main.Joosc
import codegen._
import codegen.X86Gen._
//Every possible expression
trait Expression extends AstNode {
  /**
   * get type of expression AND check for type errors
   */
  private var myT: Type = null
  def getT = {
    if (myT == null) println(this)
    assert(myT != null) // checkAndSetType() must be called before this
    myT
  }
  
  def getType(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): Type = {
    if (myT == null)
      myT = checkAndSetType
    myT
  }
  
  protected def checkAndSetType(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): Type

  def generateCode(implicit current:List[Int], params:List[String], pathList:List[List[Int]], cus:List[CompilationUnit]): List[X86Instruction]
}

trait LeftHandSide extends Expression{
  def generateAccess(implicit current:List[Int], params:List[String], pathList:List[List[Int]], cus:List[CompilationUnit]): List[X86Instruction]//Save the register we need in the eax register
  def dest(reg: X86Reg)(implicit current:List[Int], params:List[String], pathList:List[List[Int]], cus:List[CompilationUnit]) : X86Dest//The destination of the value in function of a register
  def generateCode(implicit current:List[Int], params:List[String], pathList:List[List[Int]], cus:List[CompilationUnit]): List[X86Instruction] = generateAccess :+ X86Mov(X86eax, dest(X86eax))
  
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
  def checkAndSetType(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): Type = {
    (operation, term.getType) match {
      case (InverseOperator, BooleanType) => BooleanType
      case (InverseOperator, _: IntegerTrait) => BooleanType
      case (MinusOperator, t: IntegerTrait) => t
      case (op, t) => throw new TypeCheckingError(s"invalid unary operation ($op, $t)")
    }
  }

  def generateCode(implicit current:List[Int], params:List[String], pathList:List[List[Int]], cus:List[CompilationUnit]): List[X86Instruction] =  operation match {
    case InverseOperator => term.generateCode ::: List(X86Cmp( X86eax, X86Number(1)), X86Sbb(X86eax, X86eax))
    case MinusOperator => term.generateCode :+ X86Neg(X86eax)
  }
}

/**
 * first operation second
 * e.g. 5 + "hey"
 */
case class BinaryOperation(first: Expression, operation: Operator, second: Expression) extends Expression {
  val Str = RefTypeLinked(Some(Name("java" :: "lang" :: Nil)), "String")
  def checkAndSetType(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): Type = {
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
  }

  def generateCode(implicit current:List[Int], params:List[String], pathList:List[List[Int]], cus:List[CompilationUnit]): List[X86Instruction] = {
    (first.getT, operation, second.getT) match {
      case (t, PlusOperator, Str) => Nil //TODO: String.concat
      case (Str, PlusOperator, t) => Nil //TODO: String concat
      case (_: CharTrait, op: CompareOperator, _: CharTrait) =>
        val endLabel = LabelGenerator.generate()
        second.generateCode ::: X86Push(X86eax) :: first.generateCode ::: X86Pop(X86ebx) :: X86Cmp(X86eax, X86ebx) :: X86Mov(X86eax, X86Number(0)) /*false by default*/ ::
        (op match {
          //Is the truelabel needed?
          case SmallerOperator => X86Jge(endLabel) 
          case GreaterOperator => X86Jle(endLabel)
          case EqualOperator => X86Jne(endLabel)
          case NotEqualOperator => X86Je(endLabel)
          case LessEqualOperator => X86Jg(endLabel)
          case GreaterEqualOperator => X86Jl(endLabel)
        }) :: X86Mov(X86eax, X86Number(1)) :: endLabel :: Nil
      case (_: IntegerTrait, op: ArithmeticOperator, _: IntegerTrait) => 
        second.generateCode ::: X86Push(X86eax) :: first.generateCode ::: X86Pop(X86ebx) :: 
        (op match {
          case PlusOperator => X86Add(X86eax, X86ebx) :: Nil
          case MinusOperator => X86Sub(X86eax, X86ebx) :: Nil
          case StarOperator => X86Imul(X86eax, X86ebx) :: Nil
          case DivOperator => X86Idiv(X86eax, X86ebx) :: Nil
          case ModOperator => X86Idiv(X86eax, X86ebx) :: X86Mov(X86eax, X86edx) :: Nil
        })
      case (left: IntegerTrait , op: CompareOperator, right: IntegerTrait) => //copy from character comparison
        val endLabel = LabelGenerator.generate()
        second.generateCode ::: X86Push(X86eax) :: first.generateCode ::: X86Pop(X86ebx) :: X86Cmp(X86eax, X86ebx) :: X86Mov(X86eax, X86Number(0)) /*false by default*/ ::
        (op match {
          case SmallerOperator => X86Jge(endLabel) 
          case GreaterOperator => X86Jle(endLabel)
          case EqualOperator => X86Jne(endLabel)
          case NotEqualOperator => X86Je(endLabel)
          case LessEqualOperator => X86Jg(endLabel)
          case GreaterEqualOperator => X86Jl(endLabel)
        }) :: X86Mov(X86eax, X86Number(1)) :: endLabel :: Nil
      case (BooleanType, op: BooleanOperator, BooleanType) =>
        val endLabel = LabelGenerator.generate()
        second.generateCode ::: X86Push(X86eax) :: first.generateCode :::
        (op match {
          case BitXorOperator =>
            X86Bxor(X86eax, X86ebx) :: Nil
          case BitAndOperator =>
            X86Band(X86eax, X86ebx) :: Nil
          case BitOrOperator =>
            X86Bor(X86eax, X86ebx) :: Nil
          case EqualOperator =>
            X86Cmp(X86eax, X86ebx) :: X86Mov(X86eax, X86Number(0)) /*false by default*/ :: X86Jne(endLabel) :: X86Mov(X86eax, X86Number(1)) :: endLabel :: Nil
          case NotEqualOperator =>
            X86Cmp(X86eax, X86ebx) :: X86Mov(X86eax, X86Number(0)) /*false by default*/ :: X86Je(endLabel) :: X86Mov(X86eax, X86Number(1)) :: endLabel :: Nil
          case AndOperator =>
            val falseLabel = LabelGenerator.generate()
            X86Cmp(X86eax, X86Number(0)) :: X86Je(falseLabel) :: X86Cmp(X86ebx, X86Number(0)) :: X86Je(falseLabel) :: X86Mov(X86eax, X86Number(1)) :: X86Jmp(endLabel) :: falseLabel :: X86Mov(X86eax, X86Number(0)) :: endLabel :: Nil
          case OrOperator =>
            val trueLabel = LabelGenerator.generate()
            X86Cmp(X86eax, X86Number(1)) :: X86Je(trueLabel) :: X86Cmp(X86eax, X86Number(1)) :: X86Je(trueLabel) :: X86Mov(X86eax, X86Number(0)) :: X86Jmp(endLabel) :: trueLabel :: X86Mov(X86eax, X86Number(1)) :: endLabel :: Nil
        })
      case (_, EqualOperator | NotEqualOperator, _) =>
        val endLabel = LabelGenerator.generate()
        second.generateCode ::: X86Push(X86eax) :: first.generateCode ::: X86Pop(X86ebx) :: X86Cmp(X86eax, X86ebx) :: X86Mov(X86eax, X86Number(0)) /*false by default*/ ::
        (operation match {
          case EqualOperator => X86Jne(endLabel) :: X86Mov(X86eax, X86Number(1)) :: endLabel :: Nil
          case NotEqualOperator => X86Je(endLabel) :: X86Mov(X86eax, X86Number(1)) :: endLabel :: Nil
        })
      /* -> TODO we can treat all this the same right?
      case (x: RefType, EqualOperator | NotEqualOperator, y: RefType) if (TypeChecker.checkTypeMatch(y, x)) =>
        
      case (_: RefType, EqualOperator | NotEqualOperator, NullType) =>
        
      case (NullType, EqualOperator | NotEqualOperator, _: RefType) =>
        
      case (_: ArrayType, EqualOperator | NotEqualOperator, NullType) =>
        
      case (NullType, EqualOperator | NotEqualOperator, _: ArrayType) =>
        
      case (NullType, EqualOperator | NotEqualOperator, NullType) =>

      case (x, op, y) => throw new TypeCheckingError(s"no operation $op found for arguments $x and $y")
      * 
      */
    }
  }
}

/**
 * (typeChecked) exp
 */
case class CastExpression(typeCast: Type, target: Expression) extends Expression {
  def checkAndSetType(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): Type = {
    (typeCast, target.getType) match {
      case (x, y) if (TypeChecker.checkTypeMatch(x, y)) => typeCast
      case (x, y) if (TypeChecker.checkTypeMatch(y, x)) => typeCast
      case (_: IntegerTrait, _: IntegerTrait) => typeCast
      case _ => throw new TypeCheckingError("impossile cast: (" + typeCast + ") " + target.getType)
    }
  }
  def generateCode(implicit current:List[Int], params:List[String], pathList:List[List[Int]], cus:List[CompilationUnit]): List[X86Instruction] = target.generateCode //TODO chakge that 
}

/**
 * array[index]
 */
case class ArrayAccess(array: Expression, index: Expression) extends LeftHandSide {
  def checkAndSetType(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): Type = {
    (array.getType, index.getType) match {
      case (ArrayType(e), _: IntegerTrait) => e
      case (at, it) => throw new TypeCheckingError(s"type error in array access $at[$it]")
    }
  }

  def generateAccess(implicit current:List[Int], params:List[String], pathList:List[List[Int]], cus:List[CompilationUnit]): List[X86Instruction] = {
    val arrComp = array.generateCode ::: (nullCheck(X86eax) :+ X86Push(X86eax))
    val indexComp = index.generateCode 
    val checkIndex = List(X86Cmp(X86eax, X86Number(0)), X86Jl(X86Exception), X86Pop(X86edx), X86Cmp(X86eax, X86RegOffsetMemoryAccess(X86edx, 4)), X86Jge(X86Exception))
    val generateAddr = List(X86Shl(X86eax, 2), X86Add(X86eax, X86edx))
    arrComp ::: indexComp ::: checkIndex ::: generateAddr
  }
  def dest(reg: X86Reg)(implicit current:List[Int], params:List[String], pathList:List[List[Int]], cus:List[CompilationUnit]) = X86RegOffsetMemoryAccess(reg, 4)
}

/**
 * new typeName[size]
 */
case class ArrayCreation(typeName: Type, size: Expression) extends Expression {
  def checkAndSetType(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): Type = {
    size.getType match {
      case _: IntegerTrait => typeName
      case _ => throw new TypeCheckingError(s"type error in array size ($size)")
    }
  }

  def generateCode(implicit current:List[Int], params:List[String], pathList:List[List[Int]], cus:List[CompilationUnit]): List[X86Instruction] = {
    size.generateCode ::: X86Add(X86eax, X86Number(2)) /*3 extra fields*/ :: X86Imul(X86eax, X86Number(4)) /*times bytes*/ :: X86Call(X86Label("__malloc")) :: Nil //eax will contain the error
  }
}

/**
 * leftHandSide = rightHandSide
 */
case class Assignment(leftHandSide: LeftHandSide, rightHandSide: Expression) extends Expression {
  def checkAndSetType(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): Type = {
    leftHandSide match {
      case FieldAccess(LinkedVariableOrField(_, _: ArrayType, _), "length") => throw new TypeCheckingError("length field of an array is final")
      case _ =>
    }
    if (!TypeChecker.checkTypeMatch(leftHandSide.getType, rightHandSide.getType)) throw new TypeCheckingError(s"assignment: types don't match (expected ${leftHandSide.getType}, found ${rightHandSide.getType})\nLHS expression: $leftHandSide\nRHS expression: $rightHandSide")
    else leftHandSide.getType
  }

  def generateCode(implicit current:List[Int], params:List[String], pathList:List[List[Int]], cus:List[CompilationUnit]): List[X86Instruction] =
    leftHandSide.generateAccess ::: List(X86Push(X86eax)) :::rightHandSide.generateCode ::: List(X86Pop(X86ebx), X86Mov(leftHandSide.dest(X86ebx), X86eax))

}

/**
 * accessed.field
 */
case class FieldAccess(accessed: Expression, field: String) extends LeftHandSide {
  def checkAndSetType(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): Type = {
    accessed.getType match {
      case r: RefType =>
        val f = Util.findField(r, accessed.isInstanceOf[RefType], field)        
        f.fieldType
      case a: ArrayType if (field == "length") => IntType
      case x => throw new TypeCheckingError(s"trying access member of non-reference type ($x)")
    }
  }

  def generateAccess(implicit current:List[Int], params:List[String], pathList:List[List[Int]], cus:List[CompilationUnit]): List[X86Instruction] = {
    accessed match {
      case RefTypeLinked(pkgName: Option[Name], className:String) =>
        Nil
      case a: ArrayType if (field == "length") => access.generateCode
      case x => notImpl
    }
  }
  def dest(reg: X86Reg)(implicit current:List[Int], params:List[String], pathList:List[List[Int]], cus:List[CompilationUnit]) : X86Dest =  {
    accessed match {
      case RefTypeLinked(pkgName: Option[Name], className:String) =>
        X86LblMemoryAccess(X86Label(CodeGenerator.makeLabel(pkgName, className, field)))
      case a: ArrayType if (field == "length") => X86RegOffsetMemoryAccess(X86Reg, X86Number(4))
      case x => X86eax
    }
  }
}

/**
 * new Class(arguments)
 */
case class ClassCreation(constructor: RefType, arguments: List[Expression]) extends Expression {
  def checkAndSetType(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): Type = {
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

  def generateCode(implicit current:List[Int], params:List[String], pathList:List[List[Int]], cus:List[CompilationUnit]): List[X86Instruction] = {
    val ref = constructor.asInstanceOf[RefTypeLinked]
    val classDef = ref.getTypeDef.asInstanceOf[ClassDefinition] // can only instantiate classes
    val consDef = classDef.constructors.find(_.parameters.map(_.paramType) == arguments.map(_.getT)).get
    X86Call(X86Label(CodeGenerator.makeLabel(ref.pkgName, classDef, "$alloc"))) ::
    X86Call(X86Label(CodeGenerator.makeConstructorLabel(ref.pkgName, classDef, consDef))) :: Nil

  }
}

/**
 * method(arguments)
 */
case class ThisMethodInvocation(thisType: RefType, method: String, arguments: List[Expression]) extends Expression {
  private val t = This(thisType)

  def checkAndSetType(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): Type = {
    t.getType
    ExprMethodInvocation(t, method, arguments).getType
  }

  def generateCode(implicit current:List[Int], params:List[String], pathList:List[List[Int]], cus:List[CompilationUnit]): List[X86Instruction] = {
    ExprMethodInvocation(t, method, arguments).generateCode
  }

}

/**
 * (accessed).method(arguments)
 */
case class ExprMethodInvocation(accessed: Expression, method: String, arguments: List[Expression]) extends Expression {
  def checkAndSetType(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): Type = {
    accessed.getType match {
      case r: RefType =>
        val m = Util.findMethod(r, accessed.isInstanceOf[RefType], method, arguments)
        m.returnType
      case x => throw new TypeCheckingError(s"trying access member of non-reference type ($accessed of type $x)")
    }
  }

  def generateCode(implicit current:List[Int], params:List[String], pathList:List[List[Int]], cus:List[CompilationUnit]): List[X86Instruction] = {
    val accessComp = accessed match {
      case _: Type => Nil // List(X86Mov(X86RegMemoryAccess(X86ebp), X86Number(0)))
      case _ => accessed.generateCode ::: (nullCheck(X86eax) :+  X86Mov(X86RegMemoryAccess(X86ebp), X86eax))
    }

    val argumentsComp  = arguments.zipWithIndex.flatMap{case (exp, ind) => exp.generateCode :+ X86Mov(X86RegOffsetMemoryAccess(X86ebp, 4*(1 + ind)), X86eax)}
    val call = accessed match {
      case r: RefTypeLinked =>
        // static methods
        def findMethod(cd: ClassDefinition): MethodDeclaration = {
          cd.methods.find(
            md => (md.methodName == method) && (md.parameters.map(_.paramType) == arguments.map(_.getT))
          ) match {
            case Some(m) => m
            case None => findMethod(cd.parent.get.asInstanceOf[RefTypeLinked].getTypeDef.asInstanceOf[ClassDefinition])
          }
        }
        val classDef = r.getTypeDef.asInstanceOf[ClassDefinition] // if it is a static method we know we are in a class
        val lbl = CodeGenerator.makeMethodLabel(r.pkgName, classDef, findMethod(classDef))
        X86Call(X86Label(lbl)) :: Nil
      case _ =>
        accessed.getT.asInstanceOf[RefTypeLinked].getTypeDef match {
          case cd: ClassDefinition =>
            val index = CodeGenerator.getVtable(accessed.getT.asInstanceOf[RefTypeLinked].pkgName, cd, cus).indexWhere(meth => meth.methodName == method && meth.parameters.map(_.paramType) == arguments.map(_.getT))
            List(X86Mov(X86eax, X86RegMemoryAccess(X86ebp)), X86Call(X86RegOffsetMemoryAccess(X86eax, 4 * (index + 1))))
          case _: InterfaceDefinition => notImpl
        }
    }
    accessComp ::: argumentsComp ::: call
  }
}

/**
 * exp instanceof typeChecked
 */
case class InstanceOfCall(exp: Expression, typeChecked: Type) extends Expression {
  def checkAndSetType(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): Type = {
    (exp.getType, typeChecked) match {

      case (p: PrimitiveType, _) => throw new TypeCheckingError("instanceof incompatible with primitive type: " + p)
      case (_, p: PrimitiveType) => throw new TypeCheckingError("instanceof incompatible with primitive type: " + p)
      case (_, NullType | VoidType) => throw new TypeCheckingError("cannot cast to void or null")

      case (x, y) if (TypeChecker.checkTypeMatch(x, y)) => BooleanType
      case (x, y) if (TypeChecker.checkTypeMatch(y, x)) => BooleanType

      case _ => throw new TypeCheckingError("cannot instanceof with: " + exp.getType + " instanceof " + typeChecked)
    }
  }

  def generateCode(implicit current:List[Int], params:List[String], pathList:List[List[Int]], cus:List[CompilationUnit]): List[X86Instruction] = exp.generateCode :+ X86Mov(X86eax, X86Number(1)) //TODO: implementation


}

/**
 * this
 */
case class This(thisType: RefType) extends Expression {
  def checkAndSetType(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): Type = {
    if (isStatic) throw new TypeCheckingError("cannot use this in a static block")
    thisType
  }

  def generateCode(implicit current:List[Int], params:List[String], pathList:List[List[Int]], cus:List[CompilationUnit]): List[X86Instruction] = List(X86Mov(X86eax, X86ebp))
}

/**
 * x
 */
case class VariableAccess(str: String) extends LeftHandSide {
  def checkAndSetType(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): Type = sys.error(s"getType is not supposed to be called on type VariableAccess ($str)")

  def generateAccess(implicit current:List[Int], params:List[String], pathList:List[List[Int]], cus:List[CompilationUnit]): List[X86Instruction] = sys.error("Trying to generate the code for an unlinked variable")
  def dest(reg: X86Reg)(implicit current:List[Int], params:List[String], pathList:List[List[Int]], cus:List[CompilationUnit]) = sys.error("Trying to generate the code for an unlinked variable.")
}

case class LinkedVariableOrField(name: String, varType: Type, variablePath: PathToDeclaration) extends LinkedExpression with LeftHandSide {
  val fakeThis = This(myType)
  def checkAndSetType(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): Type = {
    variablePath match {
      case _: PathField => FieldAccess(fakeThis, name).getType
      case _ => varType
    }
  }
  val children = Nil


  def generateAccess(implicit current:List[Int], params:List[String], pathList:List[List[Int]], cus: List[CompilationUnit]): List[X86Instruction] = variablePath match {
    case PathField(refType, name) => FieldAccess(fakeThis, name).generateAccess
    case PathPar(name) => Nil
    case PathLocal(index) => Nil
  }
  def dest(reg: X86Reg)(implicit current:List[Int], params:List[String], pathList:List[List[Int]], cus:List[CompilationUnit]) = variablePath match {
    case PathField(refType, name) => FieldAccess(fakeThis,name).dest(reg)
    case PathPar(name) => X86RegOffsetMemoryAccess(X86ebp, 4*(params.indexOf(name) + 1))
    case PathLocal(index) => X86RegOffsetMemoryAccess(X86ebp, 4*(- params.indexOf(index) - 2))
  }
}

case class ParenthesizedExpression(exp: Expression) extends Expression {
  def checkAndSetType(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): Type = {
    exp match {
	  case _ : RefTypeLinked => throw new TypeCheckingError("illegal start of a type")
	  case _ => exp.getType
	}
  }
  val children = List(exp)
  def generateCode(implicit current:List[Int], params:List[String], pathList:List[List[Int]], cus:List[CompilationUnit]): List[X86Instruction] = exp.generateCode
}
