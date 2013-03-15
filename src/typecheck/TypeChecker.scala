package typecheck

import ast._
import main.CompilerError

class TypeCheckingError(message: String) extends CompilerError(message)

object TypeChecker {

  def findMother(find: RefTypeLinked, cus: List[CompilationUnit], current: RefTypeLinked): Boolean = {
    if (find == current) //found!
      true
    else
      current.getTypeDef(cus) match {
        case ClassDefinition(className, parent, interfaces, _, _, _, _) =>
          (parent.toList ::: interfaces) match {
            case Nil => false
            case list => list.exists(x => findMother(find, cus, x.asInstanceOf[RefTypeLinked]))
          }
        case InterfaceDefinition(name, parents, _, _) =>
          parents match {
            case Nil => false
            case list => list.exists(x => findMother(find, cus, x.asInstanceOf[RefTypeLinked]))
          }
      }
  }

  def checkTypeMatch(expected: Type, found: Type)(implicit cus: List[CompilationUnit]): Boolean = {
    (expected, found) match {
      //case (NullType, _) => sys.error("nothing can be assigned to null")
      case (x, y) if (x == y) => true
      case (Java.Object, _: RefTypeLinked) => true
      case (x: RefTypeLinked, y: RefTypeLinked) => findMother(x, cus, y)
      case (ArrayType(x: RefTypeLinked), ArrayType(y: RefTypeLinked)) => findMother(x, cus, y)
      case (ArrayType(x), ArrayType(y)) => (x == y)
      case (_: RefTypeLinked, NullType) => true
      case (_: ArrayType, NullType) => true
      case (Java.Object, _: ArrayType) => true
      case (Java.Cloneable, _: ArrayType) => true
      case (Java.Serializable, _: ArrayType) => true
      case (IntType, _: IntegerTrait) => true
      case (ShortType, _: ShortTrait) => true
      case (ByteType, _: ByteTrait) => true
      case (CharType, _: CharTrait) => true
      case _ => false
    }
  }

  def check(cus: List[CompilationUnit]) = {
    implicit val compUnits = cus

    def errCondType(thetype: Type) = s"condition expression must have type boolean (got $thetype)"
    def errRetType(expected: Type, found: Type) = s"return doesn't match (expected $expected, found $found)"
    def errTypeMismatch(expected: Type, found: Type) = s"type mismatch (expected $expected, found $found)"
    def checkStatement(s: Statement, retType: Type)(implicit isStatic: Boolean, classDef: ClassDefinition): Unit = s match {
      case Block(statements) => statements.foreach(checkStatement(_, retType))
      case EmptyStatement =>
      case ExpressionStatement(expr) =>
        expr.getType
      case ForStatement(init, cond, incr, loop) =>
        init.foreach(checkStatement(_, retType))
        cond.foreach(x => if (!checkTypeMatch(BooleanType, x.getType)) throw new TypeCheckingError(errCondType(x.getType)))
        incr.foreach(_.getType)
        checkStatement(loop, retType)
      case IfStatement(cond, ifblock, elseblock) =>
        if (cond.getType != BooleanType) throw new TypeCheckingError(errCondType(cond.getType))
        checkStatement(ifblock, retType)
        elseblock.foreach(checkStatement(_, retType))
      case ReturnStatement(retExpr) =>
        val retExprType = retExpr match {
          case Some(expr) =>
            if (expr.getType == VoidType) throw new TypeCheckingError("cannot return void value")
            else expr.getType
          case None => VoidType
        }
        if (!checkTypeMatch(retType, retExprType))
          throw new TypeCheckingError(errRetType(retType, retExprType))
      case LocalVariableDeclaration(thetype, _, init) =>
        init.foreach(x => if (!checkTypeMatch(thetype, x.getType)) throw new TypeCheckingError(errTypeMismatch(thetype, x.getType)))
      case WhileStatement(cond, loop) =>
        if (!checkTypeMatch(BooleanType, cond.getType)) throw new TypeCheckingError(errCondType(cond.getType))
        checkStatement(loop, retType)
    }

    def checkField(f: FieldDeclaration)(implicit classDef: ClassDefinition) = {
      implicit val isStatic = f.modifiers.contains(Modifier.staticModifier)
      f.initializer.foreach(x => if (!checkTypeMatch(f.fieldType, x.getType)) throw new TypeCheckingError(errTypeMismatch(f.fieldType, x.getType)))
    }
    def checkConstructor(c: ConstructorDeclaration)(implicit classDef: ClassDefinition) = {
      implicit val isStatic = false
      checkStatement(c.implementation, VoidType)
    }
    def checkMethod(m: MethodDeclaration)(implicit classDef: ClassDefinition) = {
      implicit val isStatic = m.modifiers.contains(Modifier.staticModifier)
      m.implementation match {
        case Some(b: Block) => checkStatement(b, m.returnType)
        case _ => // nothing to check
      }
    }
    cus.foreach(_ match {
      case CompilationUnit(_, _, Some(cd @ ClassDefinition(className, parent, _, _, fields, constructors, methods)), _) =>
        //println("=> Checking "+className)
        implicit val classDef = cd
        parent.foreach(p => if (!p.asInstanceOf[RefTypeLinked].getTypeDef.asInstanceOf[ClassDefinition].constructors.exists(c =>
          c.parameters.isEmpty)) // implicit super();
          throw new TypeCheckingError("parent class has no default constructor")
        )
        fields.foreach(checkField)
        constructors.foreach(checkConstructor)
        methods.foreach(checkMethod)
      case _ => // nothing to check
    })
  }
}
