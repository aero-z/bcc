package typecheck

import ast._
import main.CompilerError

class TypeCheckingError(message: String) extends CompilerError(message)

object TypeChecker {
  
  def checkTypeMatch(expected: Type, found: Type) =
    expected == found ||
    (found == NullType && (expected.isInstanceOf[RefType] || expected.isInstanceOf[ArrayType])) ||
    (expected == Java.Object && found.isInstanceOf[ArrayType])
      
  def check(cus: List[CompilationUnit]) = {
    implicit val compUnits = cus
    
    
    def errCondType(thetype: Type) = s"condition expression must have type boolean (got $thetype)"
    def errRetType(expected: Option[Type], found: Option[Type]) = s"return doesn't match (expected $expected, found $found)"
    def errTypeMismatch(expected: Type, found: Type) = s"type mismatch (expected $expected, found $found)"
    def checkStatement(s: Statement, retType: Option[Type]): Unit = s match {
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
        val retExprType = retExpr.map(_.getType)
        if (retType.isDefined != retExprType.isDefined || (retType.isDefined && !checkTypeMatch(retType.get, retExprType.get)))
          throw new TypeCheckingError(errRetType(retType, retExprType))
      case LocalVariableDeclaration(thetype, _, init) =>
        init.foreach(x => if (!checkTypeMatch(thetype, x.getType)) throw new TypeCheckingError(errTypeMismatch(thetype, x.getType)))
      case WhileStatement(cond, loop) =>
        if (!checkTypeMatch(BooleanType, cond.getType)) throw new TypeCheckingError(errCondType(cond.getType))
        checkStatement(loop, retType)
    }
      
    def checkField(f: FieldDeclaration) = {
      f.initializer.foreach(x => if (!checkTypeMatch(f.fieldType, x.getType)) throw new TypeCheckingError(errTypeMismatch(f.fieldType, x.getType)))
    }
    def checkConstructor(c: ConstructorDeclaration) = {
      checkStatement(c.implementation, None)
    }
    def checkMethod(m: MethodDeclaration) = {
      m.implementation match {
        case Some(b: Block) => checkStatement(b, Some(m.returnType))
        case _ => // nothing to check
      }
    }
    cus.foreach(_ match {
      case CompilationUnit(_, _, Some(ClassDefinition(className, _, _, _, fields, constructors, methods)), _) =>
        //println("=> Checking "+className)
        fields.foreach(checkField)
        constructors.foreach(checkConstructor)
        methods.foreach(checkMethod)
      case _ => // nothing to check
    })
  }
}
