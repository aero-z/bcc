package typecheck

import ast._
import main.CompilerError

object TypeChecker {
  def check(cus: List[CompilationUnit]) = {
    implicit val compUnits = cus
    
    val errCondType = "condition expression must have type boolean"
    val errRetType = "return doesn't match"
    val errTypeMismatch = "type mismatch"
    def checkStatement(s: Statement, retType: Option[Type]): Unit = s match {
      case Block(statements) => statements.foreach(checkStatement(_, retType))
      case EmptyStatement =>
      case ExpressionStatement(expr) =>
        expr.getType
      case ForStatement(init, cond, incr, loop) =>
        init.foreach(checkStatement(_, retType))
        cond.foreach(x => if (x.getType != BooleanType) throw CompilerError(errCondType))
        incr.foreach(_.getType)
        checkStatement(loop, retType)
      case IfStatement(cond, ifblock, elseblock) =>
        if (cond.getType != BooleanType) throw CompilerError(errCondType)
        checkStatement(ifblock, retType)
        elseblock.foreach(checkStatement(_, retType))
      case ReturnStatement(retExpr) =>
        if (retExpr.map(_.getType) != retType)
          throw CompilerError(errRetType) // + s" (retType = $retType, retExpr = $retExpr)")
      case LocalVariableDeclaration(thetype, _, init) =>
        init.foreach(x => if (x.getType != thetype) throw CompilerError(errTypeMismatch+" localVariableDeclaration:: "+x.getType+" != "+thetype))
      case WhileStatement(cond, loop) =>
        if (cond.getType != BooleanType) throw CompilerError(errCondType)
        checkStatement(loop, retType)
    }
      
    def checkField(f: FieldDeclaration) = {
      f.initializer.foreach(x => if (x != f.fieldType) throw CompilerError(errTypeMismatch+" checkField: "+f.fieldName+" != "+x))
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
      case CompilationUnit(_, _, Some(ClassDefinition(_, _, _, _, fields, constructors, methods)), _) =>
        fields.foreach(checkField)
        constructors.foreach(checkConstructor)
        methods.foreach(checkMethod)
      case _ => // nothing to check
    })
  }
}