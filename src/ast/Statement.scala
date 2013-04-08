package ast

import codegen._

import nameResolution._

sealed abstract class Statement extends AstNode {
  def generateCode(current:List[Int])(implicit params:List[String], pathList:List[List[Int]], cus:List[CompilationUnit]):List[X86Instruction]
}

case class Block (statements : List[Statement]) extends Statement{
  def generateCode(current:List[Int])(implicit params:List[String], pathList:List[List[Int]], cus:List[CompilationUnit]):List[X86Instruction] = {
    statements.zipWithIndex.flatMap(x => x._1.generateCode(x._2 :: 0 ::current))
  }
  //case Block(stmts) =>  stmts.foldLeft((acc, 0 :: curPath)){ case ((acc, pos), stmt) => (getLocalPath(stmt, acc, pos), pos.head + 1 :: pos.tail)}._1
}

case object EmptyStatement extends Statement{
  def generateCode(current:List[Int])(implicit params:List[String], pathList:List[List[Int]], cus:List[CompilationUnit]):List[X86Instruction] = {
    //val impl = 0 :: current //not needed
    X86Comment("-----") :: Nil
  }
}

case class ExpressionStatement(expression: Expression) extends Statement{
  def generateCode(current:List[Int])(implicit params:List[String], pathList:List[List[Int]], cus:List[CompilationUnit]):List[X86Instruction] = {
    implicit val impl = current
    X86Comment("expression statement (" + expression.getClass.getSimpleName + ")") :: expression.generateCode 
  }
}

case class ForStatement(init: Option[Statement], condition: Option[Expression], incrementation: Option[Expression], loop: Statement) extends Statement{
  def generateCode(current:List[Int])(implicit params:List[String], pathList:List[List[Int]], cus:List[CompilationUnit]):List[X86Instruction] = {
    implicit val impl = current
    val repeatLabel = LabelGenerator.generate()
    val endLabel = LabelGenerator.generate()
    X86Comment("for statement") :: repeatLabel :: init.getOrElse(EmptyStatement).generateCode(0 :: current) ::: codegen.CodeGenerator.iffalse(condition.getOrElse(BooleanLiteral(true)), endLabel) ::: loop.generateCode(1 :: current) ::: X86Jmp(repeatLabel) :: endLabel :: Nil
  }
}

case class IfStatement(condition: Expression, ifStatement: Statement, elseStatement: Option[Statement]) extends Statement{
  def generateCode(current:List[Int])(implicit params:List[String], pathList:List[List[Int]], cus:List[CompilationUnit]):List[X86Instruction] = {
    implicit val impl = current
    elseStatement match {
	    case None =>
	      val endLabel = LabelGenerator.generate()
	      X86Comment("IfStatement") :: codegen.CodeGenerator.iffalse(condition, endLabel) ::: ifStatement.generateCode(0 :: current) ::: endLabel :: Nil
	    case Some(stat) => 
	      val elseLabel = LabelGenerator.generate()
	      val endLabel = LabelGenerator.generate()
	      X86Comment("if statement") :: codegen.CodeGenerator.iffalse(condition, elseLabel) ::: ifStatement.generateCode(0 :: current) ::: X86Jmp(endLabel) :: elseLabel :: stat.generateCode(1 :: current) ::: endLabel :: Nil
    }
  }
}

case class ReturnStatement(returnExpression: Option[Expression]) extends Statement{
  def generateCode(current:List[Int])(implicit params:List[String], pathList:List[List[Int]], cus:List[CompilationUnit]):List[X86Instruction]  = {
    implicit val impl = current
    returnExpression match {
	    case None => X86Ret :: Nil //void return?
	    case Some(expr) => expr.generateCode ::: (X86Ret :: Nil) //TODO: are we sure that eax contains the right stuff?
    }
  }
}

case class LocalVariableDeclaration(typeName: Type, identifier: String, initializer: Option[Expression]) extends Statement with VariableDeclaration{
  def generateCode(current:List[Int])(implicit params:List[String], pathList:List[List[Int]], cus:List[CompilationUnit]):List[X86Instruction] = {
    println("stat LocalVariableDeclaration: "+identifier)
    println(current)
    implicit val impl = current
    val index = pathList.indexOf(current)
    X86Comment("local variable declaration:") :: initializer.getOrElse(NullLiteral).generateCode ::: X86Mov(X86RegOffsetMemoryAccess(X86ebp, (index-2)*4), X86eax) :: Nil
  }
}

case class WhileStatement(condition: Expression, loop: Statement) extends Statement{
  def generateCode(current:List[Int])(implicit params:List[String], pathList:List[List[Int]], cus:List[CompilationUnit]):List[X86Instruction] = {
    implicit val impl = 0 :: current
    val repeatLabel = LabelGenerator.generate()
    val endLabel = LabelGenerator.generate()
    X86Comment("while statement") :: repeatLabel :: codegen.CodeGenerator.iffalse(condition, endLabel) ::: loop.generateCode(impl) ::: endLabel :: Nil
  }
}
