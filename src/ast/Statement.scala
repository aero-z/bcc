package ast

import codegen._
import ast.Literal

sealed abstract class Statement extends AstNode{
  def generateCode:List[X86Instruction]
}

case class Block (statements : List[Statement]) extends Statement{
  def generateCode:List[X86Instruction] = statements.flatMap(_.generateCode)
}

case object EmptyStatement extends Statement{
  def generateCode:List[X86Instruction] = X86Comment("-----") :: Nil
}

case class ExpressionStatement(expression: Expression) extends Statement{
  def generateCode:List[X86Instruction]  = expression.generateCode
}

case class ForStatement(init: Option[Statement], condition: Option[Expression], incrementation: Option[Expression], loop: Statement) extends Statement{
  def generateCode:List[X86Instruction] = {
    val repeatLabel = LabelGenerator.generate
    val endLabel = LabelGenerator.generate
    X86Comment("for statement") :: repeatLabel :: init.getOrElse(EmptyStatement).generateCode ::: codegen.CodeGenerator.iffalse(condition.getOrElse(BooleanLiteral(true)), endLabel) ::: loop.generateCode ::: X86Jmp(repeatLabel) :: endLabel :: Nil
  }
}

case class IfStatement(condition: Expression, ifStatement: Statement, elseStatement: Option[Statement]) extends Statement{
  def generateCode:List[X86Instruction] = elseStatement match {
    case None =>
      val endLabel = LabelGenerator.generate
      X86Comment("IfStatement") :: codegen.CodeGenerator.iffalse(condition, endLabel) ::: ifStatement.generateCode ::: endLabel :: Nil
    case Some(stat) => 
      val elseLabel = LabelGenerator.generate
      val endLabel = LabelGenerator.generate
      X86Comment("if statement") :: codegen.CodeGenerator.iffalse(condition, elseLabel) ::: ifStatement.generateCode ::: X86Jmp(endLabel) :: elseLabel :: stat.generateCode ::: endLabel :: Nil
  }
}

case class ReturnStatement(returnExpression: Option[Expression]) extends Statement{
  def generateCode:List[X86Instruction]  = returnExpression match {
    case None => X86Ret :: Nil //void return?
    case Some(expr) => expr.generateCode ::: (X86Ret :: Nil) //TODO: are we sure that eax contains the right stuff?
  }
}

case class LocalVariableDeclaration(typeName: Type, identifier: String, initializer: Option[Expression]) extends Statement with VariableDeclaration{
  def generateCode:List[X86Instruction] = {
    //TODO: Bastien will implement this!
    X86Comment("local variable declaration") :: Nil
  }
}

case class WhileStatement(condition: Expression, loop: Statement) extends Statement{
  def generateCode:List[X86Instruction] = {
    val repeatLabel = LabelGenerator.generate
    val endLabel = LabelGenerator.generate
    X86Comment("while statement") :: repeatLabel :: codegen.CodeGenerator.iffalse(condition, endLabel) ::: loop.generateCode ::: endLabel :: Nil
  }
}
