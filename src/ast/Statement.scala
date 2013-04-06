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
  def generateCode:List[X86Instruction] = Nil
}

case class ExpressionStatement(expression: Expression) extends Statement{
  def generateCode:List[X86Instruction]  = expression.generateCode
}

case class ForStatement(init: Option[Statement], condition: Option[Expression], incrementation: Option[Expression], loop: Statement) extends Statement{
  def generateCode:List[X86Instruction] = {
    val repeatLabel = LabelGenerator.generate
    val endLabel = LabelGenerator.generate
    //codegen.CodeGenerator.iffalse(init.getOrElse(ExpressionStatement(BooleanLiteral(true).asInstanceOf[Expression]).asInstanceOf[Statement]), endLabel) ::: loop.generateCode ::: X86Jmp(repeatLabel) :: endLabel :: Nil
    return null;
  }
}

case class IfStatement(condition: Expression, ifStatement: Statement, elseStatement: Option[Statement]) extends Statement{
  def generateCode:List[X86Instruction] = elseStatement match {
    case None =>
      val endLabel = LabelGenerator.generate
      codegen.CodeGenerator.iffalse(condition, endLabel) ::: endLabel :: Nil
    case Some(stat) => 
      val elseLabel = LabelGenerator.generate
      val endLabel = LabelGenerator.generate
      codegen.CodeGenerator.iffalse(condition, elseLabel) ::: X86Jmp(endLabel) :: elseLabel :: stat.generateCode ::: endLabel :: Nil
  }
}

case class ReturnStatement(returnExpression: Option[Expression]) extends Statement{
  def generateCode:List[X86Instruction]  = returnExpression match {
    case None => X86Ret :: Nil //void return?
    case Some(expr) => expr.generateCode ::: (X86Ret :: Nil)
  }
}

case class LocalVariableDeclaration(typeName: Type, identifier: String, initializer: Option[Expression]) extends Statement with VariableDeclaration{
  def generateCode:List[X86Instruction] = Nil
}

case class WhileStatement(condition: Expression, loop: Statement) extends Statement{
  def generateCode:List[X86Instruction] = Nil
}
