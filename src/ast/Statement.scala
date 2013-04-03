package ast

import codegen.X86Instruction

sealed abstract class Statement extends AstNode{
  def generateCode:List[X86Instruction]
}

case class Block (statements : List[Statement]) extends Statement{
  def generateCode:List[X86Instruction] = Nil
}



case object EmptyStatement extends Statement{
  def generateCode:List[X86Instruction] = Nil
}



case class ExpressionStatement(expression: Expression) extends Statement{
  def generateCode:List[X86Instruction]  = Nil
}



case class ForStatement(init: Option[Statement], condition: Option[Expression], incrementation: Option[Expression], loop: Statement) extends Statement{
  def generateCode:List[X86Instruction] = Nil
}



case class IfStatement(condition: Expression, ifStatement: Statement, elseStatement: Option[Statement]) extends Statement{
  def generateCode:List[X86Instruction]  = Nil
}



case class ReturnStatement(returnExpression: Option[Expression]) extends Statement{
  def generateCode:List[X86Instruction]  = Nil
}



case class LocalVariableDeclaration(typeName: Type, identifier: String, initializer: Option[Expression]) extends Statement with VariableDeclaration{
  def generateCode:List[X86Instruction] = Nil
}




case class WhileStatement(condition: Expression, loop: Statement) extends Statement{
  def generateCode:List[X86Instruction] = Nil
}
