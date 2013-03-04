package abstractSyntaxTree

sealed abstract class Statement extends AstNode

case class Block (statements : List[Statement]) extends Statement {
  val children = statements
}
case object EmptyStatement extends Statement {
  val children = Nil
}
case class ExpressionStatement(expression: Expression) extends Statement {
  val children = expression :: Nil
}

case class ForStatement(init: Option[Statement], condition: Option[Expression], incrementation: Option[Expression], loop: Statement) extends Statement {
  val children = loop :: init ::: condition ::: incrementation
}
case class IfStatement(condition: Expression, ifStatement: Statement, elseStatement: Option[Statement]) extends Statement {
  val children = condition :: ifStatement :: elseStatement
}
case class ReturnStatement(returnExpression: Option[Expression]) extends Statement {
  val children = returnExpression ::: Nil
}
case class LocalVariableDeclaration(typeName: Type, identifier: String, initializer: Option[Expression]) extends Statement with VariableDeclaration {
  val children = initializer ::: Nil
}
case class WhileStatement(condition: Expression, loop: Statement) extends Statement {
  val children = condition :: loop :: Nil
}
