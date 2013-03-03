package abstractSyntaxTree

abstract class Statement extends AstNode

case class Block (statements : List[Statement]) extends Statement
case object EmptyStatement extends Statement
case class ExpressionStatement(expression: Expression) extends Statement
case class ForStatement(init: Option[Statement], condition: Option[Expression], incrementation: Option[Expression], loop: Statement) extends Statement
case class IfStatement(condition: Expression, ifStatement: Statement, elseStatement: Option[Statement]) extends Statement
case class ReturnStatement(returnExpression: Option[Expression]) extends Statement
case class LocalVariableDeclaration(typeName: Type, identifier: String, initializer: Option[Expression]) extends Statement with VariableDeclaration
case class WhileStatement(condition: Expression, loop: Statement) extends Statement
