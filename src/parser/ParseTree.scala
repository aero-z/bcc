package parser

class ParseTree

class Statement extends ParseTree
class Expression extends ParseTree
class Variable(name: String) extends Expression
class Function(name: String) extends Expression
class Assignment(assignee: Expression, value: Expression) extends Statement
class Operation(function: Expression, parameters: List[Expression]) extends Expression
class Literal extends Expression