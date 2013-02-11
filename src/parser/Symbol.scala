package parser

import scanner.Token
import scanner.OperatorToken
import scanner.IdentifierToken

trait Symbol

abstract class NonTerminalSymbol extends Symbol

case class Expression() extends NonTerminalSymbol
case class Variable(id: IdentifierToken) extends NonTerminalSymbol
case class Operation(op: OperatorToken, args: List[Expression]) extends NonTerminalSymbol