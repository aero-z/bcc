package parser

import scanner.Token

abstract class Symbol

case class TerminalSymbol(token: Token)  extends Symbol
case class NonTerminalSymbol(id: String) extends Symbol