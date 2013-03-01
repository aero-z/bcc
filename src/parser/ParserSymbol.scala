package parser

import scanner.Token
import scanner.OperatorToken
import scanner.IdentifierToken


trait ParserSymbol

case class NonTerminalSymbol(name: String, reduceSymbols: List[ParserSymbol]) extends ParserSymbol 
