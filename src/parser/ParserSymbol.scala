package parser

trait ParserSymbol

case class NonTerminalSymbol(name: String, reduceSymbols: List[ParserSymbol]) extends ParserSymbol 
