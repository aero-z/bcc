package parser

import scanner.Token
import scanner.EndToken

object Parser {
  def reduce(stack: List[Symbol]) : List[Symbol] = {
    ???
  }
  
  def parse(stack: List[Symbol], input: List[Token], dfa: Dfa): ParseTree = {

    val (newDfa, action) = dfa.next(stack.head)
    action match {
      case Action.Shift =>
      	parse(TerminalSymbol(input.head) :: stack, input.tail, newDfa)
      case Action.Reduce =>
        parse(reduce(stack), input, newDfa)
    }
  }
}