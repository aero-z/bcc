package parser

import scanner.Token
import scanner.EndToken

object Parser {
  def reduce(stack: List[Symbol], rule: Rule) : List[Symbol] = {
    ???
  }
  
  def parse(stack: List[Symbol], input: List[Symbol], dfa: Dfa): ParseTree = {

    /*val (newDfa, action) = dfa.next(stack.head)
    action match {
      case ShiftAction() =>
      	parse(input.head :: stack, input.tail, newDfa)
      case ReduceAction(rule) =>
        parse(reduce(stack, ???), input, newDfa)
    }*/
    ???
  }
}