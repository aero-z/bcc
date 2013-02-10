package parser

import scanner.Token
import scanner.EndToken

object Parser {
    def reduce(stack: List[Symbol], rule: Rule): List[Symbol] = {
        ???
    }

    def parse(input: List[Token]): ParseTree = {
        val dfa = Dfa.fromFile(null) //TODO remove this hack...
        def parseRec(stack: List[Symbol], input: List[Symbol], state: dfa.State): ParseTree = {

            /*val (newDfa, action) = dfa.next(stack.head)
    action match {
      case ShiftAction() =>
        parse(input.head :: stack, input.tail, newDfa)
      case ReduceAction(rule) =>
        parse(reduce(stack, ???), input, newDfa)
    }*/
            ???
        }
        parseRec(Nil, input, 0)
    }

}