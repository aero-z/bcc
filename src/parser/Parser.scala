package parser

import scanner.Token
import scanner.EndToken

class ParseException(str: String) extends Exception(str)

object Parser {

    def parse(input: List[Token], dfa: Dfa): Symbol = {
        def parseRec(stack: List[(Symbol, Dfa.State)], input: List[Symbol]): Symbol = {
            dfa.delta(stack.head._2, input.head) match {
	            case ShiftAction(state) =>
	            	input.head match {
	            	  case EndToken() => assert(stack.size == 2); stack.head._1
	            	  case _ =>	parseRec((input.head, state) :: stack, input.tail)
	            	}
	            case ReduceAction(rule) =>
	                val (stackRemain, newSymbol) = reduce(stack, rule);
	                parseRec(stackRemain, newSymbol :: input)
	            case ErrorAction() =>
	                throw new ParseException("invalid input")
            }
        }
        def reduce(stack: List[(Symbol, Dfa.State)], rule: Rule): (List[(Symbol, Dfa.State)], Symbol) = {
            (stack drop (rule.numOfSym), NonTerminalSymbol(rule.nonTerminalSymbol, stack take (rule.numOfSym) map (_._1)))
        }
        parseRec(List((null, 0)), input)
    }
}
