package parser

import scanner.Token
import scanner.EndToken
import scala.io.Source._
import java.io.File._

object Parser {

    def parse(input: List[Token]): Symbol = {
        val dfa = Dfa.fromFile(null); //TODO remove this hack...
        def parseRec(stack: List[(Symbol, dfa.State)], input: List[Symbol]): Symbol = {
            input match {
                case NonTerminalSymbol("S", _) :: Nil => input head
                case _ => dfa.delta(stack.head._2, input.head) match {
                    case ShiftAction(state) => parseRec((input.head, state) :: stack, input.tail)
                    case ReduceAction(rule) =>
                        val (stackRemain, newSymbol) = reduce(stack, rule);
                        parseRec(stackRemain, newSymbol :: input)
                }
            }
        }
        def reduce(stack: List[(Symbol, dfa.State)], rule: Rule): (List[(Symbol, dfa.State)], Symbol) = {
            (stack drop (rule.numOfSym), NonTerminalSymbol(rule.nonTerminalSymbol, stack take (rule.numOfSym) map (_._1)))
        }
        parseRec(List((null, 0)), input)
    }
}
