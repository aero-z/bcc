package parser

import scanner.Token
import scanner.EndToken
import main.CompilerError
import scala.annotation.tailrec

object Parser {

  val debugEnabled = false
  def debug(a: Any) = if (debugEnabled) println(a)

  def parse(input: List[Token], dfa: Dfa): Symbol = {
    @tailrec
    def parseRec(stack: List[(Symbol, Dfa.State)], input: List[Symbol]): Symbol = {
      dfa.delta(stack.head._2, input.head) match {
        case ShiftAction(state) =>
          input.head match {
            case EndToken() =>
              assert(stack.length == 2); stack.head._1
            case x =>
              debug("shift: " + x) //TESTING
              parseRec((input.head, state) :: stack, input.tail)
          }
        case ReduceAction(rule) =>
          debug(rule.nonTerminalSymbol)
          val (stackRemain, newSymbol) = reduce(stack, rule);
          parseRec(stackRemain, newSymbol :: input)
        case ErrorAction() =>
          throw new CompilerError(s"invalid input: ${input.head} state: ${stack.head._2}")
      }
    }
    def reduce(stack: List[(Symbol, Dfa.State)], rule: Rule): (List[(Symbol, Dfa.State)], Symbol) = {
      (
        stack drop (rule.numOfSym),
        NonTerminalSymbol(rule.nonTerminalSymbol, stack take (rule.numOfSym) reverseMap (_._1))
      )
    }
    parseRec(List((null, 0)), input)
  }

  def printTree(symbol: Symbol) {
    def printRec(delim: String, tree: List[Symbol]) {
      tree.foreach(
        _ match {
          case NonTerminalSymbol(name, list) =>
            println(delim + name); printRec(delim + "    ", list)
          case x => println(delim +x.getClass().getSimpleName()+": "+x)
        })
    }
    printRec("", List(symbol))
  }
}
