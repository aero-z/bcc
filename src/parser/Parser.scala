package parser

import scanner.Token
import scanner.EndToken
import scala.io.Source._
import java.io.File._

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
  
  def readFile(path:String) {
	    val list1 = scala.io.Source.fromFile("file.txt").getLines.toList
	  	val numt = list1.head.toInt
	  	val terminals = extractTerminals(list1.tail.take(numt))
	  	
	  	val list2 = list1.tail.drop(numt)
	  	val numnt = list2.head.toInt
	  	val nonterminals = extractNonTerminals(list.tail.take(numnt))
	  	
	  	val list3 = list2.tail.drop(numnt)
		val state = ("""(\w+) (\w+) (\w+) (\w+)""").r;
		Source.fromString(path).getLines().map(_ match {
		  case state(state, symbol, action, nextState) => (state.toInt, symbol, action, nextState.toInt);
		})
  }
  def extractTerminals(lines:List[String]) = lines.map(_ => TerminalSymbol(_))
  def extractNonTerminals(lines:List[String]) = lines.map(_ => NonTerminalSymbok(_))
  
}