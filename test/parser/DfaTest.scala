package parser

import scala.io.Source
import scanner.IdentifierToken
import org.scalatest.FunSuite
import scanner.OperatorToken
import scanner.EndToken

class DfaTest extends FunSuite {
  test("test DFA generation") {
    //val dfa = Dfa.fromFile(Source.fromFile("test/parser/simple.lr1"))
    val dfa = Dfa.fromFile(Source.fromString(
"""3
identifier
+
$
2
S
E
S
3
S E $
E E + identifier
E identifier
6
10
2 identifier shift 1
4 + shift 2
5 + reduce 2
1 + reduce 1
3 identifier reduce 0
4 $ shift 3
0 E shift 4
1 $ reduce 1
5 $ reduce 2
0 identifier shift 5
"""
        ))
    dfa.delta(dfa.q0, IdentifierToken("foo")) match {
      case ShiftAction(state) => assert(state === 5)
    }
    dfa.delta(5, OperatorToken("+")) match {
      case ReduceAction(rule) => rule match {
        case Rule(s, n) => assert(s === "E"); assert(n === 1)
      }
    }
    dfa.delta(0, NonTerminalSymbol("E", Nil)) match {
      case ShiftAction(state) => assert(state === 4)
    }
    dfa.delta(4, OperatorToken("+")) match {
      case ShiftAction(state) => assert(state === 2)
    }
    dfa.delta(2, IdentifierToken("bar")) match {
      case ShiftAction(state) => assert(state === 1)
    }
    dfa.delta(1, EndToken()) match {
      case ReduceAction(rule) => rule match {
        case Rule(s, n) => assert(s === "E"); assert(n === 3)
      }
    }
    dfa.delta(0, NonTerminalSymbol("E", Nil)) match { // repetition
      case ShiftAction(state) => assert(state === 4)
    }
    // TODO how does this go on
    
    assert(dfa.delta(dfa.q0, OperatorToken("+")).isInstanceOf[ErrorAction])
  }

}