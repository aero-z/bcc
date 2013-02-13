package parser

import scala.io.Source
import scanner.IdentifierToken

import org.scalatest.FunSuite

class CedricTest extends FunSuite {
  test("test DFA generation") {
    val dfa = Dfa.fromFile(Source.fromFile("test/parser/simple.lr1"))
    dfa.delta(dfa.q0, IdentifierToken("foo")) match {
      case ShiftAction(state) => ???
      case _ => fail("hoi")
    }
  }

}