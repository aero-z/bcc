package parser

import scala.io.Source
import org.scalatest.FunSuite
import scanner._
import parser._

class Newtest extends FunSuite{
  val myCode = """
  abstract class Foo {
  public void foo() {
      red(pink);
}
 }"""
  test("Everything") {
	println("BEGIN TEST")
    val tokens = Scanner.scan(myCode)
	tokens.foreach(x => println(x.getClass()))
	println("SCANNING DONE")
	val dfa = Dfa.fromFile(Source.fromFile(new java.io.File("cfg/grammar.lr1")))
	println("BUILDING DFA DONE")
	val parseTree = Parser.parse(tokens, dfa)
	println("BUILDING PARSETREE DONE")
	Parser.printTree(parseTree)
	assert(Weeder.check(parseTree))
  }
}
