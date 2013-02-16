package parser

import scala.io.Source
import org.scalatest.FunSuite
import scanner._
import parser._

class Newtest extends FunSuite{
  val myCode = """
  final class Foo {
  public void foo( ) { x = 5+5;}
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
	val ast = AST.creteAST(parseTree)
	println("BUILDING PARSETREE DONE")
	Parser.printTree(ast)
	println("Weeder result: "+Weeder.astcheck(ast))
	println("WEEDER CHECK DONE")
  }
}
