package parser

import scala.io.Source
import org.scalatest.FunSuite
import scanner._
import parser._
import ast.AstBuilder

class Newtest extends FunSuite{
  // NOTE: this test is meant for playing around and testing manually
  //       it doesn't test any additional functionality other tests test
  val myCode =  """
    	public class Foo {
    		public void Foo() {
		  		int i = 500;
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
	val ast = AstBuilder.build(parseTree, "Foo.java")
	println("BUILDING PARSETREE DONE")
	//Parser.printTree(ast)
	println("Weeder result: "+Weeder.check(ast))
	println("WEEDER CHECK DONE")
  }
}
