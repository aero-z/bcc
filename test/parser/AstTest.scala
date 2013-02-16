package parser

import scala.io.Source
import org.scalatest.FunSuite
import scanner._
import parser._

class AstTest extends FunSuite {
  val myCode = """
 abstract public static class Foo {
  public void foo() {
   println("hello");
    if (var == 5) { method(shit); }
 }
}"""
  test("AST-test") {
	println("BEGIN TEST")
    val tokens = Scanner.scan(myCode)
	tokens.foreach(x => println(x.getClass()))
	println("SCANNING DONE")
	val dfa = Dfa.fromFile(Source.fromFile(new java.io.File("cfg/grammar.lr1")))
	println("BUILDING DFA DONE")
	val parseTree:Symbol = Parser.parse(tokens, dfa)
	println("BUILDING PARSETREE DONE")
	Parser.printTree(parseTree)
    println("BUILDING AST TREE")
    //val node = AST.toNode(parseTree)
    val ast = Ast.createAst(parseTree)
    Parser.printTree(ast)
    // TODO: do some automatic verification on the tree
  }
}