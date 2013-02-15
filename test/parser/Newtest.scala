package parser

import scala.io.Source
import org.scalatest.FunSuite
import scanner._
import parser._

class Newtest extends FunSuite{
  val myCode = """Êpublic class Foo {
 Ê Êpublic void foo() {
 Ê Ê Êprintln("hello")
 Ê Ê}
 Ê}"""
  test("Everything") {
    val tokens = Scanner.scan(myCode)
	tokens.foreach(x => println(x.getClass()))
	println("SCANNING DONE")
	val dfa = Dfa.fromFile(Source.fromFile(new java.io.File("cfg/eclipse.cfg")))
	println("BUILDING DFA DONE")
	val parseTree = Parser.parse(tokens, dfa)
	println("BUILDING PARSETREE DONE")
	Parser.printTree(parseTree)
  } 
}