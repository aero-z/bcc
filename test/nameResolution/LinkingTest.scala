package nameResolution

import scala.io.Source
import org.scalatest.FunSuite
import scanner._
import parser._
import ast._

class Linking extends FunSuite {
  val classes = List(
("C1","""
package pk1;

import pk2.C1;

class C1 {
	public C1() {}
	public void foo() {
		C1 f = new C1();
		C2 a;
	}
}
"""),
("C2", """
package pk2;

class C2 {
	public C2() {}
}
""")
)
  def printAddress(cu:CompilationUnit) =
  	println("XXXXX "+cu.fileName+": "+cu.typeDef.get.hashCode)
  def createCU(string:String, name:String):CompilationUnit = {
		//println("BEGIN TEST")
	    val tokens = Scanner.scan(string)
		//tokens.foreach(x => println(x.getClass()))
		//println("SCANNING DONE")
		val dfa = Dfa.fromFile(Source.fromFile(new java.io.File("cfg/grammar.lr1")))
		//println("BUILDING DFA DONE")
		val parseTree:ParserSymbol = Parser.parse(tokens, dfa)
		//println("BUILDING PARSETREE DONE")
		//Parser.printTree(parseTree)
	    //println("BUILDING AST TREE")
	    val ast = AstBuilder.build(parseTree: ParserSymbol, name)
	    println("DONE CREATING THE Compilation Unit!")
	    ast
	}
    test("TypeLinking") {
		val list = classes.map(x => createCU(x._2, x._1))
	    list.foreach(printAddress(_))
	    try {
	      val linked = TypeLinking.treatAll(list)
	      linked.foreach(printAddress(_))
	    } catch {case e:Exception => println("The exception: "+e.toString()+" "+e.getMessage())}
	}
}