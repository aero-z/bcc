package parser

import org.scalatest.FunSuite
import scanner._
import parser._
import scala.io.Source

class WeedTest extends FunSuite {
	val dfa = Dfa.fromFile(Source.fromFile(new java.io.File("cfg/grammar.lr1")))
	
  def stuff(code:String):Symbol = {
    val tokens = Scanner.scan(code)
    //println("BEGIN TEST")
	tokens.foreach(x => println(x.getClass()))
	//println("SCANNING DONE")
	//println("BUILDING DFA DONE")
	val parseTree = Parser.parse(tokens, dfa)
	//println("BUILDING PARSETREE DONE")
	Parser.printTree(parseTree)
	val ast = Ast.createAst(parseTree)
	//println("BUILDING PARSETREE DONE")
	Parser.printTree(ast)
	ast
	//println("Weeder result: "+Weeder.astcheck(ast))
	//println("WEEDER CHECK DONE")
  }
  test("class abstract XOR final 1 PASS") {
    val code = """
    	public class Foo {
    		public Foo() {}
    		public void foo( );
    	}"""
    assert(Weeder.astcheck(stuff(code)))
  }
  test("class abstract XOR final 2 PASS") {
    val code = """
    	public final class Foo {
    		public Foo() {}
    		public void foo( ) { x = 5+5;}
    	}"""
    assert(Weeder.astcheck(stuff(code)))
  }
  test("class abstract XOR final 3 FAIL") {
    val code = """
    	public abstract final class Foo {
    		public Foo() {}
    		 public void foo( ) ;
    	}"""
    assert(!Weeder.astcheck(stuff(code)))
  }
  test("method abstract native 1 FAIL") {
    val code = """
    	public class Foo {
    		public Foo() {}
    		abstract void foo( ) { x = 5+5;} 
    	}"""
    assert(!Weeder.astcheck(stuff(code)))
  }
  test("method abstract native 2 FAIL") {
    val code = """
    	public abstract class Foo {
    		public Foo() {}
    		native void foo( ) { x = 5+5;} 
    	}"""
    assert(!Weeder.astcheck(stuff(code)))
  }
}