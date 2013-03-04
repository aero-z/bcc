package parser

import org.scalatest.FunSuite
import scanner._
import parser._
import scala.io.Source
import ast.AstBuilder
import ast.CheckOk

class WeedTest extends FunSuite {
	val dfa = Dfa.fromFile(Source.fromFile(new java.io.File("cfg/grammar.lr1")))
	
  def stuff(code:String) = {
    val tokens = Scanner.scan(code)
    //println("BEGIN TEST")
//	tokens.foreach(x => println(x.getClass()))
	//println("SCANNING DONE")
	//println("BUILDING DFA DONE")
	val parseTree = Parser.parse(tokens, dfa)
	//println("BUILDING PARSETREE DONE")
//	Parser.printTree(parseTree)
	val ast = AstBuilder.build(parseTree, "Foo.java")
	//println("BUILDING PARSETREE DONE")
	//Parser.printTree(ast)
	ast
	//println("Weeder result: "+Weeder.check(ast))
	//println("WEEDER CHECK DONE")
  }
  test("general construction pass") {
    val code = """
    	public class Foo {
    		public Foo() {
    			int i = 3;
    		}
    		public void foo( ) {
    			System.out.println("Hello World!");
    			int i = 3;
    		}
    	}"""
    assert(Weeder.check(stuff(code)).isInstanceOf[CheckOk])
  }
  test("class abstract XOR final 1 PASS") {
    val code = """
    	public class Foo {
    		public Foo() {}
    		public void foo( );
    	}"""
    assert(Weeder.check(stuff(code)).isInstanceOf[CheckOk])
  }
  test("class abstract XOR final 2 PASS") {
    val code = """
    	public final class Foo {
    		public Foo() {}
    		public void foo( ) { x = 5+5;}
    	}"""
    assert(Weeder.check(stuff(code)).isInstanceOf[CheckOk])
  }
  test("class abstract XOR final 3 FAIL") {
    val code = """
    	public abstract final class Foo {
    		public Foo() {}
    		 public void foo( ) ;
    	}"""
    assert(!Weeder.check(stuff(code)).isInstanceOf[CheckOk])
  }
  test("method abstract native 1 FAIL") {
    val code = """
    	public class Foo {
    		public Foo() {}
    		abstract void foo( ) { x = 5+5;} 
    	}"""
    assert(!Weeder.check(stuff(code)).isInstanceOf[CheckOk])
  }
  test("method abstract native 2 FAIL") {
    val code = """
    	public abstract class Foo {
    		public Foo() {}
    		native void foo( ) { x = 5+5;} 
    	}"""
    assert(!Weeder.check(stuff(code)).isInstanceOf[CheckOk])
  }
   test("abstract method -> not static SUCCESS") {
    val code = """
    	public abstract class Foo {
    		public Foo() {}
    		abstract void foo( ) ; 
    	}"""
    assert(Weeder.check(stuff(code)).isInstanceOf[CheckOk])
  }
   test("abstract method -> not static FAIL") {
    val code = """
    	public abstract class Foo {
    		public Foo() {}
    		abstract static void foo( ) ; 
    	}"""
    assert(Weeder.check(stuff(code)).isInstanceOf[CheckOk])
  }
    test("method abstract -> not final FAIL") {
    val code = """
    	public abstract class Foo {
    		public Foo() {}
    		abstract final void foo( ) { x = 5+5;} 
    	}"""
    assert(!Weeder.check(stuff(code)).isInstanceOf[CheckOk])
  }
    test("integer min size SUCCESS") {
    val code = """
    	public class Foo {
    		public Foo() {
    		}
    		public void foo( ) {
    			int i = -2147483648;
    		}
    	}"""
    assert(Weeder.check(stuff(code)).isInstanceOf[CheckOk])
  }
    test("integer min size FAIL") {
    val code = """
    	public class Foo {
    		public Foo() {
    			int i = -2147483649;
    		}
    	}"""
    assert(!Weeder.check(stuff(code)).isInstanceOf[CheckOk])
  }
    test("integer max size SUCCESS") {
    val code = """
    	public class Foo {
    		public Foo() {
    		}
    		public void foo( ) {
    			int i = 2147483647;
    		}
    }"""
    assert(Weeder.check(stuff(code)).isInstanceOf[CheckOk])
  }
  test("integer max size FAIL") {
    val code = """
    	public class Foo {
    		public Foo() {
    			int i = 2147483648;
    		}
    	}"""
    assert(!Weeder.check(stuff(code)).isInstanceOf[CheckOk])
  }
  test("normal cast") {
    val code = """
    	public class Foo {
    		public Foo() {
    		}
    		public void mymethod() {
    			int i = ((int))34;
    		}
    	}"""
    assert(Weeder.check(stuff(code)).isInstanceOf[CheckOk])
  }
}