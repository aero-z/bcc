package parser

import org.scalatest.FunSuite
import scanner._
import parser._
import scala.io.Source
import ast.AstBuilder
import ast.WeedOk
import main.CompilerError

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
    stuff(code)
  }
  test("class abstract XOR final 1 PASS") {
    val code = """
    	public class Foo {
    		public Foo() {}
    		public void foo( ) {}
    	}"""
    stuff(code)
  }
  test("class abstract XOR final 2 PASS") {
    val code = """
    	public final class Foo {
    		public Foo() {}
    		public void foo( ) { x = 5+5;}
    	}"""
    stuff(code)
  }
  test("class abstract XOR final 3 FAIL") {
    val code = """
    	public abstract final class Foo {
    		public Foo() {}
    		 public void foo( ) ;
    	}"""
    intercept[CompilerError] {
      stuff(code)
    }
  }
  test("method abstract native 1 FAIL") {
    val code = """
    	public class Foo {
    		public Foo() {}
    		abstract void foo( ) { x = 5+5;} 
    	}"""
    intercept[CompilerError] {
      stuff(code)
    }
  }
  test("method abstract native 2 FAIL") {
    val code = """
    	public abstract class Foo {
    		public Foo() {}
    		native void foo( ) { x = 5+5;} 
    	}"""
    intercept[CompilerError] {
      stuff(code)
    }
  }
   test("abstract method -> not static SUCCESS") {
    val code = """
    	public abstract class Foo {
    		public Foo() {}
    		public abstract void foo( ) ; 
    	}"""
    stuff(code)
  }
   test("abstract method -> not static FAIL") {
    val code = """
    	public abstract class Foo {
    		public Foo() {}
    		public abstract static void foo( ) ; 
    	}"""
    intercept[CompilerError] {
      stuff(code)
    }
  }
    test("method abstract -> not final FAIL") {
    val code = """
    	public abstract class Foo {
    		public Foo() {}
    		abstract final void foo( ) { x = 5+5;} 
    	}"""
    intercept[CompilerError] {
      stuff(code)
    }
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
    stuff(code)
  }
    test("integer min size FAIL") {
    val code = """
    	public class Foo {
    		public Foo() {
    			int i = -2147483649;
    		}
    	}"""
    intercept[CompilerError] {
      stuff(code)
    }
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
    stuff(code)
  }
  test("integer max size FAIL") {
    val code = """
    	public class Foo {
    		public Foo() {
    			int i = 2147483648;
    		}
    	}"""
    intercept[CompilerError] {
      stuff(code)
    }
  }
}