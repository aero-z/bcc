package main

import org.scalatest.FunSuite
import scala.io.Source

class MainTest extends FunSuite {
  test("invalid code") {
    val code = Source.fromString(
"""
  [ } this is shit
""")
    val ret = Joosc.check(code)
    assert(ret === Joosc.errCodeParseErr)
  }
  
  test("simple code") {
    val code = Source.fromString(
"""
 class Foo {
  public Foo() {
   println("hello");
  }
 }""")
    val ret = Joosc.check(code)
    assert(ret === Joosc.errCodeSuccess)
  }
  
  /*
  test("invalid escape seq") {
    val code = Source.fromString(
"""
 class Foo {
  public void foo() {
   println("he\llo");
  }
 }""")
    val ret = Joosc.check(code)
    assert(ret === Joosc.errCodeParseErr)
  }
  */
  
  test("invalid ascii char") {
    val code = Source.fromString(
"""
 class Foo {
  public Foo() {
   println("hel""" + 200.toChar + """lo");
  }
 }""")
    val ret = Joosc.check(code)
    assert(ret === Joosc.errCodeParseErr)
  }
  
  test("fancy test") {
    val code = Source.fromString(
"""
public class Test {
    public Test() {}
    public static int test() {	
		boolean b = true;
		boolean e = false;
		Object a = new Test();
		boolean c = e || a instanceof Test;
		boolean d = b && a instanceof Test;	
		if (c && d){
		    return 123;
		}
		else {
		    return 12378;
		}
    }
}
""")
    val ret = Joosc.check(code)
    assert(ret === Joosc.errCodeSuccess)
  }
}