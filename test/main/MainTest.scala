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
  public void foo() {
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
  public void foo() {
   println("hel""" + 200.toChar + """lo");
  }
 }""")
    val ret = Joosc.check(code)
    assert(ret === Joosc.errCodeParseErr)
  }
}