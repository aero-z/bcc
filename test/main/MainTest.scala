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
}