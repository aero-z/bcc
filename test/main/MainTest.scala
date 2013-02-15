package main

import org.scalatest.FunSuite
import scala.io.Source

class MainTest extends FunSuite {
  test("invalid code") {
    val code = Source.fromString(
"""
  [ } this is shit
""")
    val ret = Joosc.compile(code)
    assert(ret === Joosc.errCodeParseErr)
  }
  
  test("simple code") {
    val code = Source.fromString(
"""
  public class Foo {
  }
""")
    val ret = Joosc.compile(code)
    assert(ret === Joosc.errCodeSuccess)
  }
}