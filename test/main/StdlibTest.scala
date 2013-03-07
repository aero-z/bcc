package main

import org.scalatest.FunSuite
import scala.io.Source

class StdlibTest extends FunSuite {

  test("stdlib test") {
    val code = Source.fromString(
"""

 public class Foo {
  public Foo() {
   Object o;
   println("hello");
  }
 }
 """)
    assert(Joosc.check((code, "Foo.java") :: Nil/*StdlibFiles.stdlibFiles*/) == main.Joosc.errCodeSuccess)
  }
}
