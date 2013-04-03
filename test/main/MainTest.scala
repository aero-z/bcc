package main

import org.scalatest.FunSuite
import scala.io.Source

class MainTest extends FunSuite {
  test("fancy test") {
    val code = Source.fromString(
"""
public class classe {
    public static int intega = 5;
    public classe() {}
}

""")
    val ret = Joosc.check((code, "classe.java")::Nil)// :: StdlibFiles.stdlibFiles)
    assert(ret === Joosc.errCodeSuccess)
  }
  /*
  test("invalid code") {
    val code = Source.fromString(
"""
  [ } this is shit
""")
    val ret = Joosc.check((code, "Foo.java") :: StdlibFiles.stdlibFiles)
    assert(ret === Joosc.errCodeCompileErr)
  }

  test("simple code") {
    val code = Source.fromString(
"""
 public class Foo {
  public Foo() {
   char c = 'c';
  }
 }""")
    val ret = Joosc.check((code, "Foo.java") :: Nil)
    assert(ret === Joosc.errCodeSuccess)
  }
  
  test("invalid escape seq") {
    val code = Source.fromString(
"""
 public class Foo {
  public void foo() {
   println("he\llo");
  }
 }""")
    val ret = Joosc.check((code, "Foo.java") :: Nil)
    assert(ret === Joosc.errCodeCompileErr)
  }
  
  test("invalid char literal") {
    val code = Source.fromString(
"""
 public class Foo {
  public void foo() {
    char c = 'haa';
  }
 }""")
    val ret = Joosc.check((code, "Foo.java") :: Nil)
    assert(ret === Joosc.errCodeCompileErr)
  }
  
  test("escape seq in string") {
    val code = Source.fromString(
"""
 public class Foo {
  public void foo() {
   String s = "he\"l\nlo";
  }
 }""")
    val ret = Joosc.check((code, "Foo.java") :: Nil)
    assert(ret === Joosc.errCodeSuccess)
  }
  test("invalid ascii char") {
    val code = Source.fromString(
"""
 public class Foo {
  public Foo() {
   println("hel""" + 200.toChar + """lo");
  }
 }""")
    val ret = Joosc.check((code, "Foo.java") :: Nil)
    assert(ret === Joosc.errCodeCompileErr)
  }*/
}