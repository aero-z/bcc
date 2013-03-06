package main

import org.scalatest.FunSuite
import scala.io.Source

class StdlibTest extends FunSuite {
  val stdlibSources = List(
    "test/javaCode/stdlib/2.0/java/lang/System.java",
    "test/javaCode/stdlib/2.0/java/lang/Cloneable.java",
    "test/javaCode/stdlib/2.0/java/lang/Short.java",
    "test/javaCode/stdlib/2.0/java/lang/Byte.java",
    "test/javaCode/stdlib/2.0/java/lang/Number.java",
    "test/javaCode/stdlib/2.0/java/lang/Integer.java",
    "test/javaCode/stdlib/2.0/java/lang/Character.java",     
    "test/javaCode/stdlib/2.0/java/io/Serializable.java",
    "test/javaCode/stdlib/2.0/java/io/PrintStream.java",
    "test/javaCode/stdlib/2.0/java/io/OutputStream.java",
    "test/javaCode/stdlib/2.0/java/util/Arrays.java",
    "test/javaCode/stdlib/2.0/java/lang/String.java",
    "test/javaCode/stdlib/2.0/java/lang/Class.java",
    "test/javaCode/stdlib/2.0/java/lang/Object.java"
).map(x => (Source.fromFile(x), x))

  test("stdlib test") {
    val code = Source.fromString(
"""
import java.lang.Object;

 public class Foo {
  public Foo() {
   Object o;
   println("hello");
  }
 }
 """)
    assert(Joosc.check((code, "Foo.java") :: stdlibSources) == main.Joosc.errCodeSuccess)
 }
}
