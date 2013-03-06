package main

import scala.io.Source

object StdlibFiles {
  val stdlibFiles = List(
    "test/javaCode/stdlib/2.0/java/lang/System.java",
    "test/javaCode/stdlib/2.0/java/lang/Object.java",
    "test/javaCode/stdlib/2.0/java/lang/Class.java",
    "test/javaCode/stdlib/2.0/java/lang/Cloneable.java",
    "test/javaCode/stdlib/2.0/java/lang/Short.java",
    "test/javaCode/stdlib/2.0/java/lang/String.java",
    "test/javaCode/stdlib/2.0/java/lang/Byte.java",
    "test/javaCode/stdlib/2.0/java/lang/Number.java",
    "test/javaCode/stdlib/2.0/java/lang/Integer.java",
    "test/javaCode/stdlib/2.0/java/lang/Character.java",
    "test/javaCode/stdlib/2.0/java/util/Arrays.java",
    "test/javaCode/stdlib/2.0/java/io/Serializable.java",
    "test/javaCode/stdlib/2.0/java/io/PrintStream.java",
    "test/javaCode/stdlib/2.0/java/io/OutputStream.java"
  ).map(f => (Source.fromFile(f), f))
}