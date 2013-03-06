package main

import org.scalatest.FunSuite
import java.io.File
import scala.io.Source

class MarmosetTestA2 extends FunSuite {
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
    "test/javaCode/stdlib/2.0/java/io/OutputStream.java").map(Source.fromFile(_))
    
  test("Test all the java") {
    // TODO: run directories!!!
    val marmDir = new File("test/javaCode/a2marmoset")
    val failure = marmDir.listFiles.sortBy(_.getName()).filter(_.isFile()).filter(x => {
      val success = Joosc.check(Source.fromFile(x) :: stdlibFiles) == main.Joosc.errCodeSuccess
      if (x.getName.startsWith("Je") != success) {
        println(s"File: ${x.getName} PASSED")
        false
      } else {
        println(s"File: ${x.getName} FAILED")
        true
      }
    })

    println(s"Success rate : ${320 - failure.size}/320 = ${100 - 100 * failure.size / 320}%")
    println("Failed test:")
    failure.foreach(x => println(x.getName))
  }
}
