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
    "test/javaCode/stdlib/2.0/java/io/OutputStream.java").map(f => (Source.fromFile(f), f))
    
  
  {
    def getFiles(file: File, depth: Int): Seq[File] = {
      if (file.isDirectory() && depth != 0)
        file.listFiles().sortBy(_.getName()).flatMap(getFiles(_, depth-1))
      else
        file :: Nil
    }
    
    val marmDir = new File("test/javaCode/a2marmoset")
    val testCaseFiles = getFiles(marmDir, 1)
    testCaseFiles.foreach(file => {
      val testSources = getFiles(file, -1).map(f => (Source.fromFile(f), f.getPath))
      /*println("Files:")
      testSources.foreach(x=> println("- " + x._2))
      println("- stdlib files")*/
      test(file.getName) {
        assert(Joosc.check(testSources ++: stdlibFiles) === main.Joosc.errCodeSuccess)
      }
    })
  }
}
