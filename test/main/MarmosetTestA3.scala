package main

import org.scalatest.FunSuite
import java.io.File
import scala.io.Source

class MarmosetTestA3 extends FunSuite {

  def getFiles(file: File, depth: Int): Seq[File] = {
    if (file.isDirectory() && depth != 0)
      file.listFiles().sortBy(_.getName()).flatMap(getFiles(_, depth - 1))
    else
      file :: Nil
  }

  val marmDir = new File("test/javaCode/a3marmoset")
  val testCaseFiles = getFiles(marmDir, 1)
  testCaseFiles.foreach(file => {
    val testSources = getFiles(file, -1).map(f => (Source.fromFile(f), f.getPath))
    /*
    println("Files:")
    testSources.foreach(x=> println("- " + x._2))
    println("- stdlib files")
    */
    val errCode =
      if (file.getName.startsWith("Je")) Joosc.errCodeParseErr
      else                               Joosc.errCodeSuccess
    test(file.getName) {
      assert(Joosc.check(testSources.toList) === errCode)
    }
  })

}
