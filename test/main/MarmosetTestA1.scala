package main

import org.scalatest.FunSuite
import java.io.File
import scala.io.Source

class MarmosetTestA1 extends FunSuite {
  val marmDir = new File("test/javaCode/a1marmoset")
  marmDir.listFiles.sortBy(_.getName()).foreach(file => {
    val errCode =
      if (file.getName.startsWith("Je")) Joosc.errCodeCompileErr
      else Joosc.errCodeSuccess
    test(file.getName) {
      assert(Joosc.compile((Source.fromFile(file), file.getPath) :: Nil /*StdlibFiles.stdlibFiles*/) === errCode)
    }
  })
}
