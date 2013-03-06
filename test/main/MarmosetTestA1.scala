package main

import org.scalatest.FunSuite
import java.io.File
import scala.io.Source

class MarmosetTestA1 extends FunSuite{
  test("Test all the java"){
    val marmDir = new File("test/javaCode/a1marmoset")
    val failure = marmDir.listFiles.sortBy(_.getName()).filter( x =>
      if(x.getName.startsWith("Je")==(42==Joosc.check((Source.fromFile(x), x.getName) :: StdlibFiles.stdlibFiles))){
        println(s"File: ${x.getName} PASSED")
        false
      } else{
        println(s"File: ${x.getName} FAILED")
        true
      }
    )
    
    println(s"Success rate : ${320- failure.size}/320 = ${100-100*failure.size/320}%")
    println("Failed test:")
    failure.foreach(x => println(x.getName))
  }
}
