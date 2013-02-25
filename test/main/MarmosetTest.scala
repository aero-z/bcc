package main

import org.scalatest.FunSuite
import java.io.File
import scala.io.Source

class MarmosetTest extends FunSuite{
    test("Test all the java"){
        val marmDir = new File("test/javaCode/a1marmoset")
        val success = marmDir.listFiles.sortBy(_.getName()).filter( x =>
            if(x.getName.startsWith("Je")==(42==Joosc.check(Source.fromFile(x), x.getPath))){
                println(s"File: ${x.getName} PASSED")
                true
            } else{
                println(s"File: ${x.getName} FAILED")
                false
            }
        ).size
        
        println(s"Success rate : $success/320 = ${100*success/320}%")
    }
}