package main

import org.scalatest.FunSuite
import java.io.File
import scala.io.Source
import scala.sys.process._
class MarmosetTestA5 extends FunSuite {

  def getFiles(file: File, depth: Int): Seq[File] = {
    if (file.isDirectory() && depth != 0)
      file.listFiles().sortBy(_.getName()).flatMap(getFiles(_, depth - 1))
    else
      file :: Nil
  }
  //Check if output dir is created and clean it...
  val outputDir = new File("test/javaCode/a5marmoset/output")
  outputDir.mkdir
  outputDir.listFiles.foreach(_.delete) 

  val marmDir = new File("test/javaCode/a5marmoset")
  val testCaseFiles = getFiles(marmDir, 1)
  testCaseFiles.foreach(file => {
    val testSources = getFiles(file, -1).map(f => (Source.fromFile(f), f.getPath))
    /*
    println("Files:")
    testSources.foreach(x=> println("- " + x._2))
    println("- stdlib files")
    */
    
    test(file.getName) {
      //TODO generate the code...
      Seq("touch", outputDir.getPath + "/rouge").!!
      //assembling
      outputDir.listFiles.foreach(asm => Seq("nasm", "-O1", "-f", "elf" ,"-g", "-F", "dwarf", asm.getPath).!!)
      //Linking, but warning my mac at least does not have the same linker as marmoset

      Seq("ld", "-melf_i386", "-o", outputDir.getPath + "/main", outputDir.getPath + "/*.o").!!
       

      //It was not written any where but I suppose that all the code return 123...
      assert(Seq(outputDir.getPath + "/main").! == 123)
      

    }
    outputDir.listFiles.foreach(src => src.delete) 
  })

}
