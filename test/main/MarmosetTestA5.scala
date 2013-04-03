package main

import org.scalatest.FunSuite
import java.io._
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
  outputDir.listFiles.foreach(src => src.delete)

  val marmDir = new File("test/javaCode/a5marmoset")
  val testCaseFiles = getFiles(marmDir, 1).filter(_.getName != "output")
  testCaseFiles.foreach(file => {
    val testSources = getFiles(file, -1).map(f => (Source.fromFile(f), f.getPath))
    /*
    println("Files:")
    testSources.foreach(x=> println("- " + x._2))
    println("- stdlib files")
    */

    test(file.getName) {
      // generate the code...
      
      assert(Joosc.compile(testSources.toList) === Joosc.errCodeSuccess)
      
      val expectedExitCode =
        if (file.getName.startsWith("J1e")) 13
        else                                123

      /* // DUMMY CODE
      val writer = new BufferedWriter(new FileWriter(new File(outputDir.getPath + "/rouge.s")))
      writer.write("""
          
global _start
_start:

mov eax, 1
mov ebx, 123
int 0x80

          
 """)
      writer.close
      */
      

      //assembling
      //Linking, but warning my mac at least does not have the same linker as marmoset
      //outputDir.listFiles.foreach(file => println(file.getPath))
      try {
        if (System.getProperties.get("os.name") == "Mac OS X") {
          outputDir.listFiles.foreach(asm => Seq("nasm", "-f", "macho", asm.getPath).!)
          (Seq("ld", "-macosx_version_min", "10.6", "-e", "_start", "-o", outputDir.getPath + "/main") ++ outputDir.listFiles.map(_.getPath).filter(_.endsWith(".o"))).!
        } else {
          outputDir.listFiles.foreach(asm => Seq("nasm", "-O1", "-f", "elf", "-g", "-F", "dwarf", asm.getPath).!)
          Seq("ld", "-melf_i386", "-o", outputDir.getPath + "/main", outputDir.getPath + "/rouge.o").!
        }
        
	    //It was not written anywhere but I suppose that all the code return 123...
	    assert(Seq(outputDir.getPath + "/main").! === expectedExitCode)
      } finally {
        outputDir.listFiles.foreach(src => src.delete)
      }

    }
  })

}
