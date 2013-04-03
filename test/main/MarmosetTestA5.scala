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
      //TODO generate the code...




   //    val writer = new BufferedWriter(new FileWriter(new File(outputDir.getPath + "/rouge.s")))
//       writer.write("""
//     ; Hello World in assembly for mac
//     ; nasm -f macho hello.asm
//     ; ld -e _start -o hello hello.o
//     ;
//     section .text
//     global _start ;must be declared for linker (ld)
     
//     _syscall:
//     int 0x80 ;system call
//     ret
     
//     _start: ;tell linker entry point
     
//     push dword len ;message length
//     push dword msg ;message to write
//     push dword 1 ;file descriptor (stdout)
//     mov eax,0x4 ;system call number (sys_write)
//     call _syscall ;call kernel
     
//     ;the alternate way to call kernel:
//     ;push eax
//     ;call 7:0
     
//     add esp,12 ;clean stack (3 arguments * 4)
     
//     push dword 0 ;exit code
//     mov eax,0x1 ;system call number (sys_exit)
//     call _syscall ;call kernel
     
//     ;we do not return from sys_exit,
//     ;there's no need to clean stack
//     section .data
     
//     msg db "Hello, world!",0xa ;our dear string
//     len equ $ - msg ;length of our dear string
// """)
//       writer.close
      //assembling
      //Linking, but warning my mac at least does not have the same linker as marmoset
      //outputDir.listFiles.foreach(file => println(file.getPath))
      try{
        if(System.getProperties.get("os.name")== "Mac OS X"){
          outputDir.listFiles.foreach(asm => Seq("nasm", "-f", "macho", asm.getPath).run)
            (Seq("ld", "-macosx_version_min", "10.6", "-e", "_start", "-o", outputDir.getPath + "/main") ++ outputDir.listFiles.map(_.getPath).filter(_.endsWith(".o"))).run
        }else{
          outputDir.listFiles.foreach(asm => Seq("nasm", "-O1", "-f", "elf" ,"-g", "-F", "dwarf", asm.getPath).run)
          Seq("ld", "-melf_i386", "-o", outputDir.getPath + "/main", outputDir.getPath + "/*.o").run
        }

      
        Seq(outputDir.getPath + "/main").run
      }finally{
        outputDir.listFiles.foreach(src => src.delete)
      }
      //It was not written any where but I suppose that all the code return 123...
  //    assert(Seq(outputDir.getPath + "/main").! == 123)
      


    }
  })

}
