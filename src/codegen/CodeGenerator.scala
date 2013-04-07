package codegen

import ast._
import java.io.BufferedWriter
import java.io.FileOutputStream
import java.io.FileWriter
import java.io.File

object Types {
  //instance and static fields
  type MethodMatrix = List[SelectorIndex] //should be runtime evaluable
}
abstract class CodeObject(address: Integer) {
  def toCode: String
}
//transform all pointers to 
case class Class(name: Name, /*definition:ClassDefinition,*/ address: Integer, parent: Option[Class], staticFields: List[Field], methods: List[Method], methodMatrix: Types.MethodMatrix /*for interface usage*/ ) extends CodeObject(address) {
  def toCode = {
    "" + address + (parent match { case None => "" case Some(p) => p.address }) + staticFields.map(_.offset) + methods.foldLeft("")((string, offset) => string + offset)
  }
}
//the field from the parent are considered normal fields!
case class Object(name: Name, address: Integer, classPointer: Class, fields: List[Field] /*parent fields first!*/ ) extends CodeObject(address) {
  def toCode = {
    "" + classPointer.address + ""
  }
}
case class Field(name: Name, offset: Integer)
//case class Interface() -> not needed all checks done before running
case class Method(methodName: Name, offset: Integer, code: String) {

}

case class SelectorIndex(c: Class, m: Method, offset: Integer)

object CodeGenerator {

    def iffalse(expr:Expression, label:X86Label):List[X86Instruction] = {
      expr.generateCode ::: (X86Mov(X86ebx, X86Boolean(false)) :: X86Cmp(X86eax, X86ebx) :: X86Je(label) :: Nil) //TODO:eax contains answer?
    }
  /**
   * generate files in the output/ directory
   */
  def makeAssembly(cus: List[CompilationUnit]): Unit = {
    // DUMMY CODE
	/*
	val writer = new BufferedWriter(new FileWriter(new File("output/simple.s")))
	writer.write("""
	
	global _start
	_start:
	
	mov eax, 1
	mov ebx, 123
	int 0x80
	
	          
	 """)
	    writer.close*/
    def generate(cu: CompilationUnit, cd: ClassDefinition): String = { //we just need the CU for the full name
      
      def packageToStr(p: Option[Name]) = p match {
        case Some(x) => x
        case None => ""
      }
      val prefix = packageToStr(cu.packageName) + "." + cu.typeName
      
      
      ///////////////// data segment /////////////////
      def methodsMatch(m1: MethodDeclaration, m2: MethodDeclaration): Boolean = {
        m1.methodName == m2.methodName && m1.parameters == m2.parameters
      }
      
      def getMethods(pkg: Option[Name], cd: ClassDefinition, parentMethods: List[(Option[Name], ClassDefinition, MethodDeclaration)]): List[(Option[Name], ClassDefinition, MethodDeclaration)] = {

        def mergeMethods(ms: List[MethodDeclaration], ts: List[(Option[Name], ClassDefinition, MethodDeclaration)]): List[(Option[Name], ClassDefinition, MethodDeclaration)] = {
          ms match {
            case Nil => ts
            case m :: mss =>
              mergeMethods(mss, ts.find(t => methodsMatch(m, t._3)) match {
                case None => (pkg, cd, m) :: ts
                case Some(x) => x :: ts.filter(t => methodsMatch(m, t._3))
              })   
          }
        }
        
        val replaced = mergeMethods(cd.methods, parentMethods)
        cd.parent match {
          case None => replaced
          case Some(p) => 
            val linked = p.asInstanceOf[RefTypeLinked]
            getMethods(linked.pkgName, linked.getTypeDef(cus).asInstanceOf[ClassDefinition], replaced)
        }
      }
            
      val data = """
section .data
		
; VTABLE
class:
  dd 0 ; TODO: pointer to SIT
  """ + getMethods(cu.packageName, cd, Nil).map(t => s"${packageToStr(t._1)}.${t._2.className}.${t._3.methodName}").mkString("\n  ") + "\n\n"
      ///////////////// end of data segment /////////

      ///////////////// bss segment /////////////////
      val staticFields = cd.fields.filter(x => x.modifiers.contains(Modifier.staticModifier))
      val bss = """
section .bss

; static fields
  """ + staticFields.map(f => s"$prefix.${f.fieldName}: resb 4").mkString("\n  ") + "\n\n"
      ///////////////// end of bss segment //////////

      "; === " + cd.className + "===" + data + bss
    }
    
    cus
    //leave the java lib files out for the moment! -> makes testing easier
    .filter(_.packageName != Some(Name("java"::"lang"::Nil))).filter(_.packageName != Some(Name("java"::"io"::Nil))).filter(_.packageName != Some(Name("java"::"util"::Nil)))
    .collect { case cu @ CompilationUnit(optName, _, Some(d: ClassDefinition), name) =>
      val writer = new BufferedWriter(new FileWriter(new File("output/"+cu.typeName+".s")))
        //println("class: " + cu.typeName)
      val code = generate(cu, d)
      writer.write(code)
      writer.close
    }
  }
}
