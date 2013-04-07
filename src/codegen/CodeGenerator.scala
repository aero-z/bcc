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
      val rootName = cu.packageName.getOrElse(Name(Nil)).appendClassName(cu.typeName).toString + "." // Note: the last dot is already appended
      
      def packageToStr(p: Option[Name]) = p match {
        case Some(x) => x
        case None => ""
      }
      //val packagePrefix = packageToStr(cu.packageName)
      
      def methodsMatch(m1: MethodDeclaration, m2: MethodDeclaration): Boolean = {
        m1.methodName == m2.methodName && m1.parameters == m2.parameters
      }
      def hasMethod(cd: ClassDefinition, m: MethodDeclaration): Boolean = {
        cd.methods.exists(m2 => methodsMatch(m, m2))
      }
      
      def getMethods(pkg: Option[Name], cd: ClassDefinition, parentMethods: List[(Option[Name], ClassDefinition, MethodDeclaration)]): List[(Option[Name], ClassDefinition, MethodDeclaration)] = {
        def f(ms: List[MethodDeclaration], ts: List[(Option[Name], ClassDefinition, MethodDeclaration)]): List[(Option[Name], ClassDefinition, MethodDeclaration)] = {
          ms match {
            case Nil => ts
            case m :: mss =>
              f(mss, ts.find(t => methodsMatch(m, t._3)) match {
                case None => (pkg, cd, m) :: ts
                case Some(x) => x :: ts.filter(t => methodsMatch(m, t._3))
              })   
          }
        }
        
        val replaced = f(cd.methods, parentMethods)
        //val replaced = cd.methods.map(m => (parentMethods.find(t => methodsMatch(m, t._2)).map(t => t._1).getOrElse(cd), m))
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
		  """ +
      getMethods(cu.packageName, cd, Nil).map(t => s"${packageToStr(t._1)}.${t._2.className}.${t._3.methodName}").mkString("\n  ") + "\n\n"

    
      val staticFields = cd.fields.filter(x => x.modifiers.contains(Modifier.staticModifier)) //class fields
      val bss: List[X86Data] = staticFields.map(x => X86DataDoubleWordUninitialized(X86Label(rootName + x.fieldName)))
      /* TODO: static field initialization at some point
      def initialize(name: String, expr: Expression) = {
        expr.generateCode() ::: (X86Mov(X86Label(rootName + name), X86eax) :: Nil) //evaluate the initializer expression and 
      }*/
      //val init = staticFields.filter(_.initializer.isDefined).flatMap { case fd @ FieldDeclaration(name, _, _, Some(expr)) => initialize(name, expr) }
      val code =
        bss.foldLeft("segment .bss")((x, y) => x + "\n" + y) //TODO: is it ok if data segment is empty? (but the .bss tag is still there)
        //+ bss.foldLeft("initialize:")((x, y) => x + "\n" + y)
       val code2 = cd.methods.flatMap(_.generateCode(rootName)).foldLeft("")((x, y) => x + "\n" + y)
      //TODO: we need to export the labels for use outside the class
      //TODO we need to add code to the main for the initializer of static field assignemnt evaluations
      //case class FieldDeclaration(fieldName: String, fieldType: Type, override val modifiers: List[Modifier], initializer: Option[Expression]) extends MemberDeclaration(modifiers)
      
      //TODO: the instance fields:
      val instanceFields = cd.fields.filter(x => !x.modifiers.contains(Modifier.staticModifier))
      println(code+code2)
      //TODO: return correct code
      "; === " + cd.className + "===" + data //return value
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
