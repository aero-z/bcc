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
/*    val writer = new BufferedWriter(new FileWriter(new File("output/simple.s")))
    writer.write("""
          
global _start
_start:

mov eax, 1
mov ebx, 123
int 0x80

          
 """)
    writer.close*/
    
    //intermediate representation: for class should contain map from local variables to offset
    //val methodMatrix 
    
    def createCode(cus: List[CompilationUnit]) =
      cus.collect {
        case cu @ CompilationUnit(optName, _, Some(d: ClassDefinition), name) =>
          generate(optName.getOrElse(Name(Nil)).appendClassName(name).toString + ".", d)
      }

    def generate(rootName: String, cd: ClassDefinition) { //Note: the rootname already has a "." appended
      val staticFields = cd.fields.filter(x => x.modifiers.contains(Modifier.staticModifier)) //class fields
      val bss: List[X86Data] = staticFields.map(x => X86DataDoubleWordUninitialized(X86Label(rootName + x.fieldName)))
      /*def initialize(name: String, expr: Expression) = {
        expr.generateCode() ::: (X86Mov(X86Label(rootName + name), X86eax) :: Nil) //evaluate the initializer expression and 
      }*/
      //val init = staticFields.filter(_.initializer.isDefined).flatMap { case fd @ FieldDeclaration(name, _, _, Some(expr)) => initialize(name, expr) }
      val block = cd.methods.flatMap(_.generateCode(rootName))
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
    }

    //leave the java lib files out for the moment! -> makes testing easier
    createCode(cus
    .filter(_.packageName != Some(Name("java"::"lang"::Nil))).filter(_.packageName != Some(Name("java"::"io"::Nil))).filter(_.packageName != Some(Name("java"::"util"::Nil)))
    )

  }
}