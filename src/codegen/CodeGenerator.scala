package codegen

import ast._
import java.io.BufferedWriter
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

  /**
   * generate files in the output/ directory
   */
  def makeAssembly(cus: List[CompilationUnit]): Unit = {

    // DUMMY CODE
    val writer = new BufferedWriter(new FileWriter(new File("output/simple.s")))
    writer.write("""
          
global _start
_start:

mov eax, 1
mov ebx, 123
int 0x80

          
 """)
    writer.close

    //intermediate representation: for class should contain map from local variables to offset
    //val methodMatrix 
    def createCode(cus: List[CompilationUnit]) =
      cus.collect { case cu @ CompilationUnit(_, _, Some(d: ClassDefinition), _) => generate(cu, d) }

    def generate(cu: CompilationUnit, cd: ClassDefinition): String = { //we just need the CU for the full name
      def rootName = cu.packageName.getOrElse(Name(Nil)).appendClassName(cu.typeName).toString + "." // Note: the last dot is already appended
      val staticFields = cd.fields.filter(x => x.modifiers.contains(Modifier.staticModifier))
      val bss: List[X86Data] = staticFields.map(x => X86DataDoubleWordUninitialized(rootName + x.fieldName))
      def initialize(name: String, expr: Expression) = {
        expr.generateCode() ::: (X86Mov(X86Label(rootName + name), X86eax) :: Nil)
      }
      val init = staticFields.filter(_.initializer.isDefined).flatMap { case fd @ FieldDeclaration(name, _, _, Some(expr)) => initialize(name, expr) }

      val code =
        bss.foldLeft("segment .bss")((x, y) => x + "\n" + y) //TODO: is it ok if data segment is empty? (but the .bss tag is still there)
      val ced = bss.foldLeft("initialize:")((x, y) => x + "\n" + y)
      //TODO: we need to export the labels for use outside the class
      //TODO we need to add code to the main for the initializer of static field assignemnt evaluations
      //case class FieldDeclaration(fieldName: String, fieldType: Type, override val modifiers: List[Modifier], initializer: Option[Expression]) extends MemberDeclaration(modifiers)
      code
    }

    /*
    CodeGenerator
    .createCode(varLinked
    .filter(_.packageName != Some(Name("java"::"lang"::Nil))).filter(_.packageName != Some(Name("java"::"io"::Nil))).filter(_.packageName != Some(Name("java"::"util"::Nil)))
    )
    .foreach(println(_));
    */
  }
}