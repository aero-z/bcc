package codegen

import ast._

object Types {
  //instance and static fields
  type MethodMatrix = List[SelectorIndex] //should be runtime evaluable
}
abstract class CodeObject(address:Integer) {
  def toCode: String
}
//transform all pointers to 
case class Class(name:Name, /*definition:ClassDefinition,*/ address:Integer, parent:Option[Class], staticFields:List[Field], methods:List[Method], methodMatrix:Types.MethodMatrix /*for interface usage*/) extends CodeObject(address) {
  def toCode = {
    ""+address+(parent match {case None => "" case Some(p) => p.address})+staticFields.map(_.offset)+methods.foldLeft("")((string, offset) => string+offset)
  }
}
//the field from the parent are considered normal fields!
case class Object(name: Name, address:Integer, classPointer:Class, fields:List[Field] /*parent fields first!*/) extends CodeObject(address) {
  def toCode = {
    ""+classPointer.address+""
  }
}
case class Field(name:Name, offset:Integer)
//case class Interface() -> not needed all checks done before running
case class Method(methodName: Name, offset:Integer, code:String) {
  
}

case class SelectorIndex(c:Class, m:Method, offset:Integer)

object CodeGenerator {
  //intermediate representation: for class should contain map from local variables to offset
  //val methodMatrix 
  def createCode(cus: List[CompilationUnit]) = cus.collect{case cu @ CompilationUnit(_, _, Some(d:ClassDefinition), _) => generate(cu, d)}
  //def makeAssembly(prog: IrProgram): X68Program = ???
  
  def generate(cu:CompilationUnit, cd: ClassDefinition): String = { //we just need the CU for the full name
    def rootName = cu.packageName.getOrElse(Name(Nil)).appendClassName(cu.typeName).toString+"." // Note: the last dot is already appended
    val bss:List[X86Data] = cd.fields.filter(x => x.modifiers.contains(Modifier.staticModifier)).map(x => X86DataDoubleWordUninitialized(rootName+x.fieldName))
    val code = "segment .data\n"+bss.fold("")((x,y) => x+"\n"+y) //TODO: is it ok if data segment is empty?
    //TODO: we need to export the labels for use outside the class
    //TODO we need to add code to the main for the initializer of static field assignemnt evaluations
//case class FieldDeclaration(fieldName: String, fieldType: Type, override val modifiers: List[Modifier], initializer: Option[Expression]) extends MemberDeclaration(modifiers)
    "Hello"
  }
}