package ast

import ast.Modifier._
import ast.Operator._
import scanner.IntegerToken
import scala.Enumeration
import main.Logger
import java.io.File
import codegen._
import nameResolution._

private object Weed {
  
  def checkDuplicateModifiers(modifiers: List[Modifier]) = 
    WeedResult(modifiers.distinct.length == modifiers.length, "duplicate modifier")
  
  def checkPublicProtectedModifier(modifiers: List[Modifier]) = 
    WeedResult(modifiers.contains(Modifier.publicModifier) || modifiers.contains(Modifier.protectedModifier), "public/protected modifier missing")

}
trait VariableDeclaration
abstract class MemberDeclaration(val modifiers: List[Modifier])

//Will be used quite often, is for instance "java.util.String"
case class Name(path: List[String]) extends AstNode {
  override def toString = path.reduce((x, y) => x + "." + y)
  def getCanonicalName():String = path.last
  def appendClassName(className:String) = Name(path ::: List(className))
  def appendClassName(className:Name) = Name(path ::: className.path)
}

//Main  of a file.
case class CompilationUnit(packageName: Option[Name], importDeclarations: List[ImportDeclaration], typeDef: Option[TypeDefinition], typeName: String) extends AstNode {
  def display(): Unit = {
    Logger.debug("*" * 20)
    Logger.debug("Compilation Unit")
    Logger.debug("*" * 20)
    Logger.debug(s"File Name: $typeName")
    Logger.debug(s"Package defined: ${packageName.isDefined}")
    for (x <- packageName) Logger.debug(s"Package name: ${packageName.toString}")
    Logger.debug(s"Number of import: ${importDeclarations.size}")
    for (ClassImport(name) <- importDeclarations) Logger.debug(s"Import class: ${name.toString}")
    for (PackageImport(name) <- importDeclarations) Logger.debug(s"Import package: ${name.toString}")
    Logger.debug("*" * 20)
    Logger.debug("")
    for (typeDefinition <- typeDef) typeDefinition.display
  }
  override lazy val weedResult = typeDef match {
    case None => WeedOk()
    case Some(x) => WeedResult(x.getName() == typeName, "file name and class name don't match")
  }
}

abstract class ImportDeclaration(name: Name) extends AstNode {
  def getName():Name = name
}

case class ClassImport(name: Name) extends ImportDeclaration(name)
case class PackageImport(name: Name) extends ImportDeclaration(name)
case class LinkImport(name:String, refType:RefTypeLinked) extends ImportDeclaration(Name(name::Nil))
/*
class Wrapper(typeDef: => TypeDefinition) extends AstNode with NotNull {
* we need a lazy valuation somewhere
}
* */
//Either a class or an interface
sealed abstract class TypeDefinition(typeName: String, modifiers: List[Modifier]) extends AstNode with NotNull {
  def getName():String = typeName
  def display: Unit
  
  def weedResult_ = Weed.checkDuplicateModifiers(modifiers) ++
                    WeedResult(!(modifiers.contains(Modifier.abstractModifier) && modifiers.contains(Modifier.finalModifier)),
                           "class can't be abstract and final at the same time") ++
                    WeedResult(modifiers.contains(Modifier.publicModifier), "type definition must be public")
  override lazy val weedResult = weedResult_
}

case class InterfaceDefinition(interfaceName: String, parents: List[RefType],
  modifiers: List[Modifier], methods: List[MethodDeclaration]) extends TypeDefinition(interfaceName, modifiers) {
  def display: Unit = {
    Logger.debug("*" * 20)
    Logger.debug("Interface declaration")
    Logger.debug("*" * 20)
    Logger.debug(s"Interface name: $interfaceName")
    Logger.debug(s"Number of parents: ${parents.size}")
    for (x:RefType <- parents) Logger.debug(s"Interface extending: ${x.toString}")
    for (x <- modifiers) Logger.debug(s"Modifier: ${x.toString()}")
    Logger.debug(s"Number of methods: ${methods.size}")
    Logger.debug("*" * 20)
    Logger.debug("")
    for (meth <- methods) meth.display
  }
  override lazy val weedResult = super.weedResult_ ++
      methods.foldLeft(WeedOk(): WeedResult)((wr, m) => wr ++ (m match {
             case MethodDeclaration(_,_,_,_,Some(x), _) => WeedFail("an interface method cannot have a body")
             case MethodDeclaration(_,_,modifiers,_,_,_) => WeedResult(!modifiers.contains(Modifier.staticModifier) && !modifiers.contains(Modifier.finalModifier) && !modifiers.contains(Modifier.nativeModifier), "an interface method cannot be static, final, or native")
             case _ => WeedOk()
      }))
}

case class ClassDefinition(className: String, parent: Option[RefType], interfaces: List[RefType], modifiers: List[Modifier], fields: List[FieldDeclaration], constructors: List[ConstructorDeclaration], methods: List[MethodDeclaration]) extends TypeDefinition(className, modifiers) {
  def display: Unit = {
    Logger.debug("*" * 20)
    Logger.debug("Class declaration")
    Logger.debug("*" * 20)
    Logger.debug(s"Class name: $className")
    Logger.debug(s"Has parent: ${parent.isDefined}")
    for (x:RefType <- parent) Logger.debug(s"Class extending: ${x.toString}")
    Logger.debug(s"Number of implemented interfaces: ${interfaces.size}")
    for (x:RefType <- interfaces) Logger.debug(s"Interface implemented: ${x.toString}")
    for (x <- modifiers) Logger.debug(s"Modifier: ${Modifier.fromModifier(x)}")
    Logger.debug(s"Number of fields: ${fields.size}")
    Logger.debug(s"Number of constructors: ${constructors.size}")
    Logger.debug(s"Number of methods: ${methods.size}")
    Logger.debug("*" * 20)
    Logger.debug("")
    for (field <- fields) field.display
    for (constructor <- constructors) constructor.display
    for (method <- methods) method.display
  }
  
  override lazy val weedResult = super.weedResult_ ++
      methods.foldLeft(WeedOk(): WeedResult)((wr, m) => wr ++ (m match {
             case MethodDeclaration(_,_,modifiers,_,impl,_) =>
               WeedResult(impl.isDefined == (!modifiers.contains(Modifier.abstractModifier) && !modifiers.contains(Modifier.nativeModifier)),
                   "method must have a body if and only if it is neither abstract nor native")
             case _ => WeedOk()
      })) ++ WeedResult(!constructors.exists(_.name != className), "constructors must have same name as the class")
}

private object Function {
  
  def generateCode(parameters: List[Parameter], implementation: Option[Block], localPath: List[PathLocal])(implicit cus:List[CompilationUnit]): List[X86Instruction] = {
    val current:List[Int] = Nil
    implicit val params:List[String] = parameters.map(_.id)
    implicit val pathList:List[List[Int]] = localPath.map(_.statementIndex)
    val numParams = parameters.length
    val endLabel = LabelGenerator.generate
    implementation.toList.flatMap(impl => {
      val code = impl.generateCode(current)
      X86Push(X86ebp) ::
      X86Push(X86ebx) ::
      X86Sub(X86esp, X86Number(numParams * 4)) ::
      X86Mov(X86ebp, X86esp) ::
      code.dropRight(1).map(_ match {
        case X86Ret =>
          X86Jmp(endLabel)
        case x => x
      }) :::
      (if (code.count(_ == X86Ret) > 1) endLabel :: Nil else Nil) :::
      X86Add(X86esp, X86Number(numParams * 4)) ::
      X86Pop(X86ebx) ::
      X86Pop(X86ebp) ::
      X86Ret :: Nil
    })
  }
}

//What can be put in a class
case class MethodDeclaration(methodName: String, returnType: Type, override val modifiers: List[Modifier],
  parameters: List[Parameter], implementation: Option[Block], localPath: List[PathLocal]) extends MemberDeclaration(modifiers) with VariableDeclaration with AstNode {
  def display: Unit = {
    Logger.debug("*" * 20)
    Logger.debug("Method declaration")
    Logger.debug("*" * 20)
    Logger.debug(s"Method name: $methodName")
    Logger.debug(s"Return type: ${returnType.typeName}")
    Logger.debug(s"Number of modifiers: ${modifiers.size}")
    for(x <- modifiers) Logger.debug(s"Modifier: ${Modifier.fromModifier(x)}")
    Logger.debug(s"Number of parameters: ${parameters.size}")
    for(Parameter(x, y) <- parameters) Logger.debug(s"Parameter type : ${x.typeName}\n          name: $y")
    Logger.debug(s"Is defined: ${implementation.isDefined}")
    Logger.debug("*" * 20)   
    Logger.debug("")
    //TODO something about the implementation
  }
  
  def generateCode(implicit cus:List[CompilationUnit]): List[X86Instruction] =
    Function.generateCode(parameters, implementation, localPath)
    
  override lazy val weedResult =
      Weed.checkDuplicateModifiers(modifiers) ++
      Weed.checkPublicProtectedModifier(modifiers) ++
      WeedResult(!modifiers.contains(Modifier.staticModifier) || !modifiers.contains(Modifier.finalModifier),
        "a static method cannot be final") ++
      WeedResult(!modifiers.contains(Modifier.nativeModifier) || modifiers.contains(Modifier.staticModifier),
        "a native method must be static") ++
      WeedResult(!modifiers.contains(Modifier.abstractModifier) || (!modifiers.contains(Modifier.staticModifier) && !modifiers.contains(Modifier.finalModifier)),
        "an abstract method cannot be static or final")
}

case class FieldDeclaration(fieldName: String, fieldType: Type, override val modifiers: List[Modifier],
  initializer: Option[Expression]) extends MemberDeclaration(modifiers) with VariableDeclaration with AstNode {
  def display: Unit = {
    Logger.debug("*" * 20)
    Logger.debug("Field declaration")
    Logger.debug("*" * 20)
    Logger.debug(s"Field name: $fieldName")
    Logger.debug(s"Type: ${fieldType.typeName}")
    Logger.debug(s"Number of modifiers: ${modifiers.size}")
    for(x <- modifiers) Logger.debug(s"Modifier: ${Modifier.fromModifier(x)}")
    Logger.debug(s"Field initializer: ${initializer.isDefined}")
    Logger.debug("*" * 20)
    Logger.debug("")
    //TODO something about the initializer
  }
  override lazy val weedResult = Weed.checkDuplicateModifiers(modifiers) ++
                            Weed.checkPublicProtectedModifier(modifiers) ++
                            WeedResult(!modifiers.contains(Modifier.finalModifier), "no field can be final")
}

case class ConstructorDeclaration(name: String, modifiers: List[Modifier], parameters: List[Parameter], implementation: Block, localPath: List[PathLocal]) extends AstNode with VariableDeclaration {
  def display: Unit = {
    Logger.debug("*" * 20)
    Logger.debug("Constructor declaration")
    Logger.debug("*" * 20)
    Logger.debug(s"Number of modifiers: ${modifiers.size}")
    for(x <- modifiers) Logger.debug(s"Modifier: ${Modifier.fromModifier(x)}")
    Logger.debug(s"Number of parameters: ${parameters.size}")
    for(Parameter(x,y) <- parameters) Logger.debug(s"Parameter type : ${x.typeName}\n            name: $y")
    Logger.debug("*" * 20)
    Logger.debug("")
    //TODO something fancy about the implementation
  }
  
  def generateCode(implicit cus:List[CompilationUnit]): List[X86Instruction] =
    Function.generateCode(parameters, Some(implementation), localPath)
  
  override lazy val weedResult = Weed.checkDuplicateModifiers(modifiers)
}

case class Parameter(paramType: Type, id:String) extends AstNode with VariableDeclaration
