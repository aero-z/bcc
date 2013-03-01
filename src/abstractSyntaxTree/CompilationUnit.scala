package abstractSyntaxTree

import abstractSyntaxTree.Modifier._
import abstractSyntaxTree.Operator._
import scanner.IntegerToken
import scala.Enumeration
import main.Logger
//Will be used quite often, is for instance "java.util.String"
case class Name(path: List[String]) extends Expression {
  override def toString = path.reduce((x, y) => x + "." + y)
  def getCanonicalName():String =  {
    def getRec(list:List[String]):String = list match {
    	case x :: Nil => x
    	case x :: xs => getRec(xs)
    }
    getRec(path);
  }
}

//Main  of a file.
case class CompilationUnit(packageName: Option[Name], importDeclarations: List[ImportDeclaration], typeDef: Option[TypeDefinition], fileName: String) {
  def display(): Unit = {
    Logger.debug("*" * 20)
    Logger.debug("Compilation Unit")
    Logger.debug("*" * 20)
    Logger.debug(s"File Name: $fileName")
    Logger.debug(s"Package defined: ${packageName.isDefined}")
    for (x <- packageName) Logger.debug(s"Package name: ${packageName.toString}")
    Logger.debug(s"Number of import: ${importDeclarations.size}")
    for (ClassImport(name) <- importDeclarations) Logger.debug(s"Import class: ${name.toString}")
    for (PackageImport(name) <- importDeclarations) Logger.debug(s"Import package: ${name.toString}")
    Logger.debug("*" * 20)
    Logger.debug("")
    for (typeDefinition <- typeDef) typeDefinition.display
  }
}

abstract class ImportDeclaration(name: Name) {
  def getName():Name = name
}

case class ClassImport(name: Name) extends ImportDeclaration(name)
case class PackageImport(name: Name) extends ImportDeclaration(name)

//Either a class or an interface
abstract class TypeDefinition(typeName: String) {
  def display: Unit
}

case class InterfaceDefinition(interfaceName: String, parents: List[RefType],
  modifiers: List[Modifier], methods: List[MethodDeclaration]) extends TypeDefinition(interfaceName) {
  def display: Unit = {
    Logger.debug("*" * 20)
    Logger.debug("Interface declaration")
    Logger.debug("*" * 20)
    Logger.debug(s"Interface name: $interfaceName")
    Logger.debug(s"Number of parents: ${parents.size}")
    for (RefType(x) <- parents) Logger.debug(s"Interface extending: ${x.toString}")
    for (x <- modifiers) Logger.debug(s"Modifier: ${x.toString()}")
    Logger.debug(s"Number of methods: ${methods.size}")
    Logger.debug("*" * 20)
    Logger.debug("")
    for (meth <- methods) meth.display
  }
}

case class ClassDefinition(className: String, parent: Option[RefType], interfaces: List[RefType], modifiers: List[Modifier], fields: List[FieldDeclaration], constructors: List[ConstructorDeclaration], methods: List[MethodDeclaration]) extends TypeDefinition(className) {
  def display: Unit = {
    Logger.debug("*" * 20)
    Logger.debug("Class declaration")
    Logger.debug("*" * 20)
    Logger.debug(s"Class name: $className")
    Logger.debug(s"Has parent: ${parent.isDefined}")
    for (RefType(x) <- parent) Logger.debug(s"Class extending: ${x.toString}")
    Logger.debug(s"Number of implemented interfaces: ${interfaces.size}")
    for (RefType(x) <- interfaces) Logger.debug(s"Interface implemented: ${x.toString}")
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
}

//What can be put in a class
case class MethodDeclaration(methodName: String, returnType: Type, modifiers: List[Modifier],
  parameters: List[(Type, String)], implementation: Option[Block]) {
  def display: Unit = {
    Logger.debug("*" * 20)
    Logger.debug("Method declaration")
    Logger.debug("*" * 20)
    Logger.debug(s"Method name: $methodName")
    Logger.debug(s"Return type: ${returnType.typeName}")
    Logger.debug(s"Number of modifiers: ${modifiers.size}")
    for(x <- modifiers) Logger.debug(s"Modifier: ${Modifier.fromModifier(x)}")
    Logger.debug(s"Number of parameters: ${parameters.size}")
    for((x,y) <- parameters) Logger.debug(s"Parameter type : ${x.typeName}\n          name: $y")
    Logger.debug(s"Is defined: ${implementation.isDefined}")
    Logger.debug("*" * 20)
    Logger.debug("")
    //TODO something about the implementation
  }
}

case class FieldDeclaration(fieldName: String, fieldType: Type, modifiers: List[Modifier],
  initializer: Option[Expression]) {
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
}
case class ConstructorDeclaration(modifiers: List[Modifier], parameters: List[(Type, String)], implementation: Block) {
  def display: Unit = {
    Logger.debug("*" * 20)
    Logger.debug("Constructor declaration")
    Logger.debug("*" * 20)
    Logger.debug(s"Number of modifiers: ${modifiers.size}")
    for(x <- modifiers) Logger.debug(s"Modifier: ${Modifier.fromModifier(x)}")
    Logger.debug(s"Number of parameters: ${parameters.size}")
    for((x,y) <- parameters) Logger.debug(s"Parameter type : ${x.typeName}\n            name: $y")
    Logger.debug("*" * 20)
    Logger.debug("")
    //TODO something fancy about the implementation
  }
}



