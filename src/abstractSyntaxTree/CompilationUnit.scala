package abstractSyntaxTree

import abstractSyntaxTree.Modifier._
import abstractSyntaxTree.Operator._
import scanner.IntegerToken
import scala.Enumeration

//Will be used quite often, is for instance "java.util.String"
case class Name(path: List[String]) extends Expression {
    override def toString = path.reduce((x, y) => x + "." + y)
}

//Main  of a file.
case class CompilationUnit(packageName: Option[Name], importDeclarations: List[ImportDeclaration], typeDef: Option[TypeDefinition], fileName: String) {
    def display(): Unit = {
        println("*" * 20)
        println("Compilation Unit")
        println("*" * 20)
        println(s"File Name: $fileName")
        println(s"Package defined: ${packageName.isDefined}")
        for (x <- packageName) println(s"Package name: ${packageName.toString}")
        println(s"Number of import: ${importDeclarations.size}")
        for (ClassImport(name) <- importDeclarations) println(s"Import class: ${name.toString}")
        for (PackageImport(name) <- importDeclarations) println(s"Import package: ${name.toString}")
        println("*" * 20)
        println()
        for (typeDefinition <- typeDef) typeDefinition.display
    }
}

abstract class ImportDeclaration(name: Name)

case class ClassImport(name: Name) extends ImportDeclaration(name)
case class PackageImport(name: Name) extends ImportDeclaration(name)

//Either a class or an interface
abstract class TypeDefinition(typeName: String) {
    def display: Unit
}

case class InterfaceDefinition(interfaceName: String, parents: List[RefType],
    modifiers: List[Modifier], methods: List[MethodDeclaration]) extends TypeDefinition(interfaceName) {
    def display: Unit = {
        println("*" * 20)
        println("Interface declaration")
        println("*" * 20)
        println(s"Interface name: $interfaceName")
        println(s"Number of parents: ${parents.size}")
        for (RefType(x) <- parents) println(s"Interface extending: ${x.toString}")
        for (x <- modifiers) println(s"Modifier: ${x.toString()}")
        println(s"Number of methods: ${methods.size}")
        println("*" * 20)
        println()
        for (meth <- methods) meth.display
    }
}

case class ClassDefinition(className: String, parent: Option[RefType], interfaces: List[RefType], modifiers: List[Modifier], fields: List[FieldDeclaration], constructors: List[ConstructorDeclaration], methods: List[MethodDeclaration]) extends TypeDefinition(className) {
    def display: Unit = {
        println("*" * 20)
        println("Class declaration")
        println("*" * 20)
        println(s"Class name: $className")
        println(s"Has parent: ${parent.isDefined}")
        for (RefType(x) <- parent) println(s"Class extending: ${x.toString}")
        println(s"Number of implemented interfaces: ${interfaces.size}")
        for (RefType(x) <- interfaces) println(s"Interface implemented: ${x.toString}")
        for (x <- modifiers) println(s"Modifier: ${x.toString()}")
        println(s"Number of fields: ${fields.size}")
        println(s"Number of constructors: ${constructors.size}") 
        println(s"Number of methods: ${methods.size}")
        println("*" * 20)
        println()
        for (field <- fields) field.display
        for (constructor <- constructors) constructor.display
        for (method <- methods) method.display
    }
}

//What can be put in a class
case class MethodDeclaration(methodName: String, returnType: Type, modifiers: List[Modifier],
    parameters: List[(Type, String)], implementation: Some[Block]) {
    def display: Unit = {
        //TODO something fancy...
    }
}

case class FieldDeclaration(fieldName: String, fieldType: Type, modifiers: List[Modifier],
    initializer: Option[Expression]) {
    def display: Unit = {
        //TODO something fancy...
    }
}
case class ConstructorDeclaration(modifiers: List[Modifier], parameters: List[(Type, String)], implementation: Block) {
    def display: Unit = {
        //TODO something fancy...
    }
}

        

