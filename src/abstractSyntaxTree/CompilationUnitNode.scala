package abstractSyntaxTree

import abstractSyntaxTree.Modifier._
import abstractSyntaxTree.Operator._
import scanner.IntegerToken
import scala.Enumeration

//Will be used quite often, is for instance "java.util.String"
case class Name(path: List[String]) extends Expression{
    def toString = path.reduce((x, y)=> x + "." + y)
}

//Main  of a file.
case class CompilationUnit(packageName: Option[Name], classImport: List[Name],
        packageImport: List[Name], typeDef: TypeDefinition, fileName: String)

//Either a class or an interface
abstract class TypeDefinition(typeName: String)

case class InterfaceDefinition(interfaceName: String, parents: List[RefType],
    modifiers: List[Modifier], methods: List[MethodDeclaration]) extends TypeDefinition(interfaceName)

case class ClassDefinition(className: String, parent: Some[RefType], interfaces: List[RefType], modifiers: List[Modifier],
    fields: List[FieldDeclaration], methods: List[MethodDeclaration],
    constructors: List[ConstructorDeclaration]) extends TypeDefinition(className)

//What can be put in a class
case class MethodDeclaration(methodName: String, returnType: Type, modifiers: List[Modifier],
    parameters: List[(Type, String)], implementation: Some[Block])
case class FieldDeclaration(fieldName: String, fieldType: Type, modifiers: List[Modifier],
    initializer: Option[Expression])
case class ConstructorDeclaration(modifiers: List[Modifier], parameters: List[(Type, String)], implementation: Block)

        

