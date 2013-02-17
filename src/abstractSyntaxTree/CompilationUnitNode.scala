package abstractSyntaxTree

import abstractSyntaxTree.Modifier._

case class CompilationUnitNode(packageName: Option[String], importList: List[String], typeDef: TypeDefinitionNode, fileName: String)



abstract class TypeDefinitionNode(typeName: String)

case class InterfaceDefinitionNode(interfaceName: String, parents: List[String],
    modifiers: List[Modifier], methods: List[MethodDeclarationNode]) extends TypeDefinitionNode(interfaceName)

case class ClassDefinitionNode(className: String, parent: Some[String], interfaces: List[String], modifiers: List[Modifier],
    fields: List[FieldDeclarationNode], methods: List[MethodDeclarationNode],
    constructors: List[ConstructorDeclarationNode]) extends TypeDefinitionNode(className)





case class MethodDeclarationNode(methodName: String, returnType: String, modifiers: List[Modifier], 
        parameter: List[(String, String)], implementation: Some[Block]) 
case class FieldDeclarationNode() //TODO add some stuff
case class ConstructorDeclarationNode()//TODO add some stuff here...

case class Block() //TODO something here