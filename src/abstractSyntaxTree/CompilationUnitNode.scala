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
        parameters: List[(String, String)], implementation: Some[BlockNode]) 
case class FieldDeclarationNode(fieldName : String, fieldType: String, modifiers: List[Modifier], initializer : OPtion[ExpressionNode])
case class ConstructorDeclarationNode(constructorName: String, modifiers : List[Modifier],
        parameters: List[(String, String)], implementation: BlockNode)

case class BlockNode() //TODO something here
case class ExpressionNode() //TODO some stuff