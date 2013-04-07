package ast

import codegen._

abstract class Type extends AstNode {
    def typeName:String
}

abstract class PrimitiveType extends Type

object PrimitiveType{
  def fromString(str: String): Type = str match{
    case "int" => IntType
    case "boolean" => BooleanType
    case "byte" => ByteType
    case "short" => ShortType
    case "char" => CharType
    case "void" => VoidType
  }
}

trait IntegerTrait
trait ShortTrait extends IntegerTrait
trait ByteTrait extends ShortTrait
trait CharTrait extends IntegerTrait

case object IntType extends PrimitiveType with IntegerTrait {
    def typeName: String = "int"
}

case object BooleanType extends PrimitiveType {
    def typeName: String = "boolean"
}

case object ByteType extends PrimitiveType with ByteTrait {
    def typeName: String = "byte"
}

case object ShortType extends PrimitiveType with ShortTrait {
    def typeName: String = "short"
}

case object CharType extends PrimitiveType with CharTrait {
    def typeName: String = "char"
}

case object VoidType extends Type {
  def typeName: String = "void"
}

case class ArrayType(elementType: Type) extends Type {
  def typeName: String = elementType.typeName + "[]"
}

abstract class RefType(path:Name) extends Type {
    def typeName: String = path.toString
}

case class RefTypeUnlinked(path: Name) extends RefType(path) {
}

case class RefTypeLinked(pkgName: Option[Name], className:String) extends RefType(Name(pkgName.getOrElse(Name(Nil)).path ::: className::Nil)) with LinkedExpression {
  def getType(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): Type = this
  def getTypeDef(implicit cus:List[CompilationUnit]): TypeDefinition = {
   cus.find(c => c.packageName == pkgName && c.typeName == className).get.typeDef.get
 }
  def generateCode(implicit current:List[Int], params:List[String], pathList:List[List[Int]], cus:List[CompilationUnit]): List[X86Instruction] = ??? //TODO: implementation
}

case object NullType extends Type {
  def typeName: String = "nullType"
}

object Java {
	val Object       = RefTypeLinked(Some(Name(List("java", "lang"))), "Object")
	val Cloneable    = RefTypeLinked(Some(Name(List("java", "lang"))), "Cloneable")
	val Serializable = RefTypeLinked(Some(Name(List("java", "io"))), "Serializable")
}
