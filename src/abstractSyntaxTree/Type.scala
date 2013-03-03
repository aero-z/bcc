package abstractSyntaxTree

abstract class Type extends AstNode{
    def typeName:String
}

abstract class PrimitiveType extends Type

object PrimitiveType{
  def fromString(str: String): PrimitiveType = str match{
    case "int" => IntType
    case "boolean" => BooleanType
    case "byte" => ByteType
    case "short" => ShortType
    case "char" => CharType
    case "void" => VoidType
  }
}

case object IntType extends PrimitiveType{
    def typeName: String = "int"
}

case object BooleanType extends PrimitiveType{
    def typeName: String = "boolean"
}

case object ByteType extends PrimitiveType{
    def typeName: String = "byte"
}

case object ShortType extends PrimitiveType{
    def typeName: String = "short"
}

case object CharType extends PrimitiveType{
    def typeName: String = "char"
}

case object VoidType extends PrimitiveType{
  def typeName: String = "void"
}

case class ArrayType(elementType: Type) extends Type{
    def typeName: String = elementType.typeName + "[]"
}

abstract class RefType(path:Name) extends Type {
    def typeName: String = path.toString
}

case class RefTypeUnlinked(path: Name) extends RefType(path) {
}

case class RefTypeLinked(path:Name, decl:Declaration) extends RefType(path) {
  
}