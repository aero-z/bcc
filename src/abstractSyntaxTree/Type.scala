package abstractSyntaxTree



abstract class Type{
    def typeName:String
}

abstract class PrimitiveType extends Type

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


case class ArrayType(elementType: Type) extends Type{
    def typeName: String = elementType.typeName + "[]"
}

case class RefType(path : Name) extends Type{
    def typeName: String = path.toString
}