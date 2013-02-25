package abstractSyntaxTree

object Modifier extends Enumeration{
    type Modifier = Value
    val abstractModifier, finalModifier, nativeModifier,
    privateModifier, protectedModifier, publicModifier, staticModifier = Value
    
    def fromString(str: String): Modifier = str match {
        case "abstract" => abstractModifier
        case "final" => finalModifier
        case "native" => nativeModifier
        case "private" => privateModifier
        case "protected" => protectedModifier
        case "public" => publicModifier
        case "static" => staticModifier
    }
}