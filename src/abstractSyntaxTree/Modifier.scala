package abstractSyntaxTree

object Modifier extends Enumeration{
    type Modifier = Value
    val abstractModifier, finalModifier, nativeModifier,
    privateModifier, protectedModifier, publicModifier, staticModifier = Value
    
    def modifierToString(mod: Modifier): String = mod match {
        case `abstractModifier` => "abstract"
        case `finalModifier` => "final"
        case `nativeModifier` => "native"
        case `privateModifier` => "private"
        case `protectedModifier` => "protected"
        case `publicModifier` => "public"
        case `staticModifier` => "static"
    }
}