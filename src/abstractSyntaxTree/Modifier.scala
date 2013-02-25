package abstractSyntaxTree

object Modifier extends Enumeration{
  type Modifier = Value
  val abstractModifier, finalModifier, nativeModifier,
    privateModifier, protectedModifier, publicModifier, staticModifier = Value
  val map = Map( "abstract" -> abstractModifier,
    "final" -> finalModifier,
    "native" -> nativeModifier,
    "private" -> privateModifier,
    "protected" -> protectedModifier,
    "public" -> publicModifier,
    "static" -> staticModifier)
  def fromString(str: String): Modifier = map.get(str).get
  def fromModifier(mod: Modifier): String = map.map( _.swap).get(mod).get
}
