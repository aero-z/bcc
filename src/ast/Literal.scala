package ast

import scanner.IntegerToken
import scanner.StringToken
import scanner.CharacterToken



abstract class Literal extends Expression

case class NumberLiteral(int: Int) extends Literal{
  lazy val getType: Type = IntType
}

case object NullLiteral extends Literal{
  lazy val getType: Type = NullType
}

case class BooleanLiteral(bool: Boolean) extends Literal{
  lazy val getType: Type = BooleanType
}

case class CharacterLiteral(char: CharacterToken) extends Literal{
  lazy val getType: Type = CharType
}

case class StringLiteral(str: StringToken) extends Literal{
  lazy val getType: Type = RefTypeLinked(Some(Name(List("java", "lang"))), "String")
}


