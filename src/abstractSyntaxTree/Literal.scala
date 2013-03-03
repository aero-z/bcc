package abstractSyntaxTree

import scanner.IntegerToken
import scanner.StringToken
import scanner.CharacterToken

abstract class Literal extends Expression {
  val children = Nil
}

case class NumberLiteral(int : IntegerToken) extends Literal
case class NullLiteral() extends Literal
case class BooleanLiteral(bool: Boolean) extends Literal
case class CharacterLiteral(char: CharacterToken) extends Literal
case class StringLiteral(str: StringToken) extends Literal


