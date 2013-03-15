package ast

import scanner.IntegerToken
import scanner.StringToken
import scanner.CharacterToken



abstract class Literal extends Expression

case class NumberLiteral(int: Int) extends Literal{
  def getType(implicit cus: List[CompilationUnit], isStatic: Boolean, classDef: ClassDefinition): Type = IntType
}

case object NullLiteral extends Literal{
  def getType(implicit cus: List[CompilationUnit], isStatic: Boolean, classDef: ClassDefinition): Type = NullType
}

case class BooleanLiteral(bool: Boolean) extends Literal{
  def getType(implicit cus: List[CompilationUnit], isStatic: Boolean, classDef: ClassDefinition): Type = BooleanType
}

case class CharacterLiteral(char: CharacterToken) extends Literal{
  def getType(implicit cus: List[CompilationUnit], isStatic: Boolean, classDef: ClassDefinition): Type = CharType
}

case class StringLiteral(str: StringToken) extends Literal{
  def getType(implicit cus: List[CompilationUnit], isStatic: Boolean, classDef: ClassDefinition): Type = RefTypeLinked(Some(Name(List("java", "lang"))), "String")
}


