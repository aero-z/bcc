package ast

import scanner.IntegerToken
import scanner.StringToken
import scanner.CharacterToken
import codegen._

abstract class Literal extends Expression

case class NumberLiteral(int: Int) extends Literal{
  def getType(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): Type = IntType
  def generateCode(): List[X86Instruction] = ??? //TODO: implementation
}

case object NullLiteral extends Literal{
  def getType(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): Type = NullType
  def generateCode(): List[X86Instruction] = ??? //TODO: implementation
}

case class BooleanLiteral(bool: Boolean) extends Literal{
  def getType(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): Type = BooleanType
  def generateCode(): List[X86Instruction] = ??? //TODO: implementation
}

case class CharacterLiteral(char: CharacterToken) extends Literal{
  def getType(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): Type = CharType
  def generateCode(): List[X86Instruction] = ??? //TODO: implementation
}

case class StringLiteral(str: StringToken) extends Literal{
  def getType(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): Type = RefTypeLinked(Some(Name(List("java", "lang"))), "String")
  def generateCode(): List[X86Instruction] = ??? //TODO: implementation
}


