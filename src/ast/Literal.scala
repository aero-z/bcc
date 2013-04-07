package ast

import scanner.IntegerToken
import scanner.StringToken
import scanner.CharacterToken
import codegen._

abstract class Literal extends Expression

case class NumberLiteral(int: Int) extends Literal {
  def checkAndSetType(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): Type = IntType
  def generateCode(implicit current: List[Int], params: List[String], pathList: List[List[Int]], cus: List[CompilationUnit]) = X86Mov(X86eax, X86Number(int)) :: Nil
}

case object NullLiteral extends Literal {
  def checkAndSetType(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): Type = NullType
  def generateCode(implicit current: List[Int], params: List[String], pathList: List[List[Int]], cus: List[CompilationUnit]) = X86Comment("null:") :: X86Mov(X86eax, X86Number(0)) :: Nil //null -> zero
}

case class BooleanLiteral(bool: Boolean) extends Literal {
  def checkAndSetType(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): Type = BooleanType
  def generateCode(implicit current: List[Int], params: List[String], pathList: List[List[Int]], cus: List[CompilationUnit]) = X86Comment("boolean:") :: X86Mov(X86eax, X86Boolean(bool)) :: Nil
}

case class CharacterLiteral(char: CharacterToken) extends Literal {
  def checkAndSetType(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): Type = CharType
  def generateCode(implicit current: List[Int], params: List[String], pathList: List[List[Int]], cus: List[CompilationUnit]) = X86Comment("character:") :: X86Mov(X86eax, X86Number(char.getInt)) :: Nil
}

case class StringLiteral(str: StringToken) extends Literal {
  def checkAndSetType(implicit cus: List[CompilationUnit], isStatic: Boolean, myType: RefTypeLinked): Type = RefTypeLinked(Some(Name(List("java", "lang"))), "String")
  def generateCode(implicit current: List[Int], params: List[String], pathList: List[List[Int]], cus: List[CompilationUnit]) = ??? // __malloc and stuff X86Mov(X86eax, X86Number(char.getInt)) :: Nil
}

