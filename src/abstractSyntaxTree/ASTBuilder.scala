package abstractSyntaxTree

import parser._
import scanner._
import abstractSyntaxTree.Modifier._
import scala.annotation.tailrec
import main.CompilerError

object ASTBuilder {
  // Start the extraction of the parse tree.
  def build(parseTree: ParserSymbol, fileName: String): CompilationUnit = parseTree match {
    case NonTerminalSymbol("CompilationUnit", xs) => CompilationUnit(extractPackage(xs),
      extractImport(xs), extractTypeDefinition(xs), fileName)
    case _ => throw new ASTBuildingException("That is not a compilation unit")
  }

  def extractPackage(symbols: List[ParserSymbol]): Option[Name] = symbols.collectFirst({
    case NonTerminalSymbol("PackageDeclaration", xs) => extractName(xs(1))
  })

  def extractImport(symbols: List[ParserSymbol]): List[ImportDeclaration] = symbols.collect({
    case NonTerminalSymbol("ImportDeclaration", List(_, name, _)) => ClassImport(extractName(name))
    case NonTerminalSymbol("ImportDeclaration", List(_, name, _, _, _)) => PackageImport(extractName(name))
  })
  
  def extractTypeDefinition(symbols: List[ParserSymbol]): Option[TypeDefinition] = symbols.collectFirst({
    case NonTerminalSymbol("TypeDeclaration", List(NonTerminalSymbol("ClassDeclaration", xs))) => val (fields,constructors, methods) = extractClassBody(xs)
      ClassDefinition(extractIdentifier(xs), extractParent(xs), extractInterfaces(xs), extractModifiers(xs), fields, constructors, methods)
    case NonTerminalSymbol("TypeDeclaration", List(NonTerminalSymbol("InterfaceDeclaration", xs))) => InterfaceDefinition(extractIdentifier(xs), extractInterfaces(xs), extractModifiers(xs), extractInterfaceMethods(xs))
  })
  
  
  def extractName(name: ParserSymbol): Name = {
    def recExtractName(name: ParserSymbol, acc: List[String]): List[String] = name match {
      case NonTerminalSymbol("Name", List(NonTerminalSymbol("QualifiedName", List(newName, _, NonTerminalSymbol("SimpleName", List(simpleName)))))) => recExtractName(newName, extractSimpleName(simpleName) :: acc)
      case NonTerminalSymbol("Name", List(NonTerminalSymbol("SimpleName", List(simpleName)))) => extractSimpleName(simpleName) :: acc
    }
    def extractSimpleName(simpleName: ParserSymbol): String = simpleName match {
      case IdentifierToken(id) => id
    }

    Name(recExtractName(name, Nil).reverse)
  }

  
  //Find the first identifier of a list of symbols, do not if there is multiple symbol
  def extractIdentifier(symbols: List[ParserSymbol]): String = symbols.collectFirst({
    case IdentifierToken(id) => id
  }).get
  //find John Connor parent and save it from the Terminator
  def extractParent(symbols: List[ParserSymbol]): Option[RefType] = symbols.collectFirst({
    case NonTerminalSymbol("ClassOrInterfaceType", List(name)) => RefType(extractName(name))
  })
  def extractInterfaces(symbols: List[ParserSymbol]): List[RefType] = {
    def recExtractInterfaces(symbols: List[ParserSymbol], acc: List[RefType]): List[RefType] = symbols match{
      case List(NonTerminalSymbol("Interfaces",newinterfaces), _, interface) => recExtractInterfaces(newinterfaces, extractInterface(interface)::acc)
      case List(interface) => extractInterface(interface) :: acc
    }
    def extractInterface(interface: ParserSymbol): RefType = interface match{
      case NonTerminalSymbol("ClassOrInterfaceType", List(name)) => RefType(extractName(name))
    }
    
    symbols.collectFirst({case NonTerminalSymbol("Interfaces", xs) => xs}) match{
      case Some(x) => recExtractInterfaces(x, Nil)
      case None => Nil
    }
  }
  def extractModifiers(symbols: List[ParserSymbol]): List[Modifier] = {
    def recExtractModifiers(symbols: List[ParserSymbol], acc: List[Modifier]): List[Modifier] = symbols match{
      case List(NonTerminalSymbol("Modifiers",newmodifier), modifier) => recExtractModifiers(newmodifier, extractModifier(modifier)::acc)
      case List(modifier) => extractModifier(modifier) :: acc
    }
    def extractModifier(modifier: ParserSymbol): Modifier = modifier match{
      case NonTerminalSymbol("Modifier", List(KeywordToken(str))) => Modifier.fromString(str)
    }
    
    symbols.collectFirst({case NonTerminalSymbol("Modifiers", xs) => xs}) match{
      case Some(x) => recExtractModifiers(x, Nil)
      case None => Nil
    }
  }
  
  def extractClassBody(symbols: List[ParserSymbol]): (List[FieldDeclaration], List[ConstructorDeclaration], List[MethodDeclaration]) ={
    def recExtractClassBody(symbols: List[ParserSymbol], fieldAcc: List[FieldDeclaration], constructorAcc: List[ConstructorDeclaration], methodAcc: List[MethodDeclaration]): (List[FieldDeclaration], List[ConstructorDeclaration], List[MethodDeclaration]) = symbols match{
      case List(NonTerminalSymbol("ClassBodyDeclarations", nextDeclarations), NonTerminalSymbol("ClassBodyDeclaration", List(bodyDeclaration))) => recExtractClassBody(nextDeclarations, concat(extractField(bodyDeclaration), fieldAcc), concat(extractConstructor(bodyDeclaration), constructorAcc), concat(extractMethod(bodyDeclaration), methodAcc))
      case List(NonTerminalSymbol("ClassBodyDeclaration", List(bodyDeclaration))) => (concat(extractField(bodyDeclaration), fieldAcc), concat(extractConstructor(bodyDeclaration), constructorAcc), concat(extractMethod(bodyDeclaration), methodAcc))
    }


    symbols.collectFirst({case NonTerminalSymbol("ClassBodyDeclarations", xs) => xs}) match{
      case Some(x) => recExtractClassBody(x, Nil, Nil, Nil)
      case None => (Nil, Nil, Nil)
    }
  }

  def extractInterfaceMethods(symbols: List[ParserSymbol]): List[MethodDeclaration] = Nil//TODO implement
  def extractField(symbol: ParserSymbol): Option[FieldDeclaration] = symbol match {
    case NonTerminalSymbol("FieldDeclaration", xs) => Some(FieldDeclaration(extractIdentifier(xs), extractType(xs), extractModifiers(xs), extractAssignmentExpression(xs)))
    case _ => None
  }
  def extractConstructor(symbol: ParserSymbol) : Option[ConstructorDeclaration] = symbol match {
    case NonTerminalSymbol("ConstructorDeclaration", xs) => Some(ConstructorDeclaration(extractModifiers(xs), extractParameters(xs), extractBlock(xs).get))
    case _ => None
  }
  def extractMethod(symbol: ParserSymbol) : Option[MethodDeclaration] = symbol match {
    case NonTerminalSymbol("MethodDeclaration", xs) => Some(MethodDeclaration(extractIdentifier(xs), extractType(xs), extractModifiers(xs), extractParameters(xs), extractBlock(xs)))
    case _ => None
  }
  def extractType (symbols: List[ParserSymbol]): Type = symbols.collectFirst({
    case NonTerminalSymbol("Type", List(NonTerminalSymbol("PrimitiveType", List(KeywordToken(x))))) =>PrimitiveType.fromString(x)
    case KeywordToken("void") => VoidType
    case NonTerminalSymbol("Type", List(NonTerminalSymbol("RefType", List(NonTerminalSymbol("ClassOrInterfaceType", List(name)))))) => RefType(extractName(name))
    case NonTerminalSymbol("Type", List(NonTerminalSymbol("RefType", List(NonTerminalSymbol("ArrayType", List(KeywordToken(x), _, _)))))) => ArrayType(PrimitiveType.fromString(x))
    case NonTerminalSymbol("Type", List(NonTerminalSymbol("RefType", List(NonTerminalSymbol("ArrayType", List(name , _, _)))))) => ArrayType(RefType(extractName(name)))
  }).get
  
  def extractAssignmentExpression(symbols: List[ParserSymbol]) : Option[Expression] = symbols.collectFirst({
    case NonTerminalSymbol("CondOrExpression", xs) => simplifyExpression(NonTerminalSymbol("CondOrExpression", xs))})

  def extractParameters(symbols: List[ParserSymbol]) : List[(Type, String)] = {
    def recExtractParameters(symbols: List[ParserSymbol], acc: List[(Type, String)]): List[(Type, String)] = symbols match {
      case List(NonTerminalSymbol("ParameterDefs", nextParameters), _, NonTerminalSymbol("ParameterDef", parameter)) => recExtractParameters(nextParameters, extractParameter(parameter) :: acc)
      case List(NonTerminalSymbol("ParameterDef", parameter)) => extractParameter(parameter) :: acc
    }
    def extractParameter(symbols: List[ParserSymbol]): (Type, String) = (extractType(symbols), extractIdentifier(symbols))
    symbols.collectFirst({case NonTerminalSymbol("ParameterDefs", xs) => xs}) match {
      case Some(xs) => recExtractParameters(xs, Nil).reverse
      case None => Nil
    }
  }
  def extractBlock(symbols: List[ParserSymbol]): Option[Block] = symbols.collectFirst({
    case NonTerminalSymbol("Block", List(_, statements, _)) => Block(extractStatements(statements))
    case NonTerminalSymbol("Block", List(_, _)) => Block(Nil)
  })

  def extractStatements(symbol: ParserSymbol): List[Statement] = Nil



  val binaryExpId = List("MultiplicativeExpression", "AdditiveExpression", "RelationalExpression", "EqualityExpression", "BitAndExpression", "BitXorExpression", "BitOrExpression", "CondAndExpression", "CondOrExpression")
  def simplifyExpression(expressionSymbol: ParserSymbol): Expression ={
    @tailrec
    def nameToFieldAccess(name : List[String], acc : Option[FieldAccess]) : FieldAccess = name match {
      case x :: xs => nameToFieldAccess(xs, Some(FieldAccess(acc, x)))
      case Nil => acc.get
    }
    @tailrec
    def recExtractArguments(symbol: ParserSymbol, acc: List[Expression]): List[Expression] = symbol match {
      case  NonTerminalSymbol("ArgumentList", List(next, _, exp)) => recExtractArguments(next, simplifyExpression(exp):: acc)
      case NonTerminalSymbol("ArgumentList", List(exp)) => (simplifyExpression(exp) :: acc). reverse
    }

    def extractCastType(symbol: ParserSymbol): Type = symbol match{
      case NonTerminalSymbol("PrimitiveCast", List(_, NonTerminalSymbol("PrimitiveType", List(KeywordToken(str))), _)) => PrimitiveType.fromString(str)
      case NonTerminalSymbol("PrimitiveCast", List(_, NonTerminalSymbol("PrimitiveType", List(KeywordToken(str))), _, _, _)) => ArrayType(PrimitiveType.fromString(str))
      case NonTerminalSymbol("NonPrimCast", List(_, name, _, _, _)) => ArrayType(RefType(extractName(name)))
      case NonTerminalSymbol("NonPrimCast", List(_, name, _)) => RefType((new Function1[ParserSymbol, Name]{ def apply(x: ParserSymbol)= x match{
        case name @ NonTerminalSymbol("Name", _) => extractName(name)
        case NonTerminalSymbol(_, List(next)) => apply(next)
        case _ => throw new CompilerError("Wrong cast expression")
      }})(name))
    }



    expressionSymbol match {
      case NonTerminalSymbol("ParenthesizedExpression", List(_,exp, _)) => simplifyExpression(exp)
      case NonTerminalSymbol( str, List(exp)) if binaryExpId contains str => simplifyExpression(exp)
      case NonTerminalSymbol( str, List(exp1, OperatorToken(op), exp2)) if binaryExpId contains str => BinaryOperation(simplifyExpression(exp1), Operator.fromString(op), simplifyExpression(exp2))
      case NonTerminalSymbol("RelationalExpression", List(exp, KeywordToken("instanceof"), reftype)) => InstanceOfCall(simplifyExpression(exp), extractType(List(reftype)))
      case NonTerminalSymbol("Assignment", List(lhs, _, exp)) => Assignment(simplifyExpression(lhs), simplifyExpression(exp))
      case NonTerminalSymbol("LeftHandSide", List(exp)) => simplifyExpression(exp)
      case xs @ NonTerminalSymbol("Name", _) => nameToFieldAccess(extractName(xs).path, None)
      case KeywordToken("this") => This
      case NonTerminalSymbol("FieldAccess", List(pri, _, IdentifierToken(str))) => FieldAccess(Some(simplifyExpression(pri)), str)
      case NonTerminalSymbol("PrimaryNoNewArray" , List(exp)) => simplifyExpression(exp)
      case NonTerminalSymbol("Literal", List(exp)) => simplifyExpression(exp)
      case exp : Expression => exp
      case NonTerminalSymbol("ClassInstanceCreation", List(_, cons, _, _)) => ClassCreation(RefType(extractName(cons)), Nil)
      case NonTerminalSymbol("ClassInstanceCreation", List(_, cons, _, arg, _)) => ClassCreation(RefType(extractName(cons)), recExtractArguments(arg, Nil))
      case NonTerminalSymbol("MethodInvocation", List(IdentifierToken(str), _, _)) => MethodInvocation(None, str, Nil)
      case NonTerminalSymbol("MethodInvocation", List(IdentifierToken(str), _, arg, _)) => MethodInvocation(None, str, recExtractArguments(arg, Nil))
      case NonTerminalSymbol("MethodInvocation", List(access, _, IdentifierToken(str), _, _)) => MethodInvocation(Some(simplifyExpression(access)), str, Nil)
      case NonTerminalSymbol("MethodInvocation", List(access, _, IdentifierToken(str), _ , arg, _)) => MethodInvocation(Some(simplifyExpression(access)), str, recExtractArguments(arg, Nil))
      case NonTerminalSymbol("Primary", List(exp)) => simplifyExpression(exp)
      case NonTerminalSymbol("PostFixExpression", List(exp)) => simplifyExpression(exp)
      case NonTerminalSymbol("PrimaryNoNewArray", List(exp)) => simplifyExpression(exp)
      case NonTerminalSymbol("ArrayAccess", List( array, _, index, _)) => ArrayAccess(simplifyExpression(array), simplifyExpression(index))
      case NonTerminalSymbol("ArrayCreation", List(_, NonTerminalSymbol("PrimitiveType", List(KeywordToken(arrayType))), _, size, _)) => ArrayCreation(ArrayType(PrimitiveType.fromString(arrayType)), simplifyExpression(size))
      case NonTerminalSymbol("ArrayCreation", List(_, NonTerminalSymbol("RefType", List(arrayType)), _, size, _))=> ArrayCreation(ArrayType(RefType(extractName(arrayType))), simplifyExpression(size))
      case NonTerminalSymbol("UnaryExpression", List(exp)) => simplifyExpression(exp)
      case NonTerminalSymbol("UnaryExpressionNotPlusMinus", List(exp)) => simplifyExpression(exp)
      case NonTerminalSymbol("UnaryExpression", List(OperatorToken(op), exp)) => UnaryOperation(Operator.fromString(op), simplifyExpression(exp))
      case NonTerminalSymbol("UnaryExpressionNotPlusMinus", List(OperatorToken(op), exp)) => UnaryOperation(Operator.fromString(op), simplifyExpression(exp))
      case NonTerminalSymbol("CastExpression", List(cast, exp)) => CastExpression(extractCastType(cast), simplifyExpression(exp))
    }
  }
  def concat[A](option: Option[A], list: List[A]): List[A] = option match {
    case Some(x) => x :: list
    case None => list
  }
  class ASTBuildingException(msg: String) extends Exception(msg)

}
