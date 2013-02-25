package abstractSyntaxTree

import parser._
import scanner._
import abstractSyntaxTree.Modifier._

object ASTBuilder {
  // Start the extraction of the parse tree.
  def build(parseTree: Symbol, fileName: String): CompilationUnit = parseTree match {
    case NonTerminalSymbol("CompilationUnit", xs) => CompilationUnit(extractPackage(xs),
      extractImport(xs), extractTypeDefinition(xs), fileName)
    case _ => throw new ASTBuildingException("That is not a compilation unit")
  }

  def extractPackage(symbols: List[Symbol]): Option[Name] = symbols.collectFirst({
    case NonTerminalSymbol("PackageDeclaration", xs) => extractName(xs(1))
  })

  def extractImport(symbols: List[Symbol]): List[ImportDeclaration] = symbols.collect({
    case NonTerminalSymbol("ImportDeclaration", List(_, name, _)) => ClassImport(extractName(name))
    case NonTerminalSymbol("ImportDeclaration", List(_, name, _, _, _)) => PackageImport(extractName(name))
  })
  
  def extractTypeDefinition(symbols: List[Symbol]): Option[TypeDefinition] = symbols.collectFirst({
    case NonTerminalSymbol("TypeDeclaration", List(NonTerminalSymbol("ClassDeclaration", xs))) => val (fields,constructors, methods) = extractClassBody(xs)
      ClassDefinition(extractIdentifier(xs), extractParent(xs), extractInterfaces(xs), extractModifiers(xs), fields, constructors, methods)
    case NonTerminalSymbol("TypeDeclaration", List(NonTerminalSymbol("InterfaceDeclaration", xs))) => InterfaceDefinition(extractIdentifier(xs), extractInterfaces(xs), extractModifiers(xs), extractInterfaceMethods(xs))
  })
  
  
  def extractName(name: Symbol): Name = {
    def recExtractName(name: Symbol, acc: List[String]): List[String] = name match {
      case NonTerminalSymbol("Name", List(NonTerminalSymbol("QualifiedName", List(newName, _, simpleName)))) => recExtractName(newName, extractSimpleName(simpleName) :: acc)
      case NonTerminalSymbol("Name", List(NonTerminalSymbol("SimpleName", List(simpleName)))) => extractSimpleName(simpleName) :: acc
    }
    def extractSimpleName(simpleName: Symbol): String = simpleName match {
      case NonTerminalSymbol("SimpleName", List(IdentifierToken(id))) => id
    }

    Name(recExtractName(name, Nil).reverse)
  }

  
  //Find the first identifier of a list of symbols, do not if there is multiple symbol
  def extractIdentifier(symbols: List[Symbol]): String = symbols.collectFirst({
    case IdentifierToken(id) => id
  }).get
  //find John Connor parent and save it from the Terminator
  def extractParent(symbols: List[Symbol]): Option[RefType] = symbols.collectFirst({
    case NonTerminalSymbol("ClassOrInterfaceType", List(name)) => RefType(extractName(name))
  })
  def extractInterfaces(symbols: List[Symbol]): List[RefType] = {
    def recExtractInterfaces(symbols: List[Symbol], acc: List[RefType]): List[RefType] = symbols match{
      case List(NonTerminalSymbol("Interfaces",newinterfaces), _, interface) => recExtractInterfaces(newinterfaces, extractInterface(interface)::acc)
      case List(NonTerminalSymbol("Interface", List(interface))) => extractInterface(interface) :: acc
    }
    def extractInterface(interface: Symbol): RefType = interface match{
      case NonTerminalSymbol("Interface", List(NonTerminalSymbol("ClassOrInterfaceType", List(name)))) => RefType(extractName(name))
    }
    
    symbols.collectFirst({case NonTerminalSymbol("Interfaces", xs) => xs}) match{
      case Some(x) => recExtractInterfaces(x, Nil)
      case None => Nil
    }
  }
  def extractModifiers(symbols: List[Symbol]): List[Modifier] = {
    def recExtractModifiers(symbols: List[Symbol], acc: List[Modifier]): List[Modifier] = symbols match{
      case List(NonTerminalSymbol("Modifiers",newmodifier), modifier) => recExtractModifiers(newmodifier, extractModifier(modifier)::acc)
      case List(modifier) => extractModifier(modifier) :: acc
    }
    def extractModifier(modifier: Symbol): Modifier = modifier match{
      case NonTerminalSymbol("Modifier", List(KeywordToken(str))) => Modifier.fromString(str)
    }
    
    symbols.collectFirst({case NonTerminalSymbol("Modifiers", xs) => xs}) match{
      case Some(x) => recExtractModifiers(x, Nil)
      case None => Nil
    }
  }
  
  def extractClassBody(symbols: List[Symbol]): (List[FieldDeclaration], List[ConstructorDeclaration], List[MethodDeclaration]) ={
    def recExtractClassBody(symbols: List[Symbol], fieldAcc: List[FieldDeclaration], constructorAcc: List[ConstructorDeclaration], methodAcc: List[MethodDeclaration]): (List[FieldDeclaration], List[ConstructorDeclaration], List[MethodDeclaration]) = symbols match{
      case List(NonTerminalSymbol("ClassBodyDeclarations", nextDeclarations), NonTerminalSymbol("ClassBodyDeclaration", List(bodyDeclaration))) => recExtractClassBody(nextDeclarations, concat(extractField(bodyDeclaration), fieldAcc), concat(extractConstructor(bodyDeclaration), constructorAcc), concat(extractMethod(bodyDeclaration), methodAcc))
      case List(NonTerminalSymbol("ClassBodyDeclaration", List(bodyDeclaration))) => (concat(extractField(bodyDeclaration), fieldAcc), concat(extractConstructor(bodyDeclaration), constructorAcc), concat(extractMethod(bodyDeclaration), methodAcc))
    }


    symbols.collectFirst({case NonTerminalSymbol("ClassBodyDeclarations", xs) => xs}) match{
      case Some(x) => recExtractClassBody(x, Nil, Nil, Nil)
      case None => (Nil, Nil, Nil)
    }
  }

  def extractInterfaceMethods(symbols: List[Symbol]): List[MethodDeclaration] = Nil//TODO implement
  def extractField(symbol: Symbol): Option[FieldDeclaration] = symbol match {
    case NonTerminalSymbol("FieldDeclaration", xs) => Some(FieldDeclaration(extractIdentifier(xs), extractType(xs), extractModifiers(xs), extractAssignmentExpression(xs)))
    case _ => None
  }
  def extractConstructor(symbol: Symbol) : Option[ConstructorDeclaration] = symbol match {
    case NonTerminalSymbol("ConstructorDeclaration", xs) => Some(ConstructorDeclaration(extractModifiers(xs), extractParameters(xs), extractBlock(xs).get))
    case _ => None
  }
  def extractMethod(symbol: Symbol) : Option[MethodDeclaration] = symbol match {
    case NonTerminalSymbol("MethodDeclaration", xs) => Some(MethodDeclaration(extractIdentifier(xs), extractType(xs), extractModifiers(xs), extractParameters(xs), extractBlock(xs)))
    case _ => None
  }
  def extractType (symbols: List[Symbol]): Type = symbols.collectFirst({
    case NonTerminalSymbol("Type", List(NonTerminalSymbol("PrimitiveType", List(KeywordToken(x))))) =>PrimitiveType.fromString(x)
    case NonTerminalSymbol("Type", List(NonTerminalSymbol("RefType", List(NonTerminalSymbol("ClassOrInterfaceType", List(name)))))) => RefType(extractName(name))
    case NonTerminalSymbol("Type", List(NonTerminalSymbol("RefType", List(NonTerminalSymbol("ArrayType", List(KeywordToken(x), _, _)))))) => ArrayType(PrimitiveType.fromString(x))
    case NonTerminalSymbol("Type", List(NonTerminalSymbol("RefType", List(NonTerminalSymbol("ArrayType", List(name , _, _)))))) => ArrayType(RefType(extractName(name)))      
  }).get
    
  def extractAssignmentExpression(symbols: List[Symbol]) : Option[Expression] = symbols.collectFirst({
    case NonTerminalSymbol("CondOrExpression", xs) => simplifyExpression(NonTerminalSymbol("CondOrExpression", xs))})

  def extractParameters(symbols: List[Symbol]) : List[(Type, String)] = {
    def recExtractParameters(symbols: List[Symbol], acc: List[(Type, String)]): List[(Type, String)] = symbols match {
      case List(NonTerminalSymbol("ParameterDefs", nextParameters), _, NonTerminalSymbol("ParameterDef", parameter)) => recExtractParameters(nextParameters, extractParameter(parameter) :: acc)
      case List(NonTerminalSymbol("ParameterDef", parameter)) => extractParameter(parameter) :: acc
    }
    def extractParameter(symbols: List[Symbol]): (Type, String) = (extractType(symbols), extractIdentifier(symbols))
    symbols.collectFirst({case NonTerminalSymbol("ParameterDefs", xs) => xs}) match {
      case Some(xs) => recExtractParameters(xs, Nil)
      case None => Nil
    }
  }
  def extractBlock(symbols: List[Symbol]): Option[Block] = symbols.collectFirst({
    case NonTerminalSymbol("Block", List(_, statements, _)) => Block(extractStatements(statements))
    case NonTerminalSymbol("Block", List(_, _)) => Block(Nil)
  })

  def extractStatements(symbol: Symbol): List[Statement] = Nil




  def simplifyExpression(expressionSymbol: Symbol): Expression = null
  def concat[A](option: Option[A], list: List[A]): List[A] = option match {
    case Some(x) => x :: list
    case None => list
  }
  class ASTBuildingException(msg: String) extends Exception(msg)

}
