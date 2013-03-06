package ast

import parser._
import scanner._
import ast.Modifier._
import scala.annotation.tailrec
import main.CompilerError
import java.io.File
import main.CompilerError

object AstBuilder {
  // Start the extraction of the parse tree.
  def build(parseTree: ParserSymbol, filePath: String): CompilationUnit = parseTree match {
    case NonTerminalSymbol("CompilationUnit", List(pack, imp, typeDef)) =>
      val fileName = (new File(filePath)).getName()
      val dotIndex = fileName.lastIndexOf('.')
      if (dotIndex < 0) throw CompilerError("file name must end with .java")
      val typeName = fileName.substring(0,dotIndex)
      CompilationUnit(extractPackage(pack), extractImports(imp), extractTypeDefinition(typeDef), typeName)
    case _ => throw new AstBuildingException("That is not a compilation unit")
  }

  def extractPackage(symbol: ParserSymbol): Option[Name] = symbol match {
    case NonTerminalSymbol("OptPackage", Nil) => None
    case NonTerminalSymbol("OptPackage", List(_, name, _)) => Some(extractName(name))
  }

  def extractImports(symbol: ParserSymbol): List[ImportDeclaration] = {
    @tailrec
    def recExtractImports(symbol: ParserSymbol, acc: List[ImportDeclaration]): List[ImportDeclaration] = symbol match {
      case NonTerminalSymbol("ImportDeclarations", List(next, imp)) => recExtractImports(next, extractImport(imp) :: acc)
      case NonTerminalSymbol("ImportDeclarations", List(imp)) => extractImport(imp)::acc
    }
    def extractImport(symbol: ParserSymbol) : ImportDeclaration = symbol match {
      case NonTerminalSymbol("ImportDeclaration", List(_, name, _)) => ClassImport(extractName(name))
      case NonTerminalSymbol("ImportDeclaration", List(_, name, _, _, _)) => PackageImport(extractName(name))
    }
    symbol match {
      case NonTerminalSymbol("AnyImport", Nil) => Nil
      case NonTerminalSymbol("AnyImport", List(xs)) => recExtractImports(xs, Nil)
    }
  }
  
  def extractTypeDefinition(symbol: ParserSymbol): Option[TypeDefinition] = symbol match {
    case NonTerminalSymbol("OptTypeDeclaration", List(NonTerminalSymbol("ClassDeclaration", List(mod, _, IdentifierToken(id), parent, interfaces, _, body, _)))) => val (fields, constructors, methods) = extractClassBody(body); Some(ClassDefinition(id, extractParent(parent), extractInterfaces(interfaces), extractModifiers(mod), fields, constructors, methods))
    case NonTerminalSymbol("OptTypeDeclaration", List(NonTerminalSymbol("InterfaceDeclaration", List(mod, _, IdentifierToken(id), parent, _, body, _)))) => Some(InterfaceDefinition(id, extractInterfaces(parent), extractModifiers(mod), extractInterfaceMethods(body)))
    case NonTerminalSymbol("OptTypeDeclaration", Nil) => None
  }

  
  
  
  def extractName(name: ParserSymbol): Name = {
    @tailrec
    def recExtractName(name: ParserSymbol, acc: List[String]): List[String] = name match {
      case NonTerminalSymbol("Name", List(next)) => recExtractName(next, acc)
      case NonTerminalSymbol("QualifiedName", List(next, _, IdentifierToken(id))) => recExtractName(next, id :: acc)
      case NonTerminalSymbol("SimpleName", List(IdentifierToken(id))) => id :: acc //don't .reverse!
    }
    

    Name(recExtractName(name, Nil))
  }

  
  
  //find John Connor parent and save it from the Terminator
  def extractParent(symbol: ParserSymbol): Option[RefType] = symbol match {
    case NonTerminalSymbol("OptClassParent", List( _, refType)) => extractParent(refType)
    case NonTerminalSymbol("OptClassParent", Nil) => None
    case NonTerminalSymbol("ClassOrInterfaceType", List(name)) => Some(RefTypeUnlinked(extractName(name)))
  }

  def extractInterfaces(symbol: ParserSymbol): List[RefType] = {
    @tailrec
    def recExtractInterfaces(symbol: ParserSymbol, acc: List[RefType]): List[RefType] = symbol match{
      case NonTerminalSymbol("Interfaces", List(next, _, interface)) => recExtractInterfaces(next, extractInterface(interface)::acc)
      case NonTerminalSymbol("Interfaces", List(interface)) => extractInterface(interface) :: acc
    }
    def extractInterface(interface: ParserSymbol): RefType = interface match{
      case NonTerminalSymbol("ClassOrInterfaceType", List(name)) => RefTypeUnlinked(extractName(name))
    }
    
    symbol match{
      case NonTerminalSymbol(str, Nil) if str startsWith "OptInterface" => Nil
      case NonTerminalSymbol(str, List(_, interfaces)) if str startsWith "OptInterface" => recExtractInterfaces(interfaces, Nil)
    }
  }

  def extractModifiers(symbol: ParserSymbol): List[Modifier] = {
    @tailrec
    def recExtractModifiers(symbol: ParserSymbol, acc: List[Modifier]): List[Modifier] = symbol match{
      case NonTerminalSymbol("Modifiers", List(next, modifier)) => recExtractModifiers(next, extractModifier(modifier)::acc)
      case NonTerminalSymbol("Modifiers", List(modifier)) => extractModifier(modifier) :: acc
    }

    def extractModifier(modifier: ParserSymbol): Modifier = modifier match{
      case NonTerminalSymbol("Modifier", List(KeywordToken(str))) => Modifier.fromString(str)
    }
    
    symbol match {
      case NonTerminalSymbol("AnyModifiers", List(modifiers)) => recExtractModifiers(modifiers, Nil)
      case NonTerminalSymbol("AnyModifiers", Nil) => Nil
    }
  }
  
  def extractClassBody(symbol: ParserSymbol): (List[FieldDeclaration], List[ConstructorDeclaration], List[MethodDeclaration]) ={
    type ReturnType =  (List[FieldDeclaration], List[ConstructorDeclaration], List[MethodDeclaration])
    @tailrec
    def recExtractClassBody(symbol: ParserSymbol, acc: ReturnType): ReturnType = symbol match{
      case NonTerminalSymbol("ClassBodyDeclarations", List(next, decl)) => recExtractClassBody(next, accumulate(decl, acc))
      case NonTerminalSymbol("ClassBodyDeclarations", List(decl)) => accumulate(decl, acc)
    }

    

    def accumulate(symbol: ParserSymbol, acc: ReturnType) = symbol match {
      case NonTerminalSymbol("ClassBodyDeclaration", List(field @ NonTerminalSymbol("FieldDeclaration", _))) => (extractField(field) :: acc._1, acc._2, acc._3)
      case NonTerminalSymbol("ClassBodyDeclaration", List(cons @ NonTerminalSymbol("ConstructorDeclaration", _))) => ( acc._1, extractConstructor(cons):: acc._2, acc._3)
      case NonTerminalSymbol("ClassBodyDeclaration", List(meth @ NonTerminalSymbol("MethodDeclaration", _))) => ( acc._1, acc._2, extractMethod(meth) :: acc._3)
      case NonTerminalSymbol("ClassBodyDeclaration", List(semicolon @ ScopingToken(";"))) => acc
    }

    symbol match{
      case NonTerminalSymbol("AnyClassBody", List(body)) => recExtractClassBody(body, (Nil, Nil, Nil))
      case NonTerminalSymbol("AnyClassBody", Nil) => (Nil, Nil, Nil)
    }
  }

  def extractInterfaceMethods(symbol: ParserSymbol): List[MethodDeclaration] ={
    @tailrec
    def recExtractInterfaceBody(symbol: ParserSymbol, acc: List[MethodDeclaration]): List[MethodDeclaration] = symbol match {
      case NonTerminalSymbol("InterfaceBodyDeclarations", List(next, ScopingToken(";"))) => recExtractInterfaceBody(next, acc)
      case NonTerminalSymbol("InterfaceBodyDeclarations", List(ScopingToken(";"))) => acc.reverse
      case NonTerminalSymbol("InterfaceBodyDeclarations", List(next, decl)) => recExtractInterfaceBody(next, extractMethod(decl) :: acc)
      case NonTerminalSymbol("InterfaceBodyDeclarations", List(decl)) => extractMethod(decl) :: acc
    }
    symbol match {
      case NonTerminalSymbol("AnyInterfaceBody", List(bodyDecl)) => recExtractInterfaceBody(bodyDecl, Nil)
      case NonTerminalSymbol("AnyInterfaceBody", Nil) => Nil
    }
  }
    
  def extractField(symbol: ParserSymbol): FieldDeclaration = symbol match {
    case NonTerminalSymbol("FieldDeclaration", List(mod, fieldType, IdentifierToken(id), _)) => FieldDeclaration(id, extractType(fieldType), extractModifiers(mod), None)
    case NonTerminalSymbol("FieldDeclaration", List(mod, fieldType, IdentifierToken(id), _, exp, _)) => FieldDeclaration(id, extractType(fieldType), extractModifiers(mod), Some(simplifyExpression(exp)))
  }

  def extractConstructor(symbol: ParserSymbol) : ConstructorDeclaration = symbol match {
    case NonTerminalSymbol("ConstructorDeclaration", List(mod, IdentifierToken(id), _, params, _, block)) => ConstructorDeclaration(extractModifiers(mod), extractParameters(params), extractBlock(block))
  }

  def extractMethod(symbol: ParserSymbol) : MethodDeclaration = symbol match {
    case NonTerminalSymbol("MethodDeclaration", List(mod, methType, IdentifierToken(id), _, params, _, ScopingToken(";"))) => MethodDeclaration(id, extractType(methType), extractModifiers(mod), extractParameters(params), None)
    case NonTerminalSymbol("MethodDeclaration", List(mod, methType, IdentifierToken(id), _, params, _, block)) => MethodDeclaration(id, extractType(methType), extractModifiers(mod), extractParameters(params), Some(extractBlock(block)))
  }
  def extractType(symbol :ParserSymbol): Type  = symbol match {
    case NonTerminalSymbol("Type", List(next)) => extractType(next)
    case NonTerminalSymbol("PrimitiveType", List(next)) => extractType(next)
    case KeywordToken(str) => PrimitiveType.fromString(str)
    case NonTerminalSymbol("RefType", List(next)) => extractType(next)
    case NonTerminalSymbol("ArrayType", List(primType @ NonTerminalSymbol("PrimitiveType", _), _, _)) => ArrayType(extractType(primType))
    case NonTerminalSymbol("ArrayType", List(nameType @ NonTerminalSymbol("Name", _), _, _)) => ArrayType(RefTypeUnlinked(extractName(nameType)))
    case NonTerminalSymbol("ClassOrInterfaceType", List(name)) => RefTypeUnlinked(extractName(name))
  }

  def extractParameters(symbol: ParserSymbol) : List[Parameter] = {
    @tailrec
    def recExtractParameters(symbol: ParserSymbol, acc: List[Parameter]): List[Parameter] = symbol match {
      case NonTerminalSymbol("ParameterDefs", List(next, _, param)) => recExtractParameters(next, extractParameter(param) :: acc)
      case NonTerminalSymbol("ParameterDefs", List(param)) => extractParameter(param) :: acc
    }

    def extractParameter(symbol: ParserSymbol): Parameter = symbol match {
      case NonTerminalSymbol("ParameterDef", List(paramType, IdentifierToken(id))) =>Parameter(extractType(paramType), id)
    }

    symbol match {
      case NonTerminalSymbol("AnyParameterDefs", List(parameters)) => recExtractParameters(parameters, Nil)
      case NonTerminalSymbol("AnyParameterDefs", Nil) => Nil
    }
  }

  def extractBlock(symbol: ParserSymbol): Block = symbol match{
    case NonTerminalSymbol("Block", List(_, statements, _)) => Block(extractStatements(statements))
    case NonTerminalSymbol("Block", List(_, _)) => Block(Nil)
  }

  def extractStatements(symbol: ParserSymbol): List[Statement] ={

    val recStId = List("Statement", "StatementWithoutTrailingSubstatement", "StatementNoShortIf")


    @tailrec
    def recExtractStatements(symbol: ParserSymbol, acc: List[Statement]) : List[Statement] = symbol match {
      case NonTerminalSymbol("Statements", List(next, statement)) => recExtractStatements(next, extractStatement(statement) :: acc)
      case NonTerminalSymbol("Statements", List(statement)) =>extractStatement(statement) :: acc
    }


    def extractStatement(symbol: ParserSymbol) : Statement = symbol match {
      case NonTerminalSymbol(id , List(next)) if recStId contains id => extractStatement(next)
      case block @ NonTerminalSymbol("Block", _) => extractBlock(block)
      case NonTerminalSymbol("EmptyStatement", List(_)) => EmptyStatement
      case NonTerminalSymbol("ExpressionStatement", List(exp, _)) => ExpressionStatement(simplifyExpression(exp))
      case NonTerminalSymbol("ReturnStatement", List(_, _)) => ReturnStatement(None)
      case NonTerminalSymbol("ReturnStatement", List(_, exp, _)) => ReturnStatement(Some(simplifyExpression(exp)))
      case NonTerminalSymbol("StatementWithoutTrailingSubstatement", List(lvd, _)) => extractStatement(lvd)
      case NonTerminalSymbol("LocalVariableDeclaration", List(varType, IdentifierToken(id))) => LocalVariableDeclaration(extractType(varType), id, None)
      case NonTerminalSymbol("LocalVariableDeclaration", List(varType, IdentifierToken(id), _, exp)) => LocalVariableDeclaration(extractType(varType), id, Some(simplifyExpression(exp)))
      case NonTerminalSymbol("IfThenStatement", List(_, _, exp, _, stmt)) => IfStatement(simplifyExpression(exp), extractStatement(stmt), None)
      case NonTerminalSymbol("IfThenElseStatement", List(_, _, exp, _, stmt1, _, stmt2)) => IfStatement(simplifyExpression(exp), extractStatement(stmt1), Some(extractStatement(stmt2)))
      case NonTerminalSymbol("IfThenElseStatementNoShortIf", List(_, _, exp, _, stmt1, _, stmt2)) => IfStatement(simplifyExpression(exp), extractStatement(stmt1), Some(extractStatement(stmt2)))
      case NonTerminalSymbol("WhileStatement", List(_, _, exp, _, stmt)) => WhileStatement(simplifyExpression(exp), extractStatement(stmt))
      case NonTerminalSymbol("WhileStatementNoShortIf", List(_, _, exp, _, stmt)) => WhileStatement(simplifyExpression(exp), extractStatement(stmt))
      case NonTerminalSymbol("ForStatement", List(head, stmt)) => val (init, cond, inc) = extractForHeader(head); ForStatement(init, cond, inc, extractStatement(stmt))
      case NonTerminalSymbol("ForStatementNoShortIf", List(head, stmt)) => val (init, cond, inc) = extractForHeader(head); ForStatement(init, cond, inc, extractStatement(stmt))
    }


    def extractForHeader(symbol: ParserSymbol) : (Option[Statement], Option[Expression], Option[Expression])= symbol match {
      case NonTerminalSymbol("ForHeader", List(_, _, init, _, cond, _, inc, _)) => (extractForInit(init), extractForCond(cond), extractForInc(inc))
    }
    def extractForInit(symbol: ParserSymbol): Option[Statement] = symbol match {
      case NonTerminalSymbol("ForInit", List(exp @ NonTerminalSymbol("StatementExpression", _)))=> Some(ExpressionStatement(simplifyExpression(exp)))
      case NonTerminalSymbol("ForInit", List(stmt @ NonTerminalSymbol("LocalVariableDeclaration", _))) => Some(extractStatement(stmt))
      case NonTerminalSymbol("ForInit", Nil) => None
    }

    def extractForCond(symbol: ParserSymbol): Option[Expression] = symbol match {
      case NonTerminalSymbol("ForCond", List(exp)) => Some(simplifyExpression(exp))
      case NonTerminalSymbol("ForCond", Nil) => None
    }

    def extractForInc(symbol: ParserSymbol): Option[Expression] = symbol match {
      case NonTerminalSymbol("ForInc", List(exp)) => Some(simplifyExpression(exp))
      case NonTerminalSymbol("ForInc", Nil) => None
    }

    recExtractStatements(symbol, Nil)
  }
  

  def simplifyExpression(expressionSymbol: ParserSymbol): Expression = simplifyExpression(expressionSymbol, false)
  def simplifyExpression(expressionSymbol: ParserSymbol, unaryMinus: Boolean): Expression ={
    @tailrec
    def nameToFieldAccess(name : List[String], acc : Expression) : FieldAccess = name match {
      case x :: Nil => FieldAccess(acc, x)
      case x :: xs => nameToFieldAccess(xs, FieldAccess(acc, x))
    }

    def extractCastType(symbol: ParserSymbol): Type = symbol match{
      case NonTerminalSymbol("PrimitiveCast", List(_, NonTerminalSymbol("PrimitiveType", List(KeywordToken(str))), _)) => PrimitiveType.fromString(str)
      case NonTerminalSymbol("PrimitiveCast", List(_, NonTerminalSymbol("PrimitiveType", List(KeywordToken(str))), _, _, _)) => ArrayType(PrimitiveType.fromString(str))
      case NonTerminalSymbol("NonPrimCast", List(_, name, _, _, _)) => ArrayType(RefTypeUnlinked(extractName(name)))
      case NonTerminalSymbol("NonPrimCast", List(_, name, _)) => RefTypeUnlinked((new Function1[ParserSymbol, Name]{ def apply(x: ParserSymbol)= x match{
        case name @ NonTerminalSymbol("Name", _) => extractName(name)
        case NonTerminalSymbol(_, List(next)) => apply(next)
        case _ => throw new AstBuildingException("Wrong cast expression")
      }})(name))
    }

    val binaryExpId = List("MultiplicativeExpression", "AdditiveExpression", "RelationalExpression", "EqualityExpression", "BitAndExpression", "BitXorExpression", "BitOrExpression", "CondAndExpression", "CondOrExpression")
    val recExId = binaryExpId ::: List("LeftHandSide", "PrimaryNoNewArray", "Literal", "Primary", "PostFixExpression", "UnaryExpression", "UnaryExpressionNotPlusMinus", "StatementExpression", "Expression", "AssignmentExpression")

    expressionSymbol match {
      case NonTerminalSymbol("ParenthesizedExpression", List(_,exp, _)) => simplifyExpression(exp)
      case NonTerminalSymbol("RelationalExpression", List(exp, KeywordToken("instanceof"), reftype)) => InstanceOfCall(simplifyExpression(exp), extractType(reftype))
      case NonTerminalSymbol( str, List(exp)) if recExId contains str => simplifyExpression(exp, unaryMinus)
      case NonTerminalSymbol( str, List(exp1, OperatorToken(op), exp2)) if binaryExpId contains str => BinaryOperation(simplifyExpression(exp1), Operator.fromString(op), simplifyExpression(exp2))
      case NonTerminalSymbol("Assignment", List(lhs, _, exp)) => Assignment(simplifyExpression(lhs), simplifyExpression(exp))
      case xs @ NonTerminalSymbol("Name", _) => val name = extractName(xs).path; 
        if(name.tail == Nil)  VariableAccess(name.head) else nameToFieldAccess(name.tail, VariableAccess(name.head))
      case KeywordToken("this") => This(null)
      case NonTerminalSymbol("FieldAccess", List(pri, _, IdentifierToken(str))) => FieldAccess(simplifyExpression(pri), str)
      case IntegerToken(intStr) => {
        val opIntStr = if (unaryMinus) "-" + intStr
                       else intStr
        val int = try { opIntStr.toInt }
                  catch { case _: Throwable => throw CompilerError("bad integer literal "+opIntStr); ??? }
        NumberLiteral(int)
      }
      case st : StringToken => StringLiteral(st)
      case BooleanToken(bool) => BooleanLiteral(bool)
      case char : CharacterToken => CharacterLiteral(char)
      case NullToken => NullLiteral
      case NonTerminalSymbol("ClassInstanceCreation", List(_, cons, _, arg, _)) => ClassCreation(RefTypeUnlinked(extractName(cons)), extractArguments(arg))
      case NonTerminalSymbol("MethodInvocation", List(IdentifierToken(str), _, arg, _)) => MethodInvocation(None, str, extractArguments(arg))
      case NonTerminalSymbol("MethodInvocation", List(access, _, IdentifierToken(str), _ , arg, _)) => MethodInvocation(Some(simplifyExpression(access)), str, extractArguments(arg))
      case NonTerminalSymbol("ArrayAccess", List( array, _, index, _)) => ArrayAccess(simplifyExpression(array), simplifyExpression(index))
      case NonTerminalSymbol("ArrayCreation", List(_, arrayType, _, size, _)) => ArrayCreation(ArrayType(extractType(arrayType)), simplifyExpression(size))
      case NonTerminalSymbol("UnaryExpression", List(OperatorToken(op), exp)) => {
        val operator = Operator.fromString(op)
        val expression = simplifyExpression(exp, true)
        (operator, expression) match {
          case (Operator.minus, n @ NumberLiteral(int)) => n
          case _ => UnaryOperation(operator, expression)
        }
      }
      case NonTerminalSymbol("UnaryExpressionNotPlusMinus", List(OperatorToken(op), exp)) => UnaryOperation(Operator.fromString(op), simplifyExpression(exp, unaryMinus))
      case NonTerminalSymbol("CastExpression", List(cast, exp)) => CastExpression(extractCastType(cast), simplifyExpression(exp))
    }
  }
  
  def extractArguments(symbol: ParserSymbol): List[Expression] = {

    @tailrec
    def recExtractArguments(symbol: ParserSymbol, acc: List[Expression]): List[Expression] = symbol match {
      case  NonTerminalSymbol("ArgumentsList", List(next, _, exp)) => recExtractArguments(next, simplifyExpression(exp):: acc)
      case NonTerminalSymbol("ArgumentsList", List(exp)) => simplifyExpression(exp) :: acc
    }

    symbol match {
      case NonTerminalSymbol("AnyArguments", List(arg)) => recExtractArguments(arg, Nil)
      case NonTerminalSymbol("AnyArguments", Nil) => Nil
    }

  }

  class AstBuildingException(msg: String) extends CompilerError(msg)

}
