package abstractSyntaxTree

import parser._
import scanner._

object ASTBuilder {

    def build(parseTree: Symbol, fileName: String): CompilationUnit = parseTree match {
        case NonTerminalSymbol("CompilationUnit", xs) => CompilationUnit(extractPackage(xs),
           extractImport(xs), extractTypeDefinition(xs), fileName)
        case _ => throw new ASTBuildingException("That is not a compilation unit")
    }

    def extractPackage(symbols: List[Symbol]): Option[Name] = symbols.collectFirst({
        case NonTerminalSymbol("PackageDeclaration", xs) => extractName(xs(1))
    })

    def extractImport(symbols: List[Symbol]): List[ImportDeclaration] = ???
    def extractTypeDefinition(symbols: List[Symbol]): Option[TypeDefinition] = ???
    def extractName(name: Symbol): Name ={
        def recExtractName(name: Symbol): List[String] = name match {
        case NonTerminalSymbol("Name", List(NonTerminalSymbol("QualifiedName", List(newName, ScopingToken("."), simpleName)))) => extractSimpleName(simpleName) :: recExtractName(newName)
        case NonTerminalSymbol("Name", List(NonTerminalSymbol("SimpleName", List(simpleName)))) => List(extractSimpleName(simpleName))
    }
        def extractSimpleName(simpleName: Symbol): String = simpleName match{
            case NonTerminalSymbol("SimpleName", List(IdentifierToken(id))) => id
        }

        Name(recExtractName(name).reverse)
    }
    

    class ASTBuildingException(msg:String) extends Exception(msg)

}