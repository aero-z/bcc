package nameResolution

import ast._

class EnvironmentException(message:String) extends Exception

object TypeLinking {
  def treatAll(cus:List[CompilationUnit]):List[CompilationUnit] = {
    val possibleImports = getPossibleImports(cus)
    cus.map(linkCompilationUnit(_, possibleImports))
  }
  def getPossibleImports(cus:List[CompilationUnit]):List[(Option[Name], Name, TypeDefinition)] = {//:(Map[Name, TypeDefinition], Map[Name, List[(Name, TypeDefinition)]]) = { //pcknm -> List[ClassDecl]
    def newName(packageName:Name, className:String):Name = {
      Name(packageName.path ::: "." :: className :: Nil)
    }
    val withDefinition:List[CompilationUnit] = cus.filter(_.typeDef.isDefined) //all cu with an included TypeDefinition
    //concatenate entire string for entire name
    //newName(x.packageName.getOrElse(Name(Nil)), x.typeDef.get.getName).toString
    val names = withDefinition.map(x => x.typeDef.get.getName) //doesn't contain the same simple names
    if (names.length == names.distinct.length)
      throw new EnvironmentException("two typedefinition given as arguments for joosc have the same name!");
    //val possibleSingleImports:Map[Name, TypeDefinition] = withDefinition.map(x => (newName(x.packageName.getOrElse(Name(Nil)), x.typeDef.get.getName), x.typeDef.get)).toMap
    //val possiblePackageImports:Map[Name, List[(Name, TypeDefinition)]] = withDefinition.filter(_.packageName.isDefined).groupBy(_.packageName).toList.map(x => (x._1.get, x._2.map(y => (newName(y.packageName.getOrElse(Name(Nil)), y.typeDef.get.getName), y.typeDef.get)))).toMap
    //(possibleSingleImports, possiblePackageImports)
    withDefinition.map(x => (x.packageName, Name(x.typeDef.get.getName :: Nil), x.typeDef.get))
  }
  def linkCompilationUnit(cu:CompilationUnit, possibleImports:List[(Option[Name], Name, TypeDefinition)]):CompilationUnit = {
    def importSingle(decl:ImportDeclaration, imp:Map[Name, TypeDefinition]):Map[Name, TypeDefinition] = decl match {
	    case ClassImport(name @ Name(path)) =>
	      val packageName = path.dropRight(1) match {case Nil => None case list => Some(Name(list))}
	      val className = Name(List(path.last))
	      val found = possibleImports.find(x => x._1 == packageName && x._2 == className)
	      if (!found.isDefined )
	          throw new EnvironmentException(path+"cannot be imported because it is not part of the possible imports")
	      imp + (name -> found.get._3) + (className -> found.get._3) //add mapping with simple AND full name
	    case PackageImport(name @ Name(path)) => 
	      val imports = possibleImports.filter(_._2 == Some(name))
	      //TODO check if already imported!?
	      if (imports == Nil)
	        imp
	      else 
	        imports.foldLeft(imp)((map,imp) => map + (imp._2 -> imp._3) + (imp._1.getOrElse(Name(Nil)).appendClassName(imp._2) -> imp._3))
    }
    def importList(list:List[ImportDeclaration], imp:Map[Name, TypeDefinition]):Map[Name, TypeDefinition] = list match {
      case Nil => imp
      case x :: xs => importList(xs, importSingle(x, imp))
    }
	def linkAst(cu:CompilationUnit, imported:Map[Name, TypeDefinition], possibleImports:Map[Name, TypeDefinition]):CompilationUnit = {
	    def linkCompilationUnit(cu:CompilationUnit):CompilationUnit = {
		    CompilationUnit(cu.packageName, cu.importDeclarations, cu.typeDef.map(linkTypeDefinition(_)), cu.fileName)
		  }
		  def linkTypeDefinition(td:TypeDefinition):TypeDefinition = td match {
		    case id:InterfaceDefinition => InterfaceDefinition(id.interfaceName, id.parents, id.modifiers, id.methods.map(linkMethod(_)))
		    case cd:ClassDefinition => ClassDefinition(cd.className, cd.parent, cd.interfaces, cd.modifiers, cd.fields.map(linkField(_)), cd.constructors.map(linkConstructor(_)), cd.methods.map(linkMethod(_))) 
		  }
		  def linkMethod(md:MethodDeclaration):MethodDeclaration = {
		    MethodDeclaration(md.methodName, link(md.returnType), md.modifiers, md.parameters.map(linkParameter(_)), md.implementation: Option[Block]) 
		  }
		  def linkField(fd:FieldDeclaration):FieldDeclaration = {
		    FieldDeclaration(fd.fieldName, link(fd.fieldType), fd.modifiers, fd.initializer.map(linkExpression(_)))
		  }
		  def linkConstructor(cd:ConstructorDeclaration):ConstructorDeclaration = {
		    ConstructorDeclaration(cd.modifiers, cd.parameters.map(linkParameter(_)), linkBlock(cd.implementation)) 
		  }
		  def linkParameter(parameter:Parameter):Parameter = {
		    Parameter(link(parameter.paramType), parameter.id)
		  }
		  def linkBlock(block:Block):Block = { //separate linkBlock required for Constructor an other stuff
		    Block(block.statements.map(linkStatement(_)))
		  }
		  def linkStatement(s:Statement):Statement = s match {
		    case Block(statements : List[Statement]) => Block(statements.map(linkStatement(_)))
		    case ExpressionStatement(expression: Expression) =>ExpressionStatement(linkExpression(expression))
		    case ForStatement(init, condition, incrementation, loop) => ForStatement(init.map(linkStatement(_)), condition.map(linkExpression(_)), incrementation.map(linkExpression(_)), linkStatement(loop))
		    case IfStatement(condition: Expression, ifStatement: Statement, elseStatement: Option[Statement]) => IfStatement(linkExpression(condition), linkStatement(ifStatement), elseStatement.map(linkStatement(_)))
		    case ReturnStatement(returnExpression) => ReturnStatement(returnExpression.map(linkExpression(_)))
		    case LocalVariableDeclaration(typeName: Type, identifier: String, initializer: Option[Expression]) => LocalVariableDeclaration(link(typeName), identifier, initializer.map(linkExpression(_)))
		    case WhileStatement(condition: Expression, loop: Statement) => WhileStatement(linkExpression(condition), linkStatement(loop))
		    case _ => s //any other case!
		  }
		  def linkExpression(e:Expression):Expression = e match {
		    case UnaryOperation(operation, term) => UnaryOperation(operation, linkExpression(term))
			case BinaryOperation(first, operation, second) => BinaryOperation(linkExpression(first), operation, linkExpression(second))
			case CastExpression(typeCast, target) => CastExpression(link(typeCast), linkExpression(target))
			case ArrayAccess(typeCast : Expression, target: Expression) => ArrayAccess(linkExpression(typeCast), linkExpression(target))
			case ArrayCreation(typeName : Type, size: Expression) => ArrayCreation(link(typeName), linkExpression(size))
			case Assignment(leftHandSide: Expression, rightHandSide: Expression) => Assignment(linkExpression(leftHandSide), linkExpression(rightHandSide))
			case ClassCreation(constructor: RefType, parameters: List[Expression]) => ClassCreation(link(constructor), parameters.map(linkExpression(_)))
			case FieldAccess(accessed : Expression, field: String) => FieldAccess(linkExpression(accessed), field)
			case MethodInvocation(accessed: Option[Expression], method : String, arguments: List[Expression]) => MethodInvocation(accessed.map(linkExpression(_)), method, arguments.map(linkExpression(_)))
			case InstanceOfCall(exp: Expression, typeChecked: Type) => InstanceOfCall(linkExpression(exp), link(typeChecked))
			case _ => e //return expression by default
		  }
		  def link[A <: Type](pt:A):A = pt match { //anything which is not RefType is simpletype
		    case u:RefTypeUnlinked =>
		      u.asInstanceOf[A] //TODO: resolve name and create new node!
		    case l:RefTypeLinked =>
		      println("already linked! Why are you traversing twice?)");
		      l.asInstanceOf[A]
		    case ArrayType(elementType) => ArrayType(link(elementType)).asInstanceOf[A]
		    case x => x
		  }
		  linkCompilationUnit(cu)
	  }
	val imported = importList(cu.importDeclarations, Map[Name, TypeDefinition]())
    val possible = possibleImports.map(x => (x._1.getOrElse(Name(Nil)).appendClassName(x._2), x._3)).toMap
	linkAst(cu, imported, possible)
  }
}