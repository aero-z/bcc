package nameResolution

import ast._

class EnvironmentException(message:String) extends Exception(message)

object TypeLinking {
  def treatAll(cus:List[CompilationUnit]):List[CompilationUnit] = {
    println("CREATE POSSIBLE IMPORTS:")
    //val possibleImports = getPossibleImports(cus)
    println("TREAT FILES SEPARATELY:")
    //cus.map(linkCompilationUnit(_, possibleImports))
    lazy val myList:List[CompilationUnit] = cus.map(linkCompilationUnit(_, getPossibleImports(cus, myList)))
    myList
  }
  def getPossibleImports(cus:List[CompilationUnit], typeDefList: => List[CompilationUnit]):List[(Option[Name], String,() => TypeDefinition)] = {//:(Map[Name, TypeDefinition], Map[Name, List[(Name, TypeDefinition)]]) = { //pcknm -> List[ClassDecl]
    println("GET POSSIBLE IMPORTS:")
    def newName(packageName:Name, className:String):Name = {
      Name(packageName.path ::: "." :: className :: Nil)
    }
    val withDefinition:List[CompilationUnit] = cus.filter(_.typeDef.isDefined) //all cu with an included TypeDefinition
    withDefinition.foreach(x => println("added to possible imports: "+x.typeDef.get.getName))
    //concatenate entire string for entire name
    //newName(x.packageName.getOrElse(Name(Nil)), x.typeDef.get.getName).toString
    val names = withDefinition.map(x => x.typeDef.get.getName) //doesn't contain the same simple names
    println("nb. of names: "+names.length)
    println("nb. of distinct names: "+names.distinct.length)
    if (names.length != names.distinct.length)
      throw new EnvironmentException("two typedefinition given as arguments for joosc have the same name!");
    //val possibleSingleImports:Map[Name, TypeDefinition] = withDefinition.map(x => (newName(x.packageName.getOrElse(Name(Nil)), x.typeDef.get.getName), x.typeDef.get)).toMap
    //val possiblePackageImports:Map[Name, List[(Name, TypeDefinition)]] = withDefinition.filter(_.packageName.isDefined).groupBy(_.packageName).toList.map(x => (x._1.get, x._2.map(y => (newName(y.packageName.getOrElse(Name(Nil)), y.typeDef.get.getName), y.typeDef.get)))).toMap
    //(possibleSingleImports, possiblePackageImports)
    val result = withDefinition.map(x => (x.packageName, x.typeDef.get.getName, () => {typeDefList.find(cu => cu.packageName == x.packageName && cu.typeDef.get.getName == x.typeDef.get.getName).get.typeDef.get}))
    println("imports")
    result.foreach(x => println("package:"+x._1+" class:"+x._2))
    result
  }
  def linkCompilationUnit(cu:CompilationUnit, possibleImports:List[(Option[Name], String, () => TypeDefinition)]):CompilationUnit = {
    println("LINKCOMPILATIONSUNIT:")
    def checkPrefix(name:Name, map:Map[Name, () => TypeDefinition]) {
      if (name.path.length <= 1) return
      for (i <- (1 to name.path.length-1 by 1)) {
        if (map.get(Name(name.path.take(i))).isDefined)
          throw new EnvironmentException("Error: imported prefix: "+map.get(Name(name.path.take(i))).get)
      }
    }
    def importSelf(map:Map[Name, () => TypeDefinition]):Map[Name, () => TypeDefinition] = cu.packageName match {
      case Some(pkgname) =>
        println("IMPORT SELF: SOME")
        val selfMap = map + (Name(cu.typeName::Nil) -> (() => cu.typeDef.get))
        val fullName = Name(pkgname.path ::: List(cu.typeName))
        checkPrefix(fullName, selfMap)
        map+(Name(cu.typeName::Nil)->(() =>cu.typeDef.get))+(fullName ->(() => cu.typeDef.get))
      case None =>
        println("IMPORT SELF: NONE")
        map + (Name(cu.typeName::Nil) -> (() =>cu.typeDef.get))
    }
    def importClass(name:Name, map:Map[Name, () => TypeDefinition]):Map[Name, () => TypeDefinition] = {
      println("IMPORT CLASS")
      val className = name.path.last
      val packageName = name match {case Name(x::Nil) => None case Name(xs) => Some(Name(xs.dropRight(1)))}//might be Name(Nil)!
      val typeDef:() => TypeDefinition = possibleImports.find(x => x._1 == packageName && x._2 == className) match {case Some(found) => found._3 case None => throw new EnvironmentException("Error: not part of possible imports: "+name)}
      if (map.contains(Name(className::Nil)))
        throw new EnvironmentException("Error: imports with same name: "+className)
      val mapBuff = map+(Name(className::Nil) -> typeDef)
      packageName match {
        case None => mapBuff
        case Some(name) => checkPrefix(name, mapBuff); mapBuff+(name -> typeDef)
      }
    }
    def importPackage(packageName:Name, map:Map[Name, () => TypeDefinition]):Map[Name, () => TypeDefinition] = {
      println("IMPORT PACKAGE")
      def rec(className:Name, typeDef:() => TypeDefinition, map:Map[Name, () => TypeDefinition]):Map[Name, () => TypeDefinition] = {
        val fullName = Name(packageName.path:::className.path)
        checkPrefix(fullName, map)
        val withClass =
	        if (map.contains(className))
	          map
	        else
	          map + (className -> typeDef)
	    withClass  + (fullName -> typeDef)
      }
      val classes = possibleImports.filter(_._1 == Some(packageName)).map(x => (Name(x._2::Nil), x._3)) //fullName, typeDef
      classes.foldLeft(map)((map, row) => rec(row._1, row._2, map))
    }
    def importAll(list:List[ImportDeclaration]):Map[Name, () => TypeDefinition] = {
      println("IMPORTALL")
      val classes = list.filter{case ClassImport(_) => true case _ => false}.map(_.getName)
      val packages = list.filter{case PackageImport(_) => true case _ => false}.map(_.getName)
      val mapSelf = importSelf(Map[Name, () => TypeDefinition]())
      println("IMPORT SELF:")
      mapSelf.foreach(x => println(x._1))
      val classImports = classes.foldLeft(mapSelf)((map:Map[Name, () => TypeDefinition], y:Name) => importClass(y, map))
      println("IMPORT CLASSES:")
      classImports.foreach(x => println(x._1))
      println("IMPORT MY PACKAGE:")
      val myPackage = 
        if (cu.packageName.isDefined)
          importPackage(cu.packageName.get, classImports);
        else
      		classImports
      myPackage.foreach(x => println(x._1))
      val packageImports = packages.foldLeft(myPackage)((map:Map[Name, () => TypeDefinition], y:Name) => importPackage(y, map))
      println("IMPORT PACKAGES:")
      packageImports.foreach(x => println(x._1))
      packageImports
    }
    /*def importSingle(decl:ImportDeclaration, imp:Map[Name, TypeDefinition]):Map[Name, TypeDefinition] = decl match {
	    case ClassImport(name @ Name(path)) =>
	      println("IMPORT CLASS:"+name)
	      val packageName = path.dropRight(1) match {case Nil => None case list => Some(Name(list))}
	      val className = path.last
	      println("possible imports:")
	      possibleImports.foreach(x => println(x._1+" "+x._2))
	      println("try to find pkg:"+packageName+" className:"+className)
	      val found = possibleImports.find(x => x._1 == packageName && x._2 == className)
	      if (!found.isDefined )
	          throw new EnvironmentException(path+"cannot be imported because it is not part of the possible imports")
	      imp + (name -> found.get._3) + (Name(className::Nil) -> found.get._3) //add mapping with simple AND full name
	    case PackageImport(name @ Name(path)) => 
	      println("IMPORT PACKAGE:"+name)
	      val imports = possibleImports.filter(_._1 == Some(name))
	      //TODO check if already imported!?
	      if (imports == Nil)
	        imp
	      else 
	        imports.foldLeft(imp)((map,imp) => map + (Name(imp._2::Nil) -> imp._3) + (imp._1.getOrElse(Name(Nil)).appendClassName(imp._2) -> imp._3))
    }
    def importClass(imp:ClassImport, map:Map[Name, TypeDefinition]) {
      val className = imp.name.path.last
      val packageName = Name(imp.name.path.dropRight(1)) //drop the classname
      if (map.contains(imp.name))// || Name(cu.packageName.getOrElse(Name(Nil)).appendClassName(name)))
        throw new EnvironmentException("Single Type Import conflict:"+imp.name+" already defined!")
      val found = possibleImports.find(x => x._1 == packageName && x._2 == className)
	      if (!found.isDefined )
	          throw new EnvironmentException(packageName+" "+className+"cannot be imported because it is not part of the possible imports")
      //if (possibleImports)
    }
    def importList(list:List[ImportDeclaration], imp:Map[Name, TypeDefinition]):Map[Name, TypeDefinition] = list match {
      case Nil => println("nothing to import"); imp
      case x :: xs => println("importing:"+x.getName); importList(xs, importSingle(x, imp))
    }*/
	def linkAst(cu:CompilationUnit, imported:Map[Name, () => TypeDefinition], possibleImports:Map[Name, () => TypeDefinition]):CompilationUnit = {
	  println("LINK AST:")
	    def linkCompilationUnit(cu:CompilationUnit):CompilationUnit = {
		  println("LINK COMPILATIONUNIT:")
		    CompilationUnit(cu.packageName, cu.importDeclarations, cu.typeDef.map(linkTypeDefinition(_)).map(_()), cu.typeName)
		  }
		  def linkTypeDefinition(td:TypeDefinition):() => TypeDefinition = td match {
		    case id:InterfaceDefinition =>println("LINK INTERFACE:"); () => InterfaceDefinition(id.interfaceName, id.parents.map(link(_)), id.modifiers, id.methods.map(linkMethod(_)))
		    case cd:ClassDefinition =>println("LINK CLASS:"); () => ClassDefinition(cd.className, cd.parent.map(link(_)), cd.interfaces.map(link(_)), cd.modifiers, cd.fields.map(linkField(_)), cd.constructors.map(linkConstructor(_)), cd.methods.map(linkMethod(_))) 
		  }
		  def linkMethod(md:MethodDeclaration):MethodDeclaration = {
		    println("LINK METHOD:")
		    MethodDeclaration(md.methodName, link(md.returnType), md.modifiers, md.parameters.map(linkParameter(_)), md.implementation.map(linkBlock(_))) 
		  }
		  def linkField(fd:FieldDeclaration):FieldDeclaration = {
		    println("LINK FIELD:")
		    FieldDeclaration(fd.fieldName, link(fd.fieldType), fd.modifiers, fd.initializer.map(linkExpression(_)))
		  }
		  def linkConstructor(cd:ConstructorDeclaration):ConstructorDeclaration = {
		    println("LINK CONSTRUCTOR:")
		    ConstructorDeclaration(cd.modifiers, cd.parameters.map(linkParameter(_)), linkBlock(cd.implementation)) 
		  }
		  def linkParameter(parameter:Parameter):Parameter = {
		    println("LINK PARAMETER:")
		    Parameter(link(parameter.paramType), parameter.id)
		  }
		  def linkBlock(block:Block):Block = { //separate linkBlock required for Constructor an other stuff
		    println("LINK BLOCK:")
		    Block(block.statements.map(linkStatement(_)))
		  }
		  def linkStatement(s:Statement):Statement =  s match {
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
		      println("LINK REFERENCETYPE")
		      println("to be found: "+u.path+" length:"+u.path.toString.length)
		      imported.foreach(x => println("name:"+x._1+" length:"+x._1.path.length))
		      //imported.toList.foreach(x =>println(x._1+" -> "+x._1.path.length))
		      val typ = imported.get(u.path)
		      println("found?: "+typ.isDefined)
		      if (typ == None)
		        throw new EnvironmentException(u.typeName+" not imported yet!")
		      //println("name resolution: "+u.typeName+" -> "+typ.get.getName)
		      //println("XXBEFOREXXX "+cu.fileName+": "+u.hashCode)
		      val niew = RefTypeLinked(u.path, typ.get()).asInstanceOf[A] //TODO: resolve name and create new node!
		      println("XXAFTERXXX "+cu.typeName+": "+niew.hashCode)
		      niew
		    case l:RefTypeLinked =>
		      println("already linked! Why are you traversing twice?)");
		      l.asInstanceOf[A]
		    case ArrayType(elementType) => println("arraytype -> no link:");; ArrayType(link(elementType)).asInstanceOf[A]
		    case x => println("Simple type, no link:"); x
		  }
		  linkCompilationUnit(cu)
	}
	
	val possibleList = possibleImports.map(x => (x._1.getOrElse(Name(Nil)).appendClassName(x._2), x._3))
	val possible = possibleList.toMap
	if (possibleList.length != possible.size)
	  println("Some stuff has been overwritten!")
    println("all possible imports")
	possible.foreach(x => println("name:"+x._1))
	//val imported = importList(cu.importDeclarations, Map[Name, TypeDefinition]())
	val imported = importAll(cu.importDeclarations)
	println("imported:")
	imported.foreach(x => println("name:"+x._1))
    
	linkAst(cu, imported, possible)
  }
}
