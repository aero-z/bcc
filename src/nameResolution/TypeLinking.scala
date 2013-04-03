package nameResolution

import ast._
import main.Logger.debug
import main.Joosc

class EnvironmentException(message:String) extends main.CompilerError(message, "Environment building")

object TypeLinking {
	type NameMap = Map[Name, (Option[Name], String)]
	
	def treatAll(cus:List[CompilationUnit]):List[CompilationUnit] = {
		debug("CREATE POSSIBLE IMPORTS:")
		val possibleImports = getPossibleImports(cus)
		debug("TREAT FILES SEPARATELY:")
		cus.map(linkCompilationUnit(_, possibleImports))
	}
	def getPossibleImports(cus:List[CompilationUnit]):List[(Option[Name], String)] = {//:(Map[Name, TypeDefinition], Map[Name, List[(Name, TypeDefinition)]]) = { //pcknm -> List[ClassDecl]
		debug("GET POSSIBLE IMPORTS:")
		val names = cus.filter(_.typeDef.isDefined).map(x => x.packageName.getOrElse(Name(Nil)).appendClassName(x.typeDef.get.getName))
		if (names.length != names.distinct.length)
			throw new EnvironmentException("two typedefinition given as arguments for joosc have the same name!");
		cus.filter(_.typeDef.isDefined).map(x => (x.packageName, x.typeName))
	}
	def linkCompilationUnit(cu:CompilationUnit, possibleImports:List[(Option[Name], String)]):CompilationUnit = {
	 // if (cu.packageName == Some(Name(cu.typeName::Nil)))
	//WHUT?	  throw new EnvironmentException("prefix: type and package have the same name!");
		debug("LINKCOMPILATIONSUNIT:")
		/*def checkPrefix(name:Name, map:NameMap) {
			for (i <- (1 to name.path.length by 1)) { //everything except the full name
				if (map.get(Name(name.path.take(i))).isDefined)
					throw new EnvironmentException("Error: imported prefix: "+map.get(Name(name.path.take(i))).get)
			}
		}*/
		def importClass(fullName:Name, map:NameMap):NameMap = {
			//already distinct!
			debug("IMPORT CLASS")
			val (packageName, className) = splitFullName(fullName)
			if (map.contains(Name(className::Nil))) //imports with the same name!
				throw new EnvironmentException("imports with same name: "+className)
			if (!possibleImports.contains((packageName, className)))
				throw new EnvironmentException("not part of possible imports: "+fullName)
			map + (Name(className::Nil) -> (packageName, className)) + (fullName -> (packageName, className))
		}
		//you CANNOT import classes from the default package into named packages
		def importPackage(packageName:Option[Name], map:NameMap):NameMap = {
			debug("IMPORT PACKAGE")
			val pkgName = packageName.getOrElse(Name(Nil))
			def rec(className:String, recMap:NameMap):NameMap = {
				val clssName = Name(className::Nil)
				val fullName = pkgName.appendClassName(className)
				if (map.contains(clssName)) //class with same name already imported
					recMap + (fullName -> (packageName, className))
				else
					recMap + (fullName -> (packageName, className)) + (clssName -> (packageName, className))
			}
			possibleImports.filter(_._1 == packageName) match {
				case Nil => throw new EnvironmentException("Package not found: "+packageName) //packages have to exist!
				case x => x.foldLeft(map)((m, i) => rec(i._2, m))
			}
		}
		def splitFullName(name:Name):(Option[Name], String) =name.path match {
			case single :: Nil => (None, single)
			case list => (Some(Name(list.dropRight(1))), list.last)
		}
		def importAll(classes:List[Name]):NameMap = {
			debug("IMPORTALL")
			val emptyMap = Map[Name, (Option[Name], String)]()
			val fullName = cu.packageName.getOrElse(Name(Nil)).appendClassName(cu.typeName)
			val myMap = emptyMap + (Name(cu.typeName::Nil) -> (cu.packageName, cu.typeName)) + (fullName -> (cu.packageName, cu.typeName)) //might overwrite the old one
			//don't import yourself again
			val classImports = classes.filter(_ != fullName).foldLeft(myMap)((map:NameMap, y:Name) => importClass(y, map))
			val myPackage = importPackage(cu.packageName, classImports)
			debug("IMPORT SELF:")
			myMap.foreach(x => debug(x._1))
			debug("IMPORT CLASSES:")
			classImports.foreach(x => debug(x._1))
			//classImports.foreach(x => x._2._1 match {case Some(pkgname) => checkPrefix(pkgname, packageImports) case None => })
			debug("IMPORT MY PACKAGE:")
			myPackage.foreach(x => debug(x._1))
			myPackage //java.lang => import on demand
		}
		def linkAst(cu:CompilationUnit, imported:NameMap, onDemandImports:List[(Option[Name], String)], directAccess:NameMap):CompilationUnit = {
			debug("LINK AST:")
			def checkPrefix(names:List[String]):Int = {/*println("find: "+names);*/ names match { //return type should be discarted
			  case Nil => 0 //recursive functions need return type
			  case x :: Nil => //clash EXCEPT default package!
			    0
			    /*if (cu.packageName != None)
			      0 //don't look in the default package when the class itself is not in the default package!
			    val possiblePackage = names.dropRight(1)
			    if (imported.get(Name(names)).isDefined)
			      throw new EnvironmentException("invalid prefix: "+x+" clashes with "+imported.get(Name(names)).get)
			    if (onDemandImports.filter(x => x._1 == Some(Name(possiblePackage)) && x._2 == x).length != 0)
			      throw new EnvironmentException("invalid prefix: "+x+" clashes with "+possiblePackage+" -> "+x)
			    if (directAccess.get(Name(names)).isDefined)
			      throw new EnvironmentException("invalid prefix: "+x+" clashes with:"+directAccess.get(Name(names)))
			    0*/
			  case x :: xs => //at least 2 elements
			    val possibleClass = names.last;
			    val possiblePackage = names.dropRight(1)
			    if (imported.get(Name(names)).isDefined) throw new EnvironmentException("invalid prefix: "+x+" clashes with "+imported.get(Name(names)).get)
			    if (onDemandImports.filter(x => x._1 == Some(Name(possiblePackage)) && x._2 == possibleClass).length != 0) throw new EnvironmentException("invalid prefix: "+x+" clashes with "+possiblePackage+" -> "+possibleClass)
			    if (directAccess.get(Name(names)).isDefined) throw new EnvironmentException("invalid prefix: "+x+" clashes with:"+directAccess.get(Name(names)))
			    checkPrefix(possiblePackage);
			}}
			def linkCompilationUnit(cu:CompilationUnit):CompilationUnit = {
				debug("LINK COMPILATIONUNIT:")
				val onDemandList = onDemandImports.flatMap(x => (Name(x._2::Nil), (x._1, x._2)) :: (x._1.getOrElse(Name(Nil)).appendClassName(x._2), (x._1, x._2)) :: Nil)
				val newImportDeclarations = (directAccess.toList ::: imported.toList ::: onDemandList).distinct.map(x => LinkImport(x._1.path.reduce((x,y)=>x+"."+y), RefTypeLinked(x._2._1, x._2._2))).toList
				CompilationUnit(cu.packageName, newImportDeclarations, cu.typeDef.map(linkTypeDefinition(_)), cu.typeName)
			}
			def linkTypeDefinition(td:TypeDefinition):TypeDefinition = td match {
				case id:InterfaceDefinition =>debug("LINK INTERFACE:"); InterfaceDefinition(id.interfaceName, id.parents.map(link(_)), id.modifiers, id.methods.map(linkMethod(_)))
				case cd:ClassDefinition =>debug("LINK CLASS:"); ClassDefinition(cd.className,
                if (Joosc.linkJavaLang)
                  if(cu.packageName == Some(Name(List("java", "lang"))) &&  cu.typeName == "Object")
                    None
                  else
                    Some(cd.parent.map(link(_)).getOrElse(Java.Object))
                else cd.parent.map(link(_)),
                  cd.interfaces.map(link(_)), cd.modifiers, cd.fields.map(linkField(_)), cd.constructors.map(linkConstructor(_)), cd.methods.map(linkMethod(_)))
			}
			def linkMethod(md:MethodDeclaration):MethodDeclaration = {
				debug("LINK METHOD:")
				MethodDeclaration(md.methodName, link(md.returnType), md.modifiers, md.parameters.map(linkParameter(_)), md.implementation.map(linkBlock(_))) 
			}
			def linkField(fd:FieldDeclaration):FieldDeclaration = {
				debug("LINK FIELD:")
				FieldDeclaration(fd.fieldName, link(fd.fieldType), fd.modifiers, fd.initializer.map(linkExpression(_)))
			}
			def linkConstructor(cd:ConstructorDeclaration):ConstructorDeclaration = {
				debug("LINK CONSTRUCTOR:")
				ConstructorDeclaration(cd.name, cd.modifiers, cd.parameters.map(linkParameter(_)), linkBlock(cd.implementation)) 
			}
			def linkParameter(parameter:Parameter):Parameter = {
				debug("LINK PARAMETER:")
				Parameter(link(parameter.paramType), parameter.id)
			}
			def linkBlock(block:Block):Block = { //separate linkBlock required for Constructor an other stuff
				debug("LINK BLOCK:")
				Block(block.statements.map(linkStatement(_)))
			}
			def linkStatement(s:Statement):Statement =  s match {
				case Block(statements : List[Statement]) => Block(statements.map(linkStatement(_)))
				case ExpressionStatement(expression: Expression) =>ExpressionStatement(linkExpression(expression))
				case ForStatement(init, condition, incrementation, loop) => ForStatement(init.map(linkStatement(_)), condition.map(linkExpression(_)), incrementation.map(linkExpression(_)), linkStatement(loop))
				case IfStatement(condition: Expression, ifStatement: Statement, elseStatement: Option[Statement]) => IfStatement(linkExpression(condition), linkStatement(ifStatement), elseStatement.map(linkStatement(_)))
				case ReturnStatement(returnExpression) => debug("return");ReturnStatement(returnExpression.map(linkExpression(_)))
				case LocalVariableDeclaration(typeName: Type, identifier: String, initializer: Option[Expression]) => LocalVariableDeclaration(link(typeName), identifier, initializer.map(linkExpression(_)))
				case WhileStatement(condition: Expression, loop: Statement) => WhileStatement(linkExpression(condition), linkStatement(loop))
				case _ => s //any other case!
			}
			def linkExpression(e:Expression):Expression = e match {
				case ParenthesizedExpression(exp) => ParenthesizedExpression(linkExpression(exp))
				case UnaryOperation(operation, term) => UnaryOperation(operation, linkExpression(term))
				case BinaryOperation(first, operation, second) => BinaryOperation(linkExpression(first), operation, linkExpression(second))
				case CastExpression(typeCast, target) => CastExpression(link(typeCast), linkExpression(target))
				case ArrayAccess(typeCast : Expression, target: Expression) => ArrayAccess(linkExpression(typeCast), linkExpression(target))
				case ArrayCreation(typeName : Type, size: Expression) => ArrayCreation(link(typeName), linkExpression(size))
				case Assignment(leftHandSide: Expression, rightHandSide: Expression) => Assignment(linkExpression(leftHandSide), linkExpression(rightHandSide))
				case ClassCreation(constructor: RefType, parameters: List[Expression]) => ClassCreation(link(constructor), parameters.map(linkExpression(_)))
				case FieldAccess(accessed : Expression, field: String) => FieldAccess(linkExpression(accessed), field)
				case ExprMethodInvocation(accessed: Expression, method : String, arguments: List[Expression]) => ExprMethodInvocation(linkExpression(accessed), method, arguments.map(linkExpression(_)))
				case ThisMethodInvocation(null, method : String, arguments: List[Expression]) => ThisMethodInvocation(RefTypeLinked(cu.packageName, cu.typeName), method, arguments.map(linkExpression(_)))
				case InstanceOfCall(exp: Expression, typeChecked: Type) => InstanceOfCall(linkExpression(exp), link(typeChecked))
				case This(null) => This(RefTypeLinked(cu.packageName, cu.typeName))
				case _ => e //return expression by default
			}
			def link[A <: Type](pt:A):A = pt match { //anything which is not RefType is simpletype
				case RefTypeUnlinked(name) =>
					debug("LINK REFERENCETYPE")
					debug("to be found: "+name)
					imported.get(name) match {
						case Some(tuple) => 
						  debug("case: imported: "+tuple);
						  if (tuple._1.isDefined) {
						    //TODO: this is shit!
							  val discard = checkPrefix(tuple._1.get.path)
						  }
						  RefTypeLinked(tuple._1, tuple._2).asInstanceOf[A]
						case None => //no class import
							splitFullName(name) match {
							case (None, cname) => //on demand import 
								onDemandImports.filter(_._2 == cname) match {
									case Nil => debug("No class found with name: "+cname); throw new EnvironmentException("No class found with name: "+cname)
									case head :: Nil => debug("case: import on demand: "+head); RefTypeLinked(onDemandImports.find(_._2 == cname).get._1, cname).asInstanceOf[A]
									case head :: second :: tail => debug("Two different possible imports found for: "+cname); throw new EnvironmentException("Two different possible imports found for: "+cname)
								}
							case (pname @ Some(pn), cname) => //direct access
								val discard = checkPrefix(pn.path.dropRight(1)) //runtime check if there is a prefix
								directAccess.get(name) match {
									case Some(tuple) => debug("case: direct access :"+tuple); RefTypeLinked(tuple._1, tuple._2).asInstanceOf[A]//if not imported can still be used via direct name!
									case None => debug("not possible to import: "+name.path); throw new EnvironmentException("not possible to import: "+name.path)
								}
							}
					}
				case l:RefTypeLinked =>
					debug("already linked! Why are you traversing twice?)");
					l.asInstanceOf[A]
				case ArrayType(elementType) => debug("arraytype -> no link:"); ArrayType(link(elementType)).asInstanceOf[A]
				case x => debug("Simple type, no link:"); x
			}
			//get strict prefix of all
			
			val prefixes = (imported.keys.toList.map{case Name(x) => x.dropRight(1)} ::: onDemandImports.filter(_._1 != None).map(x => x._1.get.path)).filter(_ != Nil).distinct
			val discard = prefixes.foreach(checkPrefix(_))
			linkCompilationUnit(cu);
		}
		debug("+++++In "+cu.packageName+" :"+cu.typeName)
		
		
		val possibleList =
		  if (cu.packageName == None)
		    possibleImports.map(x => (x._1.getOrElse(Name(Nil)).appendClassName(x._2), (x._1, x._2)))
		  else
		    possibleImports.filter(_._1.isDefined).map(x => (x._1.getOrElse(Name(Nil)).appendClassName(x._2), (x._1, x._2)))

		val classes = cu.importDeclarations.filter{case ClassImport(_) => true case _ => false}.distinct.map(_.getName)
		val packages = cu.importDeclarations.filter{case PackageImport(_) => true case _ => false}.distinct.map(_.getName)

		val imported = importAll(classes)
		//check for "empty" packages as well:
		def isPrefix(s:List[String]):Boolean = {
		  if (s==Nil)
		    false
		  val packageNames = possibleImports.map(_._1).filter(_!=None).map(_.get).map{case Name(list) => list}.filter(_!=Nil).distinct
		  packageNames.exists(x => x.startsWith(s))
		}
		val onDemandImports = (Name("java"::"lang"::Nil) :: packages).distinct.flatMap(p => possibleImports.filter(_._1 == Some(p)) match { case Nil  => if (!isPrefix(p.path)) throw new EnvironmentException("empty import"+p) else Nil case x => x})
	
		val directAccess = possibleList.toMap //all direct accesses: fullName -> tuple

		if (possibleList.length != directAccess.size)
			debug("Some stuff has been overwritten!")
		cu.packageName match {
		  case Some(Name("java"::xs)) =>
		  case _ =>
			debug("BEFORE LINK AST")
			debug("imported:")
			imported.foreach(x => debug("name:"+x._1))
			debug("packages:")
			packages.foreach(debug(_))
			debug("on demand:")
			onDemandImports.foreach(x => debug("name:"+x._1+" "+x._2))
			debug("directAccess:")
			directAccess.foreach(x => debug("name:"+x._1))
			debug("//////////////////////")
			debug("")
		}
		linkAst(cu, imported, onDemandImports, directAccess)
	}
}
