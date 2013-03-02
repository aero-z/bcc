package nameResolution

import abstractSyntaxTree._
import parser._

/*
 * Asst2: packages and types=classes, interfaces
 * 1. build class environment
 * 2. resolve type names ()
 * 3. check class hierarchy (no cycles)
 * Asst3: methods and expressions (variables, fields, parameters)
 * 4. disambgue ambiguous names
 * 5.resolve expressions: varibales, static fields
 * 6. type checking
 * 7. resolve methods and instance fields
 */

class EnvironmentException(message:String) extends Exception

object NameResolution {
	def resolution(cu:CompilationUnit, possibleImports:List[(String, String, TypeDefinition)]) = cu match { //true=no problem, false=declaration collision!
	  case CompilationUnit(packageName, importDeclarations, typeDef, fileName) => { //useless matching -> how to get rid of?
		//TODO: add other classes from package to importdeclarations
	    /*
	    val currentPackage = possibleImports.find(_._1.equals(packageName))
	    val decls = currentPackage match {
	      case Some((_,_,decl)) => decl :: importDeclarations
	      case None => importDeclarations
	    }*/
	    val scope = createInitialScope(importDeclarations, possibleImports) //we know that there are no duplicates present
	    typeDef match {
	      case Some(interdef @ InterfaceDefinition(_, _, _, _)) => handleInterface(interdef, scope)
	      case Some(classdef @ ClassDefinition(_, _, _, _, _, _, _)) => handleClass(classdef, scope)
	    }
	    
	  }
	}
	//creates a reference scope which contains all class and interfaces which CAN be imported...
	def possibleImports(cuList:List[CompilationUnit]):List[(String, String, TypeDefinition)] = {
	  val names:List[String] = cuList.flatMap{case CompilationUnit(packageName, importDeclarations, typeDef, fileName) => typeDef match { case Some(typedef) => List(typedef.getName) case None => Nil}}
	  if (names.length != names.distinct.length)
	    throw new EnvironmentException("Some classes or Interfaces have the same name")
	  cuList.flatMap{ //map().filter(_ != nil) would also work
	    case CompilationUnit(packageName, importDeclarations, typeDef, fileName) =>
	    	typeDef match {
	    	  case Some(newdef) => List((packageName match {case Some(name) => name.getCanonicalName() case None => ""}, newdef.getName, newdef))
	    	  case None => Nil
	    	}
	  }
	}
	def newScope(parent:Option[Scope]):Scope = new Scope(parent, new scala.collection.immutable.ListMap[String, Object]())
	def createInitialScope(importDeclarations:List[ImportDeclaration], imports:List[(String, String, TypeDefinition)]):Scope = {
		def createRec(scope:Scope, importDeclarations:List[ImportDeclaration]):Scope = importDeclarations match {
		  case ClassImport(name) :: tail =>
		    imports.find(_._2.equals(name.getCanonicalName)) match { //TODO: do we need to check if it is really a class?
		      case Some(classimp) => createRec(scope.declareSingle(classimp), tail)
		      case None => throw new EnvironmentException("This class cannot be imported: "+name.getCanonicalName)
		    }
		  case PackageImport(name) :: tail =>
		    imports.filter(_._1.equals(name.getCanonicalName)) match {
		      case nil => throw new EnvironmentException("This package cannot be imported: "+name.getCanonicalName) //TODO: can an empty package be imported?
		      case list => scope.declareList(list.map(_._3))
		    }
		  case nil => scope //nothing to be added
		}
		createRec(newScope(None), importDeclarations)
		val importmap:Map[String, Object] = importDeclarations
	    	.filter{case ClassImport(_) => true} //not include package imports
	    	.map{case classi @ ClassImport(name) => (name.getCanonicalName, classi)}//string point to their "declaration"
	    	.toMap 
	    //TODO: treat package imports!
	    newScope(null)
	}
	def handleInterface(interfacedef:InterfaceDefinition, scope:Scope) {
	  //TODO: handle interfaces
	}
	def handleClass(classdef:ClassDefinition, scope:Scope) = classdef match {
	  case ClassDefinition(className, parent, interfaces, modifiers, fields, constructors, methods) =>
	    val fieldDecl = fields.map{case FieldDeclaration(name, _, _, _) => name}
	    // No two fields declared in the same class may have the same name
	    if (fieldDecl.length != fieldDecl.distinct.length)
	      throw new EnvironmentException("In class"+className+": field declared with the same name!")
	    //TODO: add className to environment/namespace?? => correct?
	    val classScope:Scope = scope.declareSingle(className, classdef)
	    methods.foreach(handleMethod(_, newScope(Some(classScope))))
	}
	def handleMethod(method:MethodDeclaration, scope:Scope) = method match {
	  //TODO: add name to environment/namespace?
	  case MethodDeclaration(methodName, returnType, modifiers,parameters, implementation)
	  	=> implementation match {
	  	  case Some(block @ Block(list)) => handleStatements(block :: Nil, scope)
	  	  case None => //just for completeness
	  	}
	}
	def handleStatement(statement:Statement, scope:Scope):Scope = statement match {
	  case Block(statements) =>
	    handleStatements(statements, newScope(Some(scope)))
	    scope //block does not affect scope for following declarations
	  case ForStatement(Some(init), condition, incrementation, loop) => //TODO: problem here if it is a block we cannot a scope with its value returned
	    handleStatement(loop, handleStatement(init, scope)); scope
	  case IfStatement(condition, ifStatement,Some(elseStatement)) =>
	    val notret1 = handleStatement(ifStatement, scope)
	    val notret2 = handleStatement(elseStatement, scope)
	    scope //no effect on existing scope
	  case IfStatement(condition, ifStatement,None) =>
	    val notret1 = handleStatement(ifStatement, scope)
	    scope
	  case decl @ VariableDeclaration(typeName, identifier, initializer) => scope.declareSingle(decl)
	  case WhileStatement(condition, loop) =>
	    val noret = handleStatement(loop, scope)
	    scope
	  case _ => scope
	}
	def handleStatements(statements:List[Statement], scope:Scope):Scope = statements match{
	  case nil => scope
	  case x :: nil => handleStatement(x, scope)
	  case x :: xs => handleStatements(xs, handleStatement(x, scope))
	}
	
	/*def environmentBuilding(cu:CompilationUnit):Scope = cu match {
	  case CompilationUnit(packageName, importDeclarations, typeDef, fileName) =>  {
	    val names = typeDef.map(getname(_))
	  }
	}
	
	def notSameNameClassesAndInterfaces
	
  
	def getName(td:TypeDefinition):String = td match {
 	  case ClassDefinition(className, _, _, _, _, _, _) => return className
	  case _ => ""
	}*/
	
}

//a Scope is a single linked tree so in each scope you can just see a list of encapsulated Scopes
//TODO: a scope should know it's corresponding ST node (class/block/whatever)
//TODO: don't use object but extend existing classes
case class Scope(parent:Option[Scope], envpart:Map[String, Object]) { //each scope contains only part of the environment
  //TODO: don't use Object but create a trait instead!
  //TODO: for next assignment: create a map: (String, Type) -> Decl (to allow duplicate names)
  def containedInScope(name:String):Boolean = envpart.contains(name)
  def containedInEnvironment(name:String):Boolean = 
    parent match {
      case Some(parent @ Scope(_,_)) => envpart.contains(name) || parent.containedInEnvironment(name)
      case None => envpart.contains(name)
    }
  def declareSingle(decl:Object):Scope = {
    def extract(name:String, decl:Object):Scope = {
      if (containedInScope(name))
        throw new EnvironmentException("Name already exists in scope: "+name)
      else
    	Scope(parent, envpart + (name -> decl))
    }
    decl match {
      case x @ ClassImport(name) => extract(name.getCanonicalName, x)
      //case x @ PackageImport(name) :: Nil => extract(name.getCanonicalName, x)
      //case x @ MethodDeclaration(methodName: String, returnType: Type, modifiers: List[Modifier],parameters: List[(Type, String)], implementation: Option[Block]) => extract
      case x @ VariableDeclaration(typeName, identifier, initializer) => extract(identifier, x)
    }
  }
  def declareList(list:List[Object]):Scope = list match { //for parameters and field declarations
    case x :: nil => declareSingle(x)
    case x :: xs => (declareSingle(x).declareList(xs))
    case nil => this //don't declare anything
  }
  
}