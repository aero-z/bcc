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
	def resolution(cu:CompilationUnit) = cu match { //true=no problem, false=declaration collision!
	  case CompilationUnit(packageName, importDeclarations, typeDef, fileName) => { //useless matching -> how to get rid of?
	    val names = importDeclarations.map{case ClassImport(name) => name.getCanonicalName}
	    //No two classes or interfaces have the same canonical name.
	    if (names.length != names.distinct.length)
	      throw new EnvironmentException("In CompilationUnit"+fileName+": import classes and/or interfaces with the same name")
	    val scope = createInitialScope(importDeclarations) //we know that there are no duplicates present
	    typeDef.foreach{
	      case interdef @ InterfaceDefinition(_, _, _, _) => handleInterface(interdef, scope)
	      case classdef @ ClassDefinition(_, _, _, _, _, _, _) => handleClass(classdef, scope)
	    }
	    
	  }
	}
	def newScope(parent:Scope):Scope = new Scope(parent, new scala.collection.immutable.ListMap[String, Object]())
	def createInitialScope(importDeclarations:List[ImportDeclaration]):Scope = {
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
	    methods.foreach(handleMethod(_, newScope(classScope)))
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
	    handleStatements(statements, newScope(scope))
	    scope //block does not affect scope for following declarations
	  case EmptyStatement() => scope
	  case AssignmentStatment(assignment) => scope
	  case MethodInvocationStatement(invocation) => scope
	  case ClassCreationStatement(ClassCreation(constructor, parameters)) => scope //TODO: what is a class creation? definition or instantiation?
	  case ForStatement(init, condition, incrementation) => //TODO: problem here if it is a block we cannot a scope with its value returned
	    val newscope = init match {
	      case Some(statement:Statement) => handleStatement(statement, scope)
	      case None => scope
	    }
	    incrementation match {
	      //TODO: where is the for body?
	      //case Some(statement:Expression) => handleStatement(statement, newscope)
	      case _ => scope
	      case None => scope
	    }
	  case IfStatement(condition, ifStatement,elseStatement) =>
	    val newscope = scope //TODO: add new variable from condition to 
	    val notret1 = handleStatement(ifStatement, scope)
	    val notret2 = elseStatement match {
	      case Some(statement:Statement) => handleStatement(statement, scope)
	      case None => scope 
	    }
	    scope //no effect on existing scope
	  case ReturnStatement(returnExpression) => scope
	  case decl @ VariableDeclaration(typeName, identifier, initializer) => scope.declareSingle(decl)
	  case WhileStatement(condition, loop) =>
	    //TODO: can the condition contain a new varibale
	    handleStatement(loop, scope)
	    scope
	  case nil => scope
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
case class Scope(parent:Scope, envpart:Map[String, Object]) { //each scope contains only part of the environment
  //TODO: don't use Object but create a trait instead!
  //TODO: for next assignment: create a map: (String, Type) -> Decl (to allow duplicate names)
  def containedInScope(name:String):Boolean = envpart.contains(name)
  def containedInEnvironment(name:String):Boolean = 
    parent match {
      case Scope(_,_) => envpart.contains(name) || parent.containedInEnvironment(name)
      case _ => envpart.contains(name) //should be "case null"
    }
  def declareSingle(decl:Object):Scope = {
    def extract(name:String, decl:Object):Scope = {
      if (containedInScope(name))
        throw new EnvironmentException("Name already exists in scope: "+name)
      else
    	new Scope(parent, envpart + (name -> decl))
    }
    decl match {
      case x @ ClassImport(name) => extract(name.getCanonicalName, x)
      //case x @ PackageImport(name) :: Nil => extract(name.getCanonicalName, x)
      //case x @ MethodDeclaration(methodName: String, returnType: Type, modifiers: List[Modifier],parameters: List[(Type, String)], implementation: Option[Block]) => extract
      case x @ VariableDeclaration(typeName, identifier, initializer) => extract(identifier, x)
    }
  }
  def declareList(list:List[Object]):Scope = list match {
    case x :: nil => declareSingle(x)
    case x :: xs => declareSingle(x).declareList(xs)
    case nil => this //don't declare anything
  }
  
}