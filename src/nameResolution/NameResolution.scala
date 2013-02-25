import abstractSyntaxTree._

package nameResolution

object NameResolution {
	/*def resolution(cu:CompilationUnit) {
	  
	}
	
	def environmentBuilding(cu:CompilationUnit):Scope = cu match {
	  case CompilationUnit(packageName, importDeclarations, typeDef, fileName) =>  {
	    val names = typeDef.map(getname(_))
	  }
	}
	
	def notSameNameClassesAndInterfaces
	*/
	def getName(td:TypeDefinition):String = td match {
	  case InterfaceDefinition(interfaceName, _, _, _) => return interfaceName
	  case ClassDefinition(className, _, _, _, _, _, _) => return className
	  case _ => ""
	}
	
}

case class Scope(parent:Scope, declarations:List[String]) {
  def declare(name:String):Scope = new Scope(parent, name :: declarations) //append new declaration
  def alreadyDeclared(name: String):Boolean = declarations.contains(name)
  def isDefined(name:String):Boolean = parent match {
    case Scope(_, _) => declarations.contains(name) || parent.isDefined(name)
    case null => declarations.contains(name)
  }
}