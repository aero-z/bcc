package nameResolution

import ast._
import main.Logger.debug

class HierarchyException(message:String) extends main.CompilerError(message)

object HierarchyChecking {
	def checkHierarchy(cus:List[CompilationUnit]) {
		def getCu(rtl:RefTypeLinked):CompilationUnit = {
		  cus.find(x => x.packageName.equals(rtl.pkgName) && x.typeName == rtl.className) match {
		    case Some(cu) => cu
		    case None => throw new HierarchyException("The TypeLinking should have found that this class cannot be found:"+rtl)
		  }
		}
		def isClass(cu:CompilationUnit):Boolean = cu match {
		  case CompilationUnit(_, _, Some(c:ClassDefinition), _) => true
		  case _ => false
		}
		def check(cu:CompilationUnit) {
			cu.typeDef match {
			  case Some(c:ClassDefinition) =>
			    c.parent match {
				  	//A class must not extend an interface
			    	case Some(rtl:RefTypeLinked) =>
			    	  if(!rtl.getType(cus).isInstanceOf[ClassDefinition])
			    	    throw new HierarchyException("The TypeLinking should have found that this class cannot be found:"+rtl)
			    	case None =>
			  	}
			    //c.interfaces.foreach(x => x.)
			  case Some(i:InterfaceDefinition) => 
			  case _ => //nothing to do?true
			}
		}
		cus.foreach(check(_))
		/*
		cu.typeDef.map(_ match {
			case in:InterfaceDefinition => in.parents //(interfaceName: String, parents: List[RefType],modifiers: List[Modifier], methods: List[MethodDeclaration])
			case cl:ClassDefinition => cl.parent.map{case }
		})*/
	}
	
/*
. (JLS 8.1.3, dOvs simple constraint 1)
A class must not implement a class. (JLS 8.1.4, dOvs simple constraint 2)
An interface must not be repeated in an implements clause, or in an extends clause of an interface. (JLS 8.1.4, dOvs simple constraint 3)
A class must not extend a final class. (JLS 8.1.1.2, 8.1.3, dOvs simple constraint 4)
An interface must not extend a class. (JLS 9.1.2)
The hierarchy must be acyclic. (JLS 8.1.3, 9.1.2, dOvs well-formedness constraint 1)
A class or interface must not declare two methods with the same signature (name and parameter types). (JLS 8.4, 9.4, dOvs well-formedness constraint 2)
A class must not declare two constructors with the same parameter types (dOvs 8.8.2, simple constraint 5)
A class or interface must not contain (declare or inherit) two methods with the same signature but different return types (JLS 8.1.1.1, 8.4, 8.4.2, 8.4.6.3, 8.4.6.4, 9.2, 9.4.1, dOvs well-formedness constraint 3)
A class that contains (declares or inherits) any abstract methods must be abstract. (JLS 8.1.1.1, well-formedness constraint 4)
A nonstatic method must not replace a static method (JLS 8.4.6.1, dOvs well-formedness constraint 5)
A method must not replace a method with a different return type. (JLS 8.1.1.1, 8.4, 8.4.2, 8.4.6.3, 8.4.6.4, 9.2, 9.4.1, dOvs well-formedness constraint 6)
A protected method must not replace a public method. (JLS 8.4.6.3, dOvs well-formedness constraint 7)
A method must not replace a final method. (JLS 8.4.3.3, dOvs well-formedness constraint 9)
*/ 
}
