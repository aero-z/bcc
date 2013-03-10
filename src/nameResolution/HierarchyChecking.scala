package nameResolution

import ast._
import main.Logger.debug
import main._

class HierarchyException(message: String) extends main.CompilerError(message)

object HierarchyChecking {
  def checkHierarchy(cus: List[CompilationUnit]) {
    
    def check(cu: CompilationUnit) {
      //checkAcyclic(cu)
      checkExtendsImplements(cu)
    }
    
    def getCu(rtl: RefTypeLinked): CompilationUnit = {
      cus.find(x => x.packageName.equals(rtl.pkgName) && x.typeName == rtl.className) match {
        case Some(cu) => cu
        case None => throw new HierarchyException("The TypeLinking should have found that this class cannot be found:" + rtl)
      }
    }
    def isClass(cu: CompilationUnit): Boolean = cu match {
      case CompilationUnit(_, _, Some(c: ClassDefinition), _) => true
      case _ => false
    }
    def shouldBeAbstract(cu: ClassDefinition) {
      if (!cu.modifiers.contains(Modifier.abstractModifier))
        cu.methods.foreach(x => if (x.modifiers.contains(Modifier.abstractModifier)) throw new HierarchyException("Class with abstract methods is not abstract"))
    }
    
    def checkExtendsImplements(cu: CompilationUnit) {
      cu.typeDef match {
        case Some(c: ClassDefinition) =>
          shouldBeAbstract(c)
          (c.parent: @unchecked) match {
            //A class must not extend an interface
            case Some(rtl: RefTypeLinked) =>
              val typeDef = rtl.getType(cus)
              typeDef match {
                case _:InterfaceDefinition => throw new HierarchyException("class must not extend an interface")
                case ClassDefinition(_,_,_,modifiers,_,_,_) => if (modifiers.contains(Modifier.finalModifier)) throw new HierarchyException("class cannot extend final class")
              }
            case None =>
          }
          val interfaceDefs = c.interfaces.map(_ match {
            case rtl: RefTypeLinked =>
              val typeDef = rtl.getType(cus)
              if (!typeDef.isInstanceOf[InterfaceDefinition])
                throw new HierarchyException("class must not implement a class")
              typeDef
          })
          if (interfaceDefs.length != interfaceDefs.distinct.length)
            throw new HierarchyException("duplicate implemented interface")

        case Some(i: InterfaceDefinition) =>
          val parentDefs = i.parents.map(_ match {
            case rtl: RefTypeLinked =>
              val typeDef = rtl.getType(cus)
              if (!typeDef.isInstanceOf[InterfaceDefinition])
                throw new HierarchyException("interface must not extend a class")
              typeDef
          })
          if (parentDefs.length != parentDefs.distinct.length)
            throw new HierarchyException("duplicate extended interface")
        case _ => //nothing to do?true
      }
    }

    //We can easily prove that there is no cycle mixed class-interface
    def checkAcyclic(cu: CompilationUnit) {

      cu.typeDef match {
        case Some(c: ClassDefinition) =>
          val child = RefTypeLinked(cu.packageName, cu.typeName)          
          def checkCycle(classdef: RefTypeLinked, already: List[RefTypeLinked]) {
            classdef.getType(cus).asInstanceOf[ClassDefinition].parent match {
              case Some(parent: RefTypeLinked) =>                
                if (already contains parent) throw CompilerError("Cycle in class hierarchy")
                else checkCycle(parent, parent :: already)
              case _ => ()
            }
          }
          checkCycle(child, Nil)
         case Some(i: InterfaceDefinition) =>
           val child = RefTypeLinked(cu.packageName, cu.typeName)
           def checkCycle(interface: RefTypeLinked, already: List[RefTypeLinked]) {
             interface.getType(cus).asInstanceOf[InterfaceDefinition].parents.foreach {
               case parent: RefTypeLinked =>
                 if (already contains parent) throw CompilerError("Cycle in interface hierarchy")
                 else checkCycle(parent, parent :: already)
             } 
           }
           checkCycle(child, Nil)
        case _ => ()

      }

    }
    cus.filter(_.typeDef.isDefined).foreach(check(_))
    cus.foreach(checkAcyclic(_))
    /*
     cu.typeDef.map(_ match {
     case in:InterfaceDefinition => in.parents //(interfaceName: String, parents: List[RefType],modifiers: List[Modifier], methods: List[MethodDeclaration])
     case cl:ClassDefinition => cl.parent.map{case }
     })*/
  }

  /*
   * //A class must not extend an interface
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

