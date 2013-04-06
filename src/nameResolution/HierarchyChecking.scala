package nameResolution

import ast._
import main.Logger.debug
import main._

case class HierarchyException(message: String) extends main.CompilerError(message, "Hierarchy Checking")

object HierarchyChecking {
  /*def findMethod(thisType: Type, argTypes: List[Type]) {
    thisType match {
      case RefTypeLinked(pkgName: Option[Name], className:String) => ???
      case _ => throw new HierarchyException("findMethod: thisType must be RefTypeLinked")
    }
  }*/
  
  def checkHierarchy(cus: List[CompilationUnit]) {
    
    def check(cu: CompilationUnit) {
      checkExtendsImplements(cu)
      checkAcyclic(cu)
      checkDuplicateMethods(cu)
      cu.typeDef match{
        case Some(cla : ClassDefinition) => checkClass(cla)
        case Some(in : InterfaceDefinition) => checkInterface(in)
        case _ => ()
      }
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
    
    def checkDuplicateMethods(cu: CompilationUnit) {
      val methods = cu.typeDef match {
        case Some(ClassDefinition(_,_,_,_,_,_,methods)) => methods
        case Some(InterfaceDefinition(_,_,_,methods)) => methods
        case None => Nil
      }
      if (methods.map(m => (m.methodName, m.parameters.map(p => p.paramType))).distinct.length != methods.length) throw new HierarchyException(s"HierarchyChecking: ${cu.typeName} contains twice the same signature")
    }
    
    def checkExtendsImplements(cu: CompilationUnit) {
      cu.typeDef match {
        case Some(c: ClassDefinition) =>
          shouldBeAbstract(c)
          (c.parent: @unchecked) match {
            //A class must not extend an interface
            case Some(rtl: RefTypeLinked) =>
              val typeDef = rtl.getTypeDef(cus)
              typeDef match {
                case _:InterfaceDefinition => throw new HierarchyException("class must not extend an interface")
                case ClassDefinition(_,_,_,modifiers,_,_,_) => if (modifiers.contains(Modifier.finalModifier)) throw new HierarchyException("class cannot extend final class")
              }
            case None =>
          }
          val interfaceDefs = c.interfaces.map(_ match {
            case rtl: RefTypeLinked =>
              val typeDef = rtl.getTypeDef(cus)
              if (!typeDef.isInstanceOf[InterfaceDefinition])
                throw new HierarchyException("class must not implement a class")
              typeDef
          })
          if (c.interfaces.length != c.interfaces.distinct.length)
             throw new HierarchyException("duplicate implemented interface")
          
        case Some(i: InterfaceDefinition) =>
          val parentDefs = i.parents.map(_ match {
            case rtl: RefTypeLinked =>
              val typeDef = rtl.getTypeDef(cus)
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
            classdef.getTypeDef(cus).asInstanceOf[ClassDefinition].parent match {
              case Some(parent: RefTypeLinked) =>                
                if (already contains parent) throw HierarchyException("Cycle in class hierarchy")
                else checkCycle(parent, parent :: already)
              case _ => ()
            }
          }
          checkCycle(child, Nil)
         case Some(i: InterfaceDefinition) =>
           val child = RefTypeLinked(cu.packageName, cu.typeName)
           def checkCycle(interface: RefTypeLinked, already: List[RefTypeLinked]) {
             interface.getTypeDef(cus).asInstanceOf[InterfaceDefinition].parents.foreach {
               case parent: RefTypeLinked =>
                 if (already contains parent) throw HierarchyException("Cycle in interface hierarchy")
                 else checkCycle(parent, parent :: already)
             } 
           }
           checkCycle(child, Nil)
        case _ => ()

      }

    }
    
    def checkInterface(in: InterfaceDefinition){
      checkInterfaceVsObject(in)
    }
    
    def checkInterfaceVsObject(interface: InterfaceDefinition){
      val objectMethodSig = Java.Object.getTypeDef(cus).asInstanceOf[ClassDefinition].methods.map(m => (m.methodName, m.parameters.map(_.paramType), (m.returnType, m.modifiers)))
      interface.methods.foreach{
        case MethodDeclaration(name, ret, mods, params, None, _) if(objectMethodSig.exists( x => x._1 == name && x._2 == params.map(_.paramType))) => if(! objectMethodSig.exists( x => x._1 == name && x._2 == params.map(_.paramType) && x._3._1 == ret && mods.sameElements(x._3._2))) throw HierarchyException(s"Hierarchy checking: method $name is not compatible with java.lang.Object")
        case _ => ()
      }
    }


    def checkClass(cl: ClassDefinition){
      //First get all the methods in the class definition, in concrete and abstract
      val consSig = cl.constructors.map(cons => cons.parameters.map(_.paramType))
      if(consSig.size != consSig.distinct.size) throw HierarchyException(s"${cl.className} contains twice the same constructors")
      type Sig = (String, List[Type])
      def getSig(meth: MethodDeclaration) = (meth.methodName, meth.parameters.map(_.paramType))
      def compatibleMod(over: List[Modifier.Modifier], par: List[Modifier.Modifier]) = {
        !par.contains(Modifier.finalModifier) &&
        (over.contains(Modifier.staticModifier) == par.contains(Modifier.staticModifier)) &&
        (!over.contains(Modifier.protectedModifier) || par.contains(Modifier.protectedModifier))
      }
      def merge(abAcc: List[MethodDeclaration], conAcc: List[MethodDeclaration], meth: MethodDeclaration) = {
        val over = (abAcc ::: conAcc).find(getSig(_) == getSig(meth))

        over match {
          case Some(ov) =>
            if(!compatibleMod(ov.modifiers, meth.modifiers) && (cl.methods.contains(ov) || !ov.modifiers.contains(Modifier.abstractModifier)) || ov.returnType != meth.returnType) throw HierarchyException(s"wrong override with method ${meth.methodName}")            
            else (abAcc, conAcc)
          case None => if(meth.implementation.isDefined || meth.modifiers.contains(Modifier.nativeModifier)) (abAcc, meth :: conAcc) else  (meth :: abAcc, conAcc)
        }
        
        
      }

      def getMethInClass(cl: ClassDefinition, abAcc: List[MethodDeclaration], conAcc: List[MethodDeclaration]) : (List[MethodDeclaration], List[MethodDeclaration]) = {
        val (newAb, newCon) = cl.methods.foldLeft((abAcc, conAcc)) {
          case ((abAcc, conAcc), meth) => merge(abAcc, conAcc, meth)
        }
        cl.parent match {
          case Some(refType: RefTypeLinked) => getMethInClass(refType.getTypeDef(cus).asInstanceOf[ClassDefinition], newAb, newCon)
          case _ => (newAb, newCon)
        }
      }

      def getInterfaces(typeDef: TypeDefinition): List[InterfaceDefinition] = typeDef match {
        case ClassDefinition(_, par, int, _, _, _, _) => int.map(_.asInstanceOf[RefTypeLinked].getTypeDef(cus).asInstanceOf[InterfaceDefinition]).flatMap(x => x :: getInterfaces(x)) ::: par.map(x => getInterfaces(x.asInstanceOf[RefTypeLinked].getTypeDef(cus))).getOrElse(Nil)
        case InterfaceDefinition(_, int, _, _) => int.map(_.asInstanceOf[RefTypeLinked].getTypeDef(cus).asInstanceOf[InterfaceDefinition]).flatMap(x => x :: getInterfaces(x))
      }
      val (absMeths, conMeths) = getMethInClass(cl, Nil, Nil)
      val (allAbs, allCon) = getInterfaces(cl).flatMap(_.methods).foldLeft((absMeths, conMeths)){
        case ((abAcc, conAcc), meth) => merge(abAcc, conAcc, meth)
      }
        
      if(!cl.modifiers.contains(Modifier.abstractModifier)){
        
        if(!allAbs.isEmpty) throw HierarchyException(s"${cl.className} should be abstract")


      }
    }

    // def checkClassMethod(cl : ClassDefinition){
    //   type Signature = (String, List[Type])
    //   def getSignature(meth : MethodDeclaration) = (meth.methodName, meth.parameters.map(_.paramType))
    //   def getMethodAndInterface(refType: RefTypeLinked, accMethod: List[MethodDeclaration], accInterface: List[RefTypeLinked]): List[MethodDeclaration], List[accInterface] = refType.getTypeDef(cus) match {
    //     case ClassDefinition(_, par, ins, _, _, _, meths) =>
    //       val upMeth  = meths.foldLeft(accMethod){ case (acc, method) =>
    //           acc.find(getSignature(_) == getSignature(method))) match {
    //         case Some(overMeth) =>
    //         case None => method :: acc
    //       }
    //   }

    // }
    


    cus.filter(_.typeDef.isDefined).foreach(check(_))
    //cus.foreach(checkAcyclic(_))
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

