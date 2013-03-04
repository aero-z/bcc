package abstractSyntaxTree

import abstractSyntaxTree.Modifier._
import abstractSyntaxTree.Operator._
import scanner.IntegerToken
import scala.Enumeration
import main.Logger
import scala.language.implicitConversions
import java.io.File

sealed abstract class CheckResult(passed: Boolean, errStr: String) {
  def ++(c: CheckResult) = {
    if (passed) c
    else this
  }
}

object CheckResult {
  def apply(passed: Boolean, errStr: String): CheckResult =
    if (passed) CheckOk()
    else CheckFail(errStr)
}

case class CheckFail(errStr: String) extends CheckResult(false, errStr)
case class CheckOk() extends CheckResult(true, null)

trait AstNode {
  val children: List[AstNode]
  def check: CheckResult = CheckOk()
  
  implicit def option2List[A](o: Option[A]) = o match {
    case Some(x) => x :: Nil
    case None => Nil
  }
  
  def mergeCheck(x: (Boolean, String), y: (Boolean, String)) = {
    if (x._1) x
    else y
  }
  
  def checkDuplicateModifiers(modifiers: List[Modifier]) = 
    CheckResult(modifiers.distinct.length == modifiers.length, "duplicate modifier")
  
  def checkPublicProtectedModifier(modifiers: List[Modifier]) = 
    CheckResult(modifiers.contains(Modifier.publicModifier) || modifiers.contains(Modifier.protectedModifier), "public/protected modifier missing")
}
trait VariableDeclaration

//Will be used quite often, is for instance "java.util.String"
case class Name(path: List[String]) extends Expression {
  override def toString = path.reduce((x, y) => x + "." + y)
  def getCanonicalName():String = path.last
  def appendClassName(name:Name) = Name(path ::: name.path)
  val children = Nil
}

//Main  of a file.
case class CompilationUnit(packageName: Option[Name], importDeclarations: List[ImportDeclaration], typeDef: Option[TypeDefinition], fileName: String) extends AstNode {
  def display(): Unit = {
    Logger.debug("*" * 20)
    Logger.debug("Compilation Unit")
    Logger.debug("*" * 20)
    Logger.debug(s"File Name: $fileName")
    Logger.debug(s"Package defined: ${packageName.isDefined}")
    for (x <- packageName) Logger.debug(s"Package name: ${packageName.toString}")
    Logger.debug(s"Number of import: ${importDeclarations.size}")
    for (ClassImport(name) <- importDeclarations) Logger.debug(s"Import class: ${name.toString}")
    for (PackageImport(name) <- importDeclarations) Logger.debug(s"Import package: ${name.toString}")
    Logger.debug("*" * 20)
    Logger.debug("")
    for (typeDefinition <- typeDef) typeDefinition.display
  }
  val children = packageName ::: importDeclarations ::: typeDef
  override val check = typeDef match {
    case None => CheckOk()
    case Some(x) => CheckResult(x.getName()+".java" == new File(fileName).getName(), "file name and class name don't match")
  }
}

abstract class ImportDeclaration(name: Name) extends AstNode {
  def getName():Name = name
  val children = Nil
}

case class ClassImport(name: Name) extends ImportDeclaration(name)
case class PackageImport(name: Name) extends ImportDeclaration(name)

//Either a class or an interface
sealed abstract class TypeDefinition(typeName: String, modifiers: List[Modifier]) extends AstNode with NotNull {
def getName():String = typeName
  def display: Unit
  
  override def check = checkDuplicateModifiers(modifiers) ++
                       CheckResult(!(modifiers.contains(Modifier.abstractModifier) && modifiers.contains(Modifier.finalModifier))
            , "class can't be abstract and final at the same time") ++
                       CheckResult(modifiers.contains(Modifier.publicModifier), "type definition must be private")
}

case class InterfaceDefinition(interfaceName: String, parents: List[RefType],
  modifiers: List[Modifier], methods: List[MethodDeclaration]) extends TypeDefinition(interfaceName, modifiers) {
  def display: Unit = {
    Logger.debug("*" * 20)
    Logger.debug("Interface declaration")
    Logger.debug("*" * 20)
    Logger.debug(s"Interface name: $interfaceName")
    Logger.debug(s"Number of parents: ${parents.size}")
    for (x:RefType <- parents) Logger.debug(s"Interface extending: ${x.toString}")
    for (x <- modifiers) Logger.debug(s"Modifier: ${x.toString()}")
    Logger.debug(s"Number of methods: ${methods.size}")
    Logger.debug("*" * 20)
    Logger.debug("")
    for (meth <- methods) meth.display
  }
  val children = parents ::: methods
  override val check = super.check ++ methods.foldLeft(CheckOk(): CheckResult)((cr, m) => m match {
             case MethodDeclaration(_,_,_,_,Some(x)) => CheckFail("an interface method cannot have a body")
             case MethodDeclaration(_,_,modifiers,_,_) => CheckResult(!modifiers.contains(Modifier.staticModifier) && !modifiers.contains(Modifier.finalModifier) && !modifiers.contains(Modifier.nativeModifier), "an interface method cannot be static, final, or native")
             case _ => CheckOk()
           })
}

case class ClassDefinition(className: String, parent: Option[RefType], interfaces: List[RefType], modifiers: List[Modifier], fields: List[FieldDeclaration], constructors: List[ConstructorDeclaration], methods: List[MethodDeclaration]) extends TypeDefinition(className, modifiers) {
  def display: Unit = {
    Logger.debug("*" * 20)
    Logger.debug("Class declaration")
    Logger.debug("*" * 20)
    Logger.debug(s"Class name: $className")
    Logger.debug(s"Has parent: ${parent.isDefined}")
    for (x:RefType <- parent) Logger.debug(s"Class extending: ${x.toString}")
    Logger.debug(s"Number of implemented interfaces: ${interfaces.size}")
    for (x:RefType <- interfaces) Logger.debug(s"Interface implemented: ${x.toString}")
    for (x <- modifiers) Logger.debug(s"Modifier: ${Modifier.fromModifier(x)}")
    Logger.debug(s"Number of fields: ${fields.size}")
    Logger.debug(s"Number of constructors: ${constructors.size}")
    Logger.debug(s"Number of methods: ${methods.size}")
    Logger.debug("*" * 20)
    Logger.debug("")
    for (field <- fields) field.display
    for (constructor <- constructors) constructor.display
    for (method <- methods) method.display
  }
  val children = parent ::: interfaces ::: fields ::: constructors ::: methods
}


//What can be put in a class
case class MethodDeclaration(methodName: String, returnType: Type, modifiers: List[Modifier],
  parameters: List[Parameter], implementation: Option[Block]) extends AstNode with VariableDeclaration {
  def display: Unit = {
    Logger.debug("*" * 20)
    Logger.debug("Method declaration")
    Logger.debug("*" * 20)
    Logger.debug(s"Method name: $methodName")
    Logger.debug(s"Return type: ${returnType.typeName}")
    Logger.debug(s"Number of modifiers: ${modifiers.size}")
    for(x <- modifiers) Logger.debug(s"Modifier: ${Modifier.fromModifier(x)}")
    Logger.debug(s"Number of parameters: ${parameters.size}")
    for(Parameter(x, y) <- parameters) Logger.debug(s"Parameter type : ${x.typeName}\n          name: $y")
    Logger.debug(s"Is defined: ${implementation.isDefined}")
    Logger.debug("*" * 20)
    Logger.debug("")
    //TODO something about the implementation
  }
  val children = returnType :: parameters ::: implementation
  override val check = checkDuplicateModifiers(modifiers) ++
                       checkPublicProtectedModifier(modifiers) ++
                       CheckResult(!modifiers.contains(Modifier.staticModifier) || !modifiers.contains(Modifier.finalModifier),
                                   "a static method cannot be final") ++
                       CheckResult(!modifiers.contains(Modifier.nativeModifier) || modifiers.contains(Modifier.staticModifier),
                                   "a native method must be static") ++
                       CheckResult(!modifiers.contains(Modifier.abstractModifier) || (!modifiers.contains(Modifier.staticModifier) && !modifiers.contains(Modifier.finalModifier)),
                                   "an abstract method cannot be static or final") ++
                       CheckResult(implementation.isDefined == (!modifiers.contains(Modifier.abstractModifier) && !modifiers.contains(Modifier.nativeModifier)),
                                   "method must have a body if and only if it is neither abstract nor native")
}

case class FieldDeclaration(fieldName: String, fieldType: Type, modifiers: List[Modifier],
  initializer: Option[Expression]) extends AstNode with VariableDeclaration {
  def display: Unit = {
    Logger.debug("*" * 20)
    Logger.debug("Field declaration")
    Logger.debug("*" * 20)
    Logger.debug(s"Field name: $fieldName")
    Logger.debug(s"Type: ${fieldType.typeName}")
    Logger.debug(s"Number of modifiers: ${modifiers.size}")
    for(x <- modifiers) Logger.debug(s"Modifier: ${Modifier.fromModifier(x)}")
    Logger.debug(s"Field initializer: ${initializer.isDefined}")
    Logger.debug("*" * 20)
    Logger.debug("")
    //TODO something about the initializer
  }
  val children = fieldType :: initializer
  override val check = checkDuplicateModifiers(modifiers) ++
                       checkPublicProtectedModifier(modifiers) ++
                       CheckResult(!modifiers.contains(Modifier.finalModifier), "no field can be final")
}

case class ConstructorDeclaration(modifiers: List[Modifier], parameters: List[Parameter], implementation: Block) extends AstNode with VariableDeclaration {
  def display: Unit = {
    Logger.debug("*" * 20)
    Logger.debug("Constructor declaration")
    Logger.debug("*" * 20)
    Logger.debug(s"Number of modifiers: ${modifiers.size}")
    for(x <- modifiers) Logger.debug(s"Modifier: ${Modifier.fromModifier(x)}")
    Logger.debug(s"Number of parameters: ${parameters.size}")
    for(Parameter(x,y) <- parameters) Logger.debug(s"Parameter type : ${x.typeName}\n            name: $y")
    Logger.debug("*" * 20)
    Logger.debug("")
    //TODO something fancy about the implementation
  }
  val children = implementation :: parameters 
  override val check = checkDuplicateModifiers(modifiers)
}

case class Parameter(paramType: Type, id:String) extends AstNode with VariableDeclaration {
  val children = Nil
}
