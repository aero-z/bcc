package nameResolution


import ast._
import main.CompilerError
import main.Logger.debug

case class NameLinkingException(mess: String) extends CompilerError(mess, "Name Linking")

object VarResolver{
  
  case class Environment(symbolMap: Map[String, (Type, PathToDeclaration)], currentPosition: PathLocal, pck: Option[Name], classDef: ClassDefinition){
    def open : Environment = this match {
      case Environment(sm, PathLocal(list), cpck,  cd) =>Environment(sm, PathLocal(0 :: list), cpck, cd)
    }
    def inc : Environment = this match {
      case Environment(sm, PathLocal(list), cpck, cd) =>Environment(sm, PathLocal(list.head + 1 :: list.tail), cpck, cd)
    }
    def update(stmt: Statement) : Environment = stmt match {      
      case decl: LocalVariableDeclaration => if(symbolMap contains decl.identifier) throw NameLinkingException(s"${decl.identifier} is already define") else Environment(symbolMap + (decl.identifier -> (decl.typeName, currentPosition)), currentPosition, pck, classDef).inc
      case _ => inc
    }
  }

  def getLocalPath(stmt: Statement, acc: List[PathLocal], curPath: List[Int]): List[PathLocal] = stmt match{
    case Block(stmts) =>  stmts.foldLeft((acc, 0 :: curPath)){ case ((acc, pos), stmt) => (getLocalPath(stmt, acc, pos), pos.head + 1 :: pos.tail)}._1
    case ForStatement(Some(_ :  LocalVariableDeclaration), _, _, loop) => getLocalPath(loop, PathLocal(0 :: curPath) :: acc, 1 :: curPath)
    case ForStatement(_, _, _, loop) => getLocalPath(loop, acc, 1 :: curPath)
    case IfStatement(_, ifStmt, Some(elseStmt)) => getLocalPath(elseStmt, getLocalPath(ifStmt, acc, 0 :: curPath), 1 :: curPath)
    case _: LocalVariableDeclaration => PathLocal(curPath) :: acc
    case WhileStatement(_, loop) => getLocalPath(loop, acc, 0 :: curPath)
    case _ => acc
  }

  def variableLink(cus: List[CompilationUnit]): List[CompilationUnit] = {


    def checkFields(fields: List[FieldDeclaration]): Unit = {
      val fieldName = fields.map(_.fieldName)
      if(fieldName.distinct.size != fieldName.size) throw NameLinkingException("There is two fields with the same name")
    }
    

    def linkFields(imp: List[ImportDeclaration], classDef: ClassDefinition, pck : Option[Name]): List[FieldDeclaration] = {
      def isStatic(field: FieldDeclaration) = field.modifiers.contains(Modifier.staticModifier)
      def remove(children: List[PathField], parents: List[PathField]) = parents.filter(p => children.forall(_.fieldName != p.fieldName) && classDef.fields.forall(p.fieldName != _.fieldName))

      val (parStatic, parNonStat): (List[PathField] ,List[PathField]) = {
        def rec(refType: RefTypeLinked, staticAcc: List[PathField], acc: List[PathField]): (List[PathField], List[PathField]) = {
          val classDef = refType.getTypeDef(cus).asInstanceOf[ClassDefinition]

          val par = classDef.fields.partition(isStatic(_))
          val (newStat, newNon) = (par._1.map(fi => PathField(refType, fi.fieldName)), par._2.map(fi => PathField(refType, fi.fieldName)))          
          classDef.parent match {
            case Some(ref : RefTypeLinked) => rec(ref, newStat ::: staticAcc, newStat ::: newNon ::: acc)
            case  _ => (remove(acc, newStat) ::: staticAcc, remove(acc, newNon ::: newStat) ::: acc)
          }
        }
        classDef.parent match {
          case Some(ref: RefTypeLinked) => rec(ref, Nil, Nil)
          case _ => (Nil, Nil)
        }
      }

      checkFields(classDef.fields)
      val currPath = classDef.fields.partition(isStatic(_))
      val (statPresentPath, nonPresentField) = (currPath._1.map(fi => PathField(RefTypeLinked(pck, classDef.className), fi.fieldName)), currPath._2.map(fi => PathField(RefTypeLinked(pck, classDef.className), fi.fieldName)))
      def linkFieldAssignment(previousField: List[FieldDeclaration], previousPath: List[PathField], previousStatPath: List[PathField], field : FieldDeclaration) = try{
        val currentPath = PathField(RefTypeLinked(pck, classDef.className), field.fieldName)
        (FieldDeclaration(field.fieldName, field.fieldType, field.modifiers,
          field.initializer.map(linkAssignment(_)(if(isStatic(field)) previousStatPath else previousPath, 
            if(isStatic(field)) statPresentPath else statPresentPath ::: nonPresentField))) :: previousField, 
          currentPath :: previousPath,
        if(isStatic(field)) currentPath :: previousStatPath else previousStatPath)
      }catch{
        case FieldAccessIsProbablyPckException(path) => throw NameLinkingException(s"Could not find: ${path.reduce(_ + "." + _)}")
      }
      
      def linkAssignment(exp: Expression)(possibleDecl: List[PathField], currentDecl: List[PathField]) : Expression = try {
        exp match {
          case ParenthesizedExpression(exp) => ParenthesizedExpression(linkAssignment(exp)(possibleDecl, currentDecl))
          case UnaryOperation(op, exp) => UnaryOperation(op, linkAssignment(exp)(possibleDecl, currentDecl))
          case BinaryOperation(f, op, s) => BinaryOperation(linkAssignment(f)(possibleDecl, currentDecl), op, linkAssignment(s)(possibleDecl, currentDecl))
          case CastExpression(cast, t) => CastExpression(cast, linkAssignment(t)(possibleDecl, currentDecl))
          case ArrayAccess(arr, ind) => ArrayAccess(linkAssignment(arr)(possibleDecl, currentDecl), linkAssignment(ind)(possibleDecl, currentDecl))
          case ArrayCreation(typeName, size) => ArrayCreation(typeName, linkAssignment(size)(possibleDecl, currentDecl))
          case Assignment(lhs, rhs) => Assignment(
            linkAssignment(lhs)(currentDecl ::: possibleDecl, currentDecl) match{ case x: LeftHandSide => x
              case _ => throw NameLinkingException("Trying to assign to a type.")
            }, linkAssignment(rhs)(possibleDecl, currentDecl))
          case FieldAccess(acc, field) => FieldAccess(linkAssignment(acc)(possibleDecl, currentDecl), field)
          case ClassCreation(cons, args) => ClassCreation(cons, args.map(linkAssignment(_)(possibleDecl, currentDecl)))
          case ExprMethodInvocation(acc, meth, args) => ExprMethodInvocation(linkAssignment(acc)(possibleDecl, currentDecl), meth, args.map(linkAssignment(_)(possibleDecl, currentDecl)))
          case ThisMethodInvocation(thisType, meth, args) => ThisMethodInvocation(thisType, meth, args.map(linkAssignment(_)(possibleDecl, currentDecl)))
          case InstanceOfCall(exp, check) => InstanceOfCall(linkAssignment(exp)(possibleDecl, currentDecl), check)          
          case VariableAccess(name) => linkVar(name, possibleDecl)
          case lit: Literal => lit
          case x : This => x
        }
      }catch {
        case FieldAccessIsProbablyPckException(path) => exp match {
          case FieldAccess(_, className) =>
            cus.find(cu => cu.packageName == Some(Name(path)) && cu.typeName == className).map(_ => RefTypeLinked(Some(Name(path)), className)).getOrElse(throw FieldAccessIsProbablyPckException(path :+ className))
          case _ : VariableAccess => throw FieldAccessIsProbablyPckException(path)
          case _ => throw new NameLinkingException(s"Could not find: ${path.reduce(_ + "." + _)}")
        }
      }
      
      def linkVar(name: String, previousField: List[PathField]) : LinkedExpression = {
        previousField.find(_.fieldName == name) match{
          case Some(path) => LinkedVariableOrField(name, path.refType.getTypeDef(cus).asInstanceOf[ClassDefinition].fields.find(_.fieldName == name).get.fieldType, path)
          case None => if((statPresentPath ::: nonPresentField).exists(_.fieldName == name)) throw NameLinkingException("Forward reference") else  imp.collectFirst{
            case LinkImport(typeName, refType) if name == typeName => refType
          }.getOrElse(throw FieldAccessIsProbablyPckException(List(name)))
        }
      }
          

      classDef.fields.foldLeft((List[FieldDeclaration](), parStatic, parNonStat ::: parStatic)){
        case ((decl, path, nonPath), fi) => linkFieldAssignment(decl, path, nonPath, fi)
      }._1.reverse
    
    }




    

    def linkConstructor(cons: ConstructorDeclaration, pck: Option[Name], classDef: ClassDefinition): ConstructorDeclaration = {      
      checkParameters(cons.parameters)
      val parameterMap = Map( cons.parameters.map{case Parameter(parType, id) => (id, ( parType, PathPar(id)))}:_*)
      val curPos = PathLocal(List(0))
      
      val env = Environment(parameterMap, curPos, pck, classDef)
      ConstructorDeclaration(cons.name, cons.modifiers, cons.parameters, implementationLink(env, cons.implementation), getLocalPath(cons.implementation, Nil, Nil))//TODO get the list of path
    }


    def linkMethod(meth: MethodDeclaration, pck: Option[Name], classDef: ClassDefinition) : MethodDeclaration = {      
      checkParameters(meth.parameters)
      val parameterMap = Map(meth.parameters.map{case Parameter(parType, id) => (id, (parType, PathPar(id)))}:_*)
      val curPos = PathLocal(List(0))
      val env = Environment(parameterMap, curPos, pck, classDef)
      MethodDeclaration(meth.methodName, meth.returnType, meth.modifiers, meth.parameters, meth.implementation.map(implementationLink(env, _)), meth.implementation.toList.flatMap(getLocalPath(_, Nil, Nil)))
    }
    
    def implementationLink(environment: Environment, block: Block): Block = Block(passThroughStatements(block.statements, environment))




    def checkParameters(param: List[Parameter]){
      val parameterName = param.map(_.id)
      if(parameterName.size != parameterName.distinct.size) throw NameLinkingException("A method have twice the same parameter")
    }


    def passThroughStatements(stmts: List[Statement], env: Environment): List[Statement] = stmts match {
      case x :: xs => linkStatement(x, env) :: passThroughStatements(xs, env.update(x))
      case Nil => Nil
    }

    def linkStatement(stmt: Statement, env: Environment) : Statement = try {stmt match {
      case Block(stmts) => Block(passThroughStatements(stmts, env.open))
      case EmptyStatement => EmptyStatement
      case ExpressionStatement(exp) => ExpressionStatement(linkExpression(exp)(env, ""))
      case ForStatement(init, cond, inc, stmt) => val forEnv = init.foldLeft(env.open)(_.update(_));
        ForStatement(init.map(linkStatement(_, env)), cond.map(linkExpression(_)(forEnv, "")), inc.map(linkExpression(_)(forEnv, "")), linkStatement(stmt, forEnv))
      case IfStatement(cond, ifStmt, elseStmt) => IfStatement(linkExpression(cond)(env, ""), linkStatement(ifStmt, env.open), elseStmt.map(linkStatement(_, env.open.inc)))
      case ReturnStatement(exp) => ReturnStatement(exp.map(linkExpression(_)(env, "")))
      case LocalVariableDeclaration(typeName, id, Some(init)) => LocalVariableDeclaration(typeName, id, Some(linkExpression(init)(env.update(stmt), id)))
      case LocalVariableDeclaration(typeName, id, None) => throw NameLinkingException(s"The local variable $id is not initialized")
      case WhileStatement(cond, loop) => WhileStatement(linkExpression(cond)(env, ""), linkStatement(loop, env.open))
    }}catch { case FieldAccessIsProbablyPckException(path) => throw NameLinkingException(s"Can not find ${path.reduce(_ + "." + _)}")
    }

    def linkExpression(exp: Expression)(implicit env: Environment, notAllowed:String): Expression = try {
      exp match {
        case ParenthesizedExpression(exp) => ParenthesizedExpression(linkExpression(exp))
        case UnaryOperation(op, exp) => UnaryOperation(op, linkExpression(exp))
        case BinaryOperation(f, op, s) => BinaryOperation(linkExpression(f), op, linkExpression(s))
        case CastExpression(cast, t) => CastExpression(cast, linkExpression(t))
        case ArrayAccess(arr, ind) => ArrayAccess(linkExpression(arr), linkExpression(ind))
        case ArrayCreation(typeName, size) => ArrayCreation(typeName, linkExpression(size))
        case Assignment(lhs, rhs) => Assignment(linkExpression(lhs)(env, "") match{ case x: LeftHandSide => x
              case _ => throw NameLinkingException("Trying to assign to a type.")
            }, linkExpression(rhs))
        case FieldAccess(acc, field) => FieldAccess(linkExpression(acc), field)
        case ClassCreation(cons, args) => ClassCreation(cons, args.map(linkExpression(_)))
        case ExprMethodInvocation(acc, meth, args) => ExprMethodInvocation(linkExpression(acc), meth, args.map(linkExpression(_)))
        case ThisMethodInvocation(thisType, meth, args) => ThisMethodInvocation(thisType, meth, args.map(linkExpression(_)))
        case InstanceOfCall(exp, check) => InstanceOfCall(linkExpression(exp), check)
        case x: This => x
        case VariableAccess(name) => linkVariable(name, env, notAllowed)
        case lit: Literal => lit
      }
    } catch {
      case FieldAccessIsProbablyPckException(path) =>  exp match {
        case FieldAccess(_, className) =>
          cus.find(cu => cu.packageName == Some(Name(path)) && cu.typeName == className).map(_ => RefTypeLinked(Some(Name(path)), className)).getOrElse(throw FieldAccessIsProbablyPckException(path :+ className))
        case _: VariableAccess => throw FieldAccessIsProbablyPckException(path)
        case _ => throw NameLinkingException(s"Could not find: ${path.reduce(_ + "." + _)}")
      }
    }

    def linkVariable(varName : String, env : Environment, notAllowed: String): LinkedExpression ={
      def checkClassFields(varName: String, refType: RefTypeLinked): Option[LinkedExpression] ={
        val classDef = refType.getTypeDef(cus).asInstanceOf[ClassDefinition]
        val field = classDef.fields.find(_.fieldName == varName)
        field match{
          case Some(varDecl) => Some(LinkedVariableOrField(varName, varDecl.fieldType, PathField(refType, varDecl.fieldName)))
          case None => classDef.parent match {
            case Some(refType: RefTypeLinked) => checkClassFields(varName, refType)
            case _ => None
          }
        }
      }
      if(varName == notAllowed) throw NameLinkingException(s"$varName not initialized yet")
      else      
      env.symbolMap.get(varName) match {
        case Some(x)  => LinkedVariableOrField(varName, x._1, x._2)
        case None => checkClassFields(varName, RefTypeLinked(env.pck, env.classDef.className)) match {
          case Some(x) => x
          case None =>
            cus.flatMap{
              case CompilationUnit(p, i, _, t) if(env.pck == p && env.classDef.className == t) => i
              case _ => Nil}.collectFirst{
              case LinkImport(name, refType) if name == varName => refType
            }.getOrElse(throw FieldAccessIsProbablyPckException(List(varName)))        
        }
      }
    }
    cus.map{
      case cu @ CompilationUnit(pck, imp, Some(classDef @ ClassDefinition(name, parent, interfaces, modifiers, fields, constructors, methods)), fileName) => CompilationUnit(pck, imp,
        Some(ClassDefinition(name, parent, interfaces, modifiers, linkFields(imp, classDef, pck),
          constructors.map(linkConstructor(_, pck, classDef)),
          methods.map(linkMethod(_, pck, classDef))))
          , fileName)
      case x => x
    }

  }
}

case class FieldAccessIsProbablyPckException(pck: List[String]) extends Exception

abstract class PathToDeclaration

case class PathField(refType: RefTypeLinked, fieldName: String) extends PathToDeclaration

case class PathPar(parameterName: String) extends PathToDeclaration

case class PathLocal(statementIndex: List[Int]) extends PathToDeclaration


