package nameResolution


import ast._
import main.CompilerError


object VarResolver{
  //( Map from name to new name, what to add to the new name, current class, previous cus)
  case class Environment(symbolMap: Map[String, (Type, PathToDeclaration)], currentPosition: PathToVariable, previousCUS: List[CompilationUnit],pck: Option[Name], classDef: ClassDefinition){
    def open : Environment = this match {
      case Environment(sm, PathToVariable(pck, cn, ind, list), pcus, cpck,  cd) =>Environment(sm, PathToVariable(pck, cn, ind, 0 :: list), pcus, cpck, cd)      
    }
    def inc : Environment = this match {
      case Environment(sm, PathToVariable(pck, cn, ind, list), pcus, cpck, cd) =>Environment(sm, PathToVariable(pck, cn, ind, (list.head + 1) :: list.tail), pcus, cpck, cd)
    }
    def update(stmt: Statement) : Environment = stmt match {
      case decl: LocalVariableDeclaration => if(symbolMap contains decl.identifier) throw new CompilerError(s"Name resolution exception: ${decl.identifier} is already define") else Environment(symbolMap + (decl.identifier -> (decl.typeName, currentPosition)), currentPosition,  previousCUS, pck, classDef).inc
      case _ => inc

    }
  }

  def variableLink(cus: List[CompilationUnit]): List[CompilationUnit] = cus.map{
    case cu @ CompilationUnit(pck, imp, Some(classDef @ ClassDefinition(name, parent, interfaces, modifiers, fields, constructors, methods)), fileName) => checkFields(fields); CompilationUnit(pck, imp,
      Some(ClassDefinition(name, parent, interfaces, modifiers, fields,
        constructors.zipWithIndex.map{case (_, index) => linkConstructor(cus, index, pck, classDef)},
        methods.zipWithIndex.map{case (_, index) => linkMethod(cus, index, pck, classDef)}))
        , fileName)
    case x => x
  }

  def checkFields(fields: List[FieldDeclaration]): Unit = {
    val fieldName = fields.map(_.fieldName)
    if(fieldName.distinct.size != fieldName.size) throw CompilerError("There is two fields with the same name")
  }

  def linkConstructor(cus: List[CompilationUnit], index: Int, pck: Option[Name], classDef: ClassDefinition): ConstructorDeclaration = {
    val cons = classDef.constructors(index)
    val parameterMap = Map( cons.parameters.map{case Parameter(parType, id) => (id, ( parType, PathToParameter(pck, classDef.className, index, id)))}:_*)
    val curPos = PathToVariable(pck, classDef.className, index, List(0))
    val env = Environment(parameterMap, curPos, cus, pck, classDef)
    ConstructorDeclaration(cons.modifiers, cons.parameters, implementationLink(env, cons.implementation))
  }


  def linkMethod(cus: List[CompilationUnit], index: Int, pck: Option[Name], classDef: ClassDefinition) : MethodDeclaration = {
    val meth = classDef.methods(index)
    val parameterMap = Map(meth.parameters.map{case Parameter(parType, id) => (id, (parType, PathToParameter(pck, classDef.className, index, id)))}:_*)
    val curPos = PathToVariable(pck, classDef.className, index, List(0))
    val env = Environment(parameterMap, curPos, cus, pck, classDef)
    MethodDeclaration(meth.methodName, meth.returnType, meth.modifiers, meth.parameters, meth.implementation.map(implementationLink(env, _)))
  }
  
  def implementationLink(environment: Environment, block: Block): Block = Block(passThroughStatements(block.statements, environment))

  


  def passThroughStatements(stmts: List[Statement], env: Environment): List[Statement] = stmts match {
    case x :: xs => linkStatement(x, env) :: passThroughStatements(xs, env.update(x))
    case Nil => Nil
  }

   def linkStatement(stmt: Statement, env: Environment) : Statement = stmt match {
     case Block(stmts) => Block(passThroughStatements(stmts, env.open))
     case EmptyStatement => EmptyStatement
     case ExpressionStatement(exp) => ExpressionStatement(linkExpression(exp, env))
     case ForStatement(init, cond, inc, stmt) => val forEnv = init.foldLeft(env.open)(_.update(_));
       ForStatement(init.map(linkStatement(_, env)), cond.map(linkExpression(_, forEnv.inc )), inc.map(linkExpression(_, forEnv.inc.inc)), linkStatement(stmt, forEnv.inc.inc.inc))
     case IfStatement(cond, ifStmt, elseStmt) => IfStatement(linkExpression(cond, env), linkStatement(ifStmt, env.open), elseStmt.map(linkStatement(_, env.open.inc)))
     case ReturnStatement(exp) => ReturnStatement(exp.map(linkExpression(_, env)))
     case LocalVariableDeclaration(typeName, id, initializer) => LocalVariableDeclaration(typeName, id, initializer.map(linkExpression(_, env)))
     case WhileStatement(cond, loop) => WhileStatement(linkExpression(cond, env), linkStatement(loop, env.open))
   }

   def linkExpression(exp: Expression, env: Environment): Expression = exp match {
     case UnaryOperation(op, exp) => UnaryOperation(op, linkExpression(exp, env))
     case BinaryOperation(f, op, s) => BinaryOperation(linkExpression(f, env), op, linkExpression(s, env))
     case CastExpression(cast, t) => CastExpression(cast, linkExpression(t, env))
     case ArrayAccess(arr, ind) => ArrayAccess(linkExpression(arr, env), linkExpression(ind, env))
     case ArrayCreation(typeName, size) => ArrayCreation(typeName, linkExpression(size, env))
     case Assignment(lhs, rhs) => Assignment(linkExpression(lhs, env), linkExpression(rhs, env))
     case FieldAccess(acc, field) => FieldAccess(linkExpression(acc, env), field)
     case ClassCreation(cons, args) => ClassCreation(cons, args.map(linkExpression(_, env)))
     case MethodInvocation(acc, meth, args) => MethodInvocation(acc.map(linkExpression(_, env)), meth, args.map(linkExpression(_, env)))
     case InstanceOfCall(exp, check) => InstanceOfCall(linkExpression(exp, env), check)
     case x: This => x
     case VariableAccess(name) => linkVariable(name, env)
     case lit : Literal => lit
   }

   

  def linkVariable(varName : String, env : Environment): LinkedVariableOrField ={
    def checkClassFields(varName: String, classDef: ClassDefinition): LinkedVariableOrField ={
      val field = classDef.fields.find(_.fieldName == varName)
      field match{
        case Some(varDecl) => LinkedVariableOrField(varName, varDecl.fieldType, PathToField(env.pck, env.classDef.className, varDecl.fieldName))
        case None => sys.error("We do not handle the link to a parent field yet...")
          // case None => classDef.parent match {
          //   case Some(RefTypeLinked(_, parent @ ClassDefinition(_, _, _, _, _, _, _))) => checkClassFields(varName, parent)
          //   case _ => throw new Exception(s"Name resolution exception: $varName does not exists")
          // }
      }
    }

    env.symbolMap.get(varName).map(x => LinkedVariableOrField(varName, x._1, x._2))getOrElse(checkClassFields(varName, env.classDef))
  }



}

case class LinkedVariableOrField(name : String, varType: Type, variablePath : PathToDeclaration) extends Expression{
  lazy val getType: Type = varType
  val children = Nil
}

abstract class PathToDeclaration

case class PathToField(pck: Option[Name], classDef: String, fieldName: String) extends PathToDeclaration



case class PathToParameter(pck: Option[Name], classDef: String, consIndex: Int, parameterName: String) extends PathToDeclaration
//Warning Reversed list of statement!!!
case class PathToVariable(pck: Option[Name], classDef: String, consIndex: Int, statementIndex: List[Int]) extends PathToDeclaration


