
import ast._

import scalaz._
import Scalaz._

object VarResolver{

  type Environment = (Map[String, VariableDeclaration], ClassDefinition)
  

  def variableLink(compilationUnit: CompilationUnit): CompilationUnit = compilationUnit match {
    case CompilationUnit(pck, imp, Some(classDef @ ClassDefinition(name, parent, interfaces, modifiers, fields, constructors, methods)), fileName) => CompilationUnit(pck, imp,
      Some(ClassDefinition(name, parent, interfaces, modifiers, fields, constructors.map(linkLocalVariable(_, classDef)), methods.map(linkLocalVariable(_, classDef))))
, fileName)
    case _ => compilationUnit
  }

  def linkLocalVariable(cons: ConstructorDeclaration, classDef: ClassDefinition): ConstructorDeclaration = ConstructorDeclaration(cons.modifiers, cons.parameters, implementationLink(cons.parameters, cons.implementation, classDef))


  def linkLocalVariable(meth: MethodDeclaration, classDef: ClassDefinition) : MethodDeclaration = 
    MethodDeclaration(meth.methodName, meth.returnType, meth.modifiers, meth.parameters, meth.implementation.map(implementationLink(meth.parameters, _, classDef)))
  
  def implementationLink(parameters: List[Parameter], block: Block, classDef: ClassDefinition): Block = {
    val environment = (Map(parameters.map(x => (x.id ,x)):_*), classDef)
    
    
    block

  }

  


  def passThroughStatements(stmts: List[Statement], env: Environment): List[Statement] = stmts match {
    case x :: xs => linkStatement(x, env) :: passThroughStatements(xs, update(x, env))
    case Nil => Nil
  }

  def linkStatement(stmt: Statement, env: Environment) : Statement = stmt match {
    case Block(stmts) => Block(passThroughStatements(stmts, env))
    case EmptyStatement => EmptyStatement
    case ExpressionStatement(exp) => ExpressionStatement(linkExpression(exp, env))
    case ForStatement(init, cond, inc, stmt) => val forEnv = init.foldLeft(env)((init, env) => update(env, init));
      ForStatement(init.map(linkStatement(_, env)), cond.map(linkExpression(_, forEnv)), inc.map(linkExpression(_, forEnv)), linkStatement(stmt, forEnv))
    case IfStatement(cond, ifStmt, elseStmt) => IfStatement(linkExpression(cond, env), linkStatement(ifStmt, env), elseStmt.map(linkStatement(_, env)))
    case ReturnStatement(exp) => ReturnStatement(exp.map(linkExpression(_, env)))
    case LocalVariableDeclaration(typeName, id, initializer) => LocalVariableDeclaration(typeName, id, initializer.map(linkExpression(_, env)))
    case WhileStatement(cond, loop) => WhileStatement(linkExpression(cond, env), linkStatement(loop, env))
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
    case This => This
    case variable : VariableAccess => linkVariable(variable, env)
  }

  def update(stmt: Statement, env: Environment) : Environment = stmt match {
    case decl: LocalVariableDeclaration => if(env._1 contains decl.identifier) throw new Exception("Variable Name resolution exception") else (env._1 + (decl.identifier -> decl), env._2)
    case _ => env
  }

  def linkVariable( variable : VariableAccess, env : Environment): LinkedVariableOrField = LinkedVariableOrField(variable.str, null)



}

case class LinkedVariableOrField(name : String, declaration : VariableDeclaration) extends Expression{
  val children = Nil
}
