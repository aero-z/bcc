package codegen

import ast.CompilationUnit

object CodeGenerator {
  def generate(cus: List[CompilationUnit]): IrProgram = ???
  def makeAssembly(prog: IrProgram): X68Program = ???
}