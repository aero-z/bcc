package parser

import scanner.Token
import scanner.KeywordToken
import scanner.ScopingToken
import scanner.IntegerToken
import scanner.OperatorToken
import ast._


object Weeder {
  def check(ast: AstNode): CheckResult = {
    ast.children.foldLeft(ast.isValid)((cr, a) => {
      cr ++ check(a)
    })
  }
}
