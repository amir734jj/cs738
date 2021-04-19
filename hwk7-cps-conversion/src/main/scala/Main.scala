import common._

import java.io.File

object Main {
  def main(args: Array[String]) {
    val ast = GenerateAST(new File("test/uv1.js"))

        ast.prep
        ast.buildGraph(Map())
    //
    //    val a = IFDS(ast)
    //
    //    a.worklist
    //    println(a)
    //    println
    //    print(a.toDotGraph)


        println(visit(ast))
  }

  def variable(stmt: Statement) = "temp" + stmt.id

  var visited = List[Statement]()

  def visit(stmts: List[Statement]): List[Statement] = for (stmt <- stmts if !visited.contains(stmt)) yield visit(stmt)

  def visit(stmt: Statement): Statement = {
    visited +:= stmt
    stmt match {
      case n@Script(stmts) => Script(visit(stmts))
      case n@BlockStmt(stmts) => BlockStmt(visit(stmts))
      case n@VarDeclListStmt(decls) => n
      case n@EmptyStmt() => n
      case n@ExprStmt(a@AssignExpr(op, lv, FuncCall(func, args))) => {
        val tempVar = variable(n)
        val callback = FunctionExpr(None, List(IntroduceVar(tempVar)), BlockStmt(List(ExprStmt(AssignExpr(op, lv, VarRef(tempVar))))))
        val thing = FuncCall(func, args :+ callback)
        ExprStmt(thing)
      }
      case n@ExprStmt(expr) => n
      case n@VarDeclStmt(name, f@FuncCall(func, args)) => {
        val tempVar = variable(n)
        val callback = FunctionExpr(None, List(IntroduceVar(tempVar)), BlockStmt(List(VarDeclStmt(name, VarRef(tempVar))) ++ n.succ.map(x => visit(x))))
        val thing = FuncCall(func, args :+ callback)
        ExprStmt(thing)
      }
      case n@VarDeclStmt(name, expr) => n
      case n@FunctionDecl(name, FunctionExpr(ref, ps, body)) => FunctionDecl(name, FunctionExpr(ref, ps :+ IntroduceVar("ret"), visit(body)))
      case n@ReturnStmt(expr) => {
        ExprStmt(FuncCall(VarRef("ret"), List(expr)))
      }
      case n@IfStmt(cond, thenPart, elsePart) => IfStmt(cond, visit(thenPart), visit(elsePart))
      case n@SwitchStmt(cond, cases, defaultCase) => n
      case n@CaseStmt(expr, body) => n
      case n@BreakStmt(breakLabel) => n
      case n@ContinueStmt(continueLabel) => n
      case n@DoWhileStmt(cond, body) => n
      case n@WhileStmt(cond, body) => n
      case n@ForStmt(init, cond, increment, body) => n
      case n@ForInStmt(init, expr, body) => n
      case n@LabeledStmt(label, stmt) => n
      case n@TryStmt(body, catchClause, finalCatch) => n
      case n@CatchStmt(name, body) => n
      case n@ThrowStmt(expr) => n
    }
  }
}
