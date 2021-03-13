import common.GenerateAST
import hw5.CSUV

import java.io.File

object Main {
  def main(args: Array[String]) {
    val ast = GenerateAST(new File("test/uv1.js"))

    ast.prep
    ast.buildGraph(Map())

    //    val a = hw5.UV(ast)
    val a = CSUV(ast)

    a.worklist
    println(a)
    println
    print(a.toDotGraph)
  }
}
