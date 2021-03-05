import java.io.File

object Main {
  def main(args: Array[String]) {
    val ast = GenerateAST(new File("test/rd.js"))

    ast.prep
    ast.buildGraph

    val a = RD(ast)
//        val a = LV(ast)

    a.worklist

    println(a)

    println

    print(a.toDotGraph)
  }
}
