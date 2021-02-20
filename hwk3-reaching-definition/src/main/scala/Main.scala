
import java.io.File
 

object Main {
  def main(args: Array[String]) { 
    val ast = GenerateAST(new File("test/rd1.js"))
    ast.prep 
    
    ast.buildGraph
    
    val a = Analysis(ast) 
    
    a.worklist
    
    for(n <- a.nodes.sortBy(x => x.stmt.id)) {
      println(f"${n.stmt.id}%-4d -- ${n.stmt.toString.take(15)} -- ${n.entry.toList.sortBy(x=>x).mkString(" ")}%-40s ${n.exit.toList.sortBy(x=>x).mkString(" ")}")
    }
    
    println
    
    print(a.toDotGraph)
  }
}
