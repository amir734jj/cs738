
package hwk;

import java.io.File
 

object Main {
  def main(args: Array[String]) { 
    val ast = GenerateAST(new File("test/while.js"))
    ast.prep
    
    print(ast)
    
    ast.visit
    
    println
    
    print(ast.toDotGraph)

  }
}
