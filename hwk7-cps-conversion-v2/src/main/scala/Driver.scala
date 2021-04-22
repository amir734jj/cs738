import common.GenerateAST
import cps.{CPS, KVar}

import java.io.File

object Main {
  def main(args: Array[String]) {
    val ast = GenerateAST(new File("test/fact4.js"))
    val cps = CPS.t_c(ast, Map(), KVar("halt"))
    cps.prep
    println("let halt = x => console.log(x)\n")
    println(cps)
  }
}