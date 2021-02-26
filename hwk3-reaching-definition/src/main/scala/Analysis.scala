import javax.tools.Diagnostic.Kind
import scala.collection.mutable
import scala.collection.mutable.Set
import scala.collection.mutable.Queue

// stmt: a program with CFG already built
case class Analysis (stmt: Statement) { 
  val stmts = cfgNodes.toList // the list of all CFG statements
  val map = stmts.map(s => (s, Node(s))).toMap  // map from CFG statements to nodes
  val nodes = map.values.toList // all CFG nodes
	  
  // collect the CFG statements from a program
  private def cfgNodes : Set[Statement] = {
    def h (stmt: Statement, sofar: Set[Statement]) { 
      sofar += stmt

      for(s <- stmt.succ; if ! sofar.contains(s)) h(s, sofar)
    }
    val sofar = Set[Statement]()			
    h(stmt.entry, sofar)
    sofar 
  }
	
  // helper methods to retrieve the successors and predecessors of a CFG node
  def succ(node: Node) = node.stmt.succ.map(s => map(s))
  def pred(node: Node) = node.stmt.pred.map(s => map(s))	
	
  // all variables assigned in this program
  val variables: Set[String] = vars(this.stmts)
  
  private def vars(stmt: Statement): Set[String] = {
    stmt match {
      case Script(stmts) => vars(stmts)
      case BlockStmt(stmts) => vars(stmts)
      case VarDeclListStmt(stmts) => vars(stmts)
      case VarDeclStmt(x, e) => Set(x.str)
      case ExprStmt(AssignExpr(_, LVarRef(n), _)) => Set(n) 
      case IfStmt(_, t, e) => vars(t).union(vars(e))
      case WhileStmt(_, b) => vars(b)
      case DoWhileStmt(_, b) => vars (b)
      case SwitchStmt(_, cases, d) => (d match { case Some(c) => c::cases case None => cases }).map(c => vars(c)).reduce((a,b)=>a.union(b))
      case CaseStmt(_, s) => vars(s)
      case _ => Set()
    }
  }
  
  private def vars(stmts: List[Statement]): Set[String] = stmts.map(s => vars(s)).reduce((a,b)=>a.union(b))

  def kill(expr: Expression)(implicit stmt: Statement): Set[(String, Long)] = {
    expr match {
      case InfixExpr(op, expr1, expr2) => kill(expr1) ++ kill(expr2)
      case AssignExpr(op, LVarRef(name), expr) => Set((name, (-1).asInstanceOf[Long])) ++ stmts.map(x => (name, x.id)).toSet
      case VarRef(name) => Set()
      case NumberLit(value) => Set()
    }
  }

  def kill(implicit stmt: Statement): Set[(String, Long)] = {
    stmt match {
      case VarDeclStmt(name, expr) => Set((name.str, (-1).asInstanceOf[Long])) ++ stmts.map(x => (name.str, x.id)).toSet
      case IfStmt(cond, thenPart, elsePart) => Set()
      case BlockStmt(stmts) => stmts.foldLeft(Set[(String, Long)]())((acc, s) => kill(s) ++ acc)
      case WhileStmt(cond, body) => Set()
      case ExprStmt(expr) => kill(expr)
    }
  }

  def gen(expr: Expression)(implicit stmt: Statement): Set[(String, Long)] = {
    expr match {
      case AssignExpr(op, LVarRef(name), expr) => Set((name, stmt.id)) ++ gen(expr)
      case InfixExpr(op, expr1, expr2) => gen(expr1) ++ gen(expr2)
      case VarRef(name) => Set()
      case NumberLit(value) => Set()
    }
  }

  def gen(implicit stmt: Statement): Set[(String, Long)] = {
    stmt match {
      case VarDeclStmt(name, expr) => Set((name.str, stmt.id)) ++ gen(expr)
      case IfStmt(cond, thenPart, elsePart) => Set()
      case BlockStmt(stmts) => stmts.foldLeft(Set[(String, Long)]())((acc, s) => gen(s) ++ acc)
      case ExprStmt(expr) => gen(expr)
      case WhileStmt(cond, body) => Set()
    }
  }

  // worklist algorithm to compute reaching definitions at the entry/exit of each CFG node
  def worklist {
    // TODO: you can just implement this

    var changedSet = Queue[Statement](this.stmts: _*)
    implicit val table = this.nodes.map(x => x.stmt -> x).toMap
    while(!changedSet.isEmpty) {
      val n = changedSet.dequeue()

      if (n.pred.isEmpty) {
        table(n).entry = variables.map(v => (v, (-1).asInstanceOf[Long]))
      }

      for (pred <- n.pred) {
        table(n).entry ++= table(pred).exit
      }

      val oldOut = table(n).exit
      table(n).exit = gen(n) ++ (table(n).entry -- kill(n))

      if (oldOut != table(n).exit) {
        for (s <- n.succ) {
          changedSet += s
        }
      }
    }
  }

  // make a dot graph with entry/exit reaching definition of every node
  def toDotGraph = {
    val entry = (s: Statement) => map(s).entry.toList.sortBy(x=>x).mkString(" ")
    val exit = (s: Statement) => map(s).exit.toList.sortBy(x=>x)mkString(" ")
    val labels = stmts.map(s=>s"${s.id} [label = ${s.dotStr}, xlabel = ${"\""}${entry(s)}\\n${exit(s)}${"\""}]")
    s"digraph {\n${(labels ++ stmt.toDot).reduceLeft((c,e) => c + "\n" + e)}\n}"
  }
}

// CFG node to hold the reaching definitions at the entry/exit of a CFG statement
case class Node(stmt: Statement) {
  var entry = Set[(String, Long)]()
  var exit = Set[(String, Long)]()
}
 
