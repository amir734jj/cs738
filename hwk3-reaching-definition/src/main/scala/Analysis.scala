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

  def kill(stmt: Statement): Set[(String, Long)] = _

  def gen(stmt: Statement): Set[(String, Long)] = _

  // worklist algorithm to compute reaching definitions at the entry/exit of each CFG node
  def worklist {
    // TODO: you can just implement this

    var changedSet = Queue[Statement](this.stmts: _*)
    val table = this.stmts.map(x => x -> Node(x)).toMap
    while(!changedSet.isEmpty) {
      val n = changedSet.dequeue()

      for (pred <- n.pred) {
        table(n).entry += table(pred).exit
      }

      val oldOut = table(n).exit
      table(n).exit = gen(n) + (table(n).entry - kill(n))

      if (oldOut != table(n).exit) {
        for (s <- n.succ) {
          changedSet += s
        }
      }
    }
  }

  def buildGraph: Unit = this match {
    case Script(stmts) => visit(stmts)
    case BlockStmt(stmts) => visit(stmts)
    case VarDeclStmt(name, expr) => ()
    case VarDeclListStmt(decls) => visit(decls)
    case DoWhileStmt(cond, body) => {
      body.buildGraph
      appendSuccessor(body.entry)
      body.exit.foreach(e => e appendSuccessor this)
    }
    case WhileStmt(cond, body) => {
      body.buildGraph
      appendSuccessor(body.entry)
      body.exit foreach (e => e appendSuccessor this)
    }
    case SwitchStmt(cond, cases, defaultCase) => {
      // Aggregate switch branches (include defaultCase)
      val aggregatedCases = defaultCase match {
        case Some(x) => cases ++ Seq(x)
        case None => cases
      }
      visit(aggregatedCases)
      aggregatedCases foreach (c => appendSuccessor(c.entry))
    }
    case IfStmt(cond, thenPart, elsePart) => {
      thenPart.buildGraph
      appendSuccessor(thenPart.entry)
      elsePart match {
        case EmptyStmt() =>
        case _ => {
          elsePart.buildGraph
          appendSuccessor(elsePart.entry)
        }
      }
    }
    case ExprStmt(expr) => ()
    case _ => ()
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
 
