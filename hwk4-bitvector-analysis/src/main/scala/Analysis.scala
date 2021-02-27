import scala.collection.mutable
import scala.collection.mutable.{Map, Queue, Set}

trait CFG {
  val stmt: Statement // program to be analyzed
  val extremal : List[Node]  // nodes with extremal label
    
  // helper methods to retrieve the successors and predecessors of a CFG node
  def succ(n: Node) : List[Node]
  def pred(n: Node) : List[Node]
  
  // collect the CFG statements from a program
  private def cfgNodes: Set[Statement] = {
    def h (stmt: Statement, sofar: Set[Statement]) { 
      sofar += stmt

      for(s <- stmt.succ; if ! sofar.contains(s)) h(s, sofar)
    }
    val sofar = Set[Statement]()			
    h(stmt.entry, sofar)
    sofar 
  }
  
  val stmts = cfgNodes.toList // the list of all CFG statements
  val map = stmts.map(s => (s, Node(s))).toMap  // map from CFG statements to nodes
  val nodes = map.values.toList // all CFG nodes
}

case class Node(stmt: Statement)

case class ForwardCFG(stmt: Statement) extends CFG { 
  val extremal = List(map(stmt.entry)) 

  def succ(node: Node) = node.stmt.succ.map(s => map(s))
  def pred(node: Node) = node.stmt.pred.map(s => map(s))	
}

case class BackwardCFG(stmt: Statement) extends CFG {
  val extremal = stmt.exit.map(s => map(s))
  
  def succ(node: Node) = node.stmt.pred.map(s => map(s))
  def pred(node: Node) = node.stmt.succ.map(s => map(s))	
}

// abstract type of the data-flow facts
trait Lattice [L <: Lattice[L]] {
  // meet operator (the least upper bound of 'this' and 'that')
  def lub(that: L): L
}

// Bit Vector framework
trait Analysis[L <: Lattice[L]] {
  val cfg: CFG
  val extremalValue: L
  val bottom: L
  
  // actual entry/exit data-flow facts
  val real_entry = Map[Node, L]()
  val real_exit = Map[Node, L]()
  
  // relative entry/exit data-flow facts
  val entry: Map[Node,L]
  val exit: Map[Node,L]
  
  // transfer function for each statement
  def transfer(stmt: Statement, l: L): L
  
  def worklist {
    val queue = Queue[Node]()
    queue.enqueue(cfg.nodes:_*)
	  
    // initialize entry/exit data-flow facts
    for(n <- cfg.extremal) entry.put(n, extremalValue)  
    for(n <- cfg.nodes.diff(cfg.extremal)) entry.put(n, bottom) 
    for(n <- cfg.nodes) exit.put(n, bottom)

    while(!queue.isEmpty) {
      val n = queue.dequeue()
	     
      // calculate the entry data-flow facts of 'n'
      val l1 = cfg.pred(n).foldLeft(entry(n))((l, n1) => l.lub(exit(n1)))
      entry.put(n, l1)
      // calculate the exit data-flow facts of 'n'
      val l2 = transfer(n.stmt, l1)
	    
      // if this is different from previous data-flow facts
      if (l2 != exit(n)) { 
        exit.put(n, l2)
        queue.enqueue(cfg.succ(n).toList:_*)
      } 
    }
  }
  
  // make a dot graph with actual entry/exit data-flow facts of every node
  def toDotGraph = { 
    val labels = cfg.nodes.sortBy(n=>n.stmt.id).map(n=>s"${n.stmt.id} [label = ${n.stmt.dotStr}, xlabel = ${"\""}${real_entry(n)}\\n${real_exit(n)}${"\""}]")
    s"digraph {\n${(labels ++ cfg.stmt.toDot).reduceLeft((c,e) => c + "\n" + e)}\n}"
  }
  
  override def toString =  cfg.nodes.sortBy(x => x.stmt.id).map(n => f"${n.stmt.id}%-4d ${real_entry(n)}%-40s ${real_exit(n)}").mkString("\n")
}

case class SomeLattice() extends Lattice[SomeLattice] {
  override def lub(that: SomeLattice): SomeLattice = ???
}

case class RD(stmt: Statement) extends Analysis[SomeLattice] {
  override val cfg: CFG = ForwardCFG(stmt)
  override val extremalValue: SomeLattice = _
  override val bottom: SomeLattice = _
  override val entry: mutable.Map[Node, SomeLattice] = _
  override val exit: mutable.Map[Node, SomeLattice] = _

  override def transfer(stmt: Statement, l: SomeLattice): SomeLattice = ???
}

case class LV(stmt: Statement) extends Analysis[SomeLattice] {
  override val cfg: CFG = BackwardCFG(stmt)
  override val extremalValue: SomeLattice = _
  override val bottom: SomeLattice = _
  override val entry: mutable.Map[Node, SomeLattice] = _
  override val exit: mutable.Map[Node, SomeLattice] = _

  override def transfer(stmt: Statement, l: SomeLattice): SomeLattice = ???
}
