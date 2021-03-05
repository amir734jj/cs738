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
  
  override def toString = {
    cfg.nodes.sortBy(x => x.stmt.id).map(n => f"${n.stmt.id}%-4d ${real_entry(n)}%-40s ${real_exit(n)}").mkString("\n")
  }
}

object DataFlow {
  type UnionLatticeStmt = UnionLattice[(String, Long)]
  type UnionLatticeVariable = UnionLattice[String]
}

import DataFlow._

// UnionLattice
case class UnionLattice[T](set: Set[T])(implicit mkString: Set[T] => String) extends Lattice[UnionLattice[T]] {
  // least upper bound or join (supremum)
  override def lub(that: UnionLattice[T]): UnionLattice[T] = UnionLattice(set union that.set)

  // greatest lower bound or meet (infimum)
  def glb(that: UnionLattice[T]): UnionLattice[T] = UnionLattice(set intersect that.set)

  override def toString = mkString(set)
}

object RD {
  def vars(stmt: Statement): Set[String] = {
    stmt match {
      case Script(stmts) => vars(stmts)
      case BlockStmt(stmts) => vars(stmts)
      case VarDeclListStmt(stmts) => vars(stmts)
      case VarDeclStmt(x, e) => Set(x.str)
      case ExprStmt(AssignExpr(_, LVarRef(n), _)) => Set(n)
      case IfStmt(_, t, e) => vars(t).union(vars(e))
      case WhileStmt(_, b) => vars(b)
      case DoWhileStmt(_, b) => vars (b)
      case SwitchStmt(_, cases, d) => Set((d match {
        case Some(c) => c::cases
        case None => cases
      }).flatMap(vars):_*)
      case CaseStmt(_, s) => vars(s)
      case _ => Set()
    }
  }

  def vars(stmts: List[Statement]): Set[String] = Set(stmts.flatMap(s => vars(s)):_*)

  def kill(expr: Expression)(implicit stmt: Statement, stmts: Seq[Statement]): Set[(String, Long)] = {
    expr match {
      case InfixExpr(op, expr1, expr2) => kill(expr1) ++ kill(expr2)
      case AssignExpr(op, LVarRef(name), expr) => Set((name, (-1).asInstanceOf[Long])) ++ stmts.map(x => (name, x.id))
      case VarRef(name) => Set()
      case NumberLit(value) => Set()
    }
  }

  def kill(implicit stmt: Statement, stmts: Seq[Statement]): Set[(String, Long)] = {
    stmt match {
      case VarDeclStmt(name, expr) => Set((name.str, (-1).asInstanceOf[Long])) ++ stmts.map(x => (name.str, x.id))
      case IfStmt(cond, thenPart, elsePart) => Set()
      case BlockStmt(stmts) => stmts.foldLeft(Set[(String, Long)]())((acc, s) => kill(s, stmts) ++ acc)
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

  implicit val mkString = (_: Set[(String, Long)]).toSeq.sorted.mkString(" ")
}

case class RD(stmt: Statement) extends Analysis[UnionLatticeStmt]
{
  import RD._

  override val cfg: CFG = ForwardCFG(stmt)
  override val extremalValue: UnionLatticeStmt = new UnionLatticeStmt(vars(stmt).map((_, (-1).asInstanceOf[Long])))
  override val bottom: UnionLatticeStmt = new UnionLatticeStmt(Set())
  override val entry: mutable.Map[Node, UnionLatticeStmt] = real_entry
  override val exit: mutable.Map[Node, UnionLatticeStmt] = real_exit
  implicit val stmts: Seq[Statement] = cfg.stmts

  override def transfer(stmt: Statement, l: UnionLatticeStmt): UnionLatticeStmt = {
    new UnionLatticeStmt(gen(stmt) ++ (l.set -- kill(stmt, stmts)))
  }
}

object LV {
  def kill(expr: Expression): Set[String] = {
    expr match {
      case InfixExpr(op, expr1, expr2) => kill(expr1) ++ kill(expr2)
      case AssignExpr(op, LVarRef(name), expr) => Set(name)
      case VarRef(name) => Set(name)
      case NumberLit(value) => Set()
    }
  }

  def kill(stmt: Statement): Set[String] = {
    stmt match {
      case VarDeclStmt(name, expr) => Set(name.str)
      case IfStmt(cond, thenPart, elsePart) => kill(cond) ++ kill(thenPart) ++ kill(elsePart)
      case BlockStmt(stmts) => Set(stmts.flatMap(x => kill(x)):_*)
      case WhileStmt(cond, body) => kill(cond) ++ kill(body)
      case ExprStmt(expr) => kill(expr)
    }
  }

  def gen(expr: Expression): Set[String] = {
    expr match {
      case AssignExpr(op, LVarRef(name), expr) => gen(expr)
      case InfixExpr(op, expr1, expr2) => gen(expr1) ++ gen(expr2)
      case VarRef(name) => Set(name)
      case NumberLit(value) => Set()
    }
  }

  def gen(stmt: Statement): Set[String] = {
    stmt match {
      case VarDeclStmt(name, expr) => gen(expr)
      case IfStmt(cond, thenPart, elsePart) => gen(cond) ++ gen(thenPart) ++ gen(elsePart)
      case BlockStmt(stmts) => Set(stmts.flatMap(x => gen(x)):_*)
      case ExprStmt(expr) => gen(expr)
      case WhileStmt(cond, body) => gen(cond) ++ gen(body)
    }
  }

  implicit val mkString = (s : Set[String]) => f"{${s.toSeq.sorted.mkString(", ")}}"
}

case class LV(stmt: Statement) extends Analysis[UnionLatticeVariable] {
  import LV._

  override val cfg: CFG = BackwardCFG(stmt)
  override val extremalValue: UnionLatticeVariable = new UnionLatticeVariable(Set())
  override val bottom: UnionLatticeVariable = new UnionLatticeVariable(Set())
  override val entry: mutable.Map[Node, UnionLatticeVariable] = real_exit
  override val exit: mutable.Map[Node, UnionLatticeVariable] = real_entry
  implicit val stmts: Seq[Statement] = cfg.stmts

  override def transfer(stmt: Statement, l: UnionLatticeVariable): UnionLatticeVariable = {
    new UnionLatticeVariable(gen(stmt) ++ (l.set -- kill(stmt)))
  }
}
