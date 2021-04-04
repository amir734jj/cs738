package hwk6

import common._
import interprocedural._

case class BinaryMap(vars: Set[(String, String)]) extends Lattice[BinaryMap] {
  def lub(that: BinaryMap) = BinaryMap(vars.union(that.vars))

  override def toString = "{" + vars.toList.sortBy(_._1).sortBy(_._2).mkString(", ") + "}"
}

// uninitialized variables: forward and may analysis
case class IDFS(stmt: Statement) extends Analysis[BinaryMap] {
  val cfg = ForwardCFG(stmt)
  val entry = real_entry
  val exit = real_exit

  val extremalValue = BinaryMap(Util.vars(stmt).map((Util.zero, _)))
  val bottom = BinaryMap(Set())

  def transfer(node: Node, l: BinaryMap) = node match {
    case IntraNode(stmt) => transfer(stmt, l)

    // for each parameter p, if its argument is not initialized, then p is not initialized
    case CallNode(stmt, to) => {
      def h(args: List[Expression]): BinaryMap = {
        val Some(FunctionDecl(_, FunctionExpr(_, ps, _))) = to.f
        val params = ps.map(p => p.str)
        val s = for ((p, e) <- params zip args; if initialized(e, l)) yield p
        BinaryMap(params.toSet -- s) // function f(x, y) { ... }   f(10);
      }

      stmt match {
        case ExprStmt(FuncCall(_, args)) => h(args) // f(a);
        case ExprStmt(AssignExpr(_, _, FuncCall(_, args))) => h(args) // x = f(a);
        case VarDeclStmt(_, FuncCall(_, args)) => h(args) // var x = f(a);
        case _ => bottom
      }
    }
    // add variables appearing in the body of the function and the return variable
    case EntryNode(Some(FunctionDecl(_, FunctionExpr(_, ps, stmt)))) => {
      BinaryMap(l.vars ++ (Util.vars(stmt) -- ps.map(p => p.str)) + Util.ret) // uninitialized parameters + all local variables (minus parameters) + return variable
    }

    case ExitNode(Some(_)) => BinaryMap(l.vars.intersect(Set(Util.ret))) // keep the return variable if it is present

    case n@RetNode(stmt, _) => {
      val lc = entry(cfg.call_ret(n)) // dataflow facts before the call

      def h(x: String) = BinaryMap(if (l.vars.contains(Util.ret)) lc.vars + x else lc.vars - x)

      stmt match {
        case ExprStmt(AssignExpr(_, LVarRef(x), FuncCall(_, _))) => h(x) // x = f(e);
        case VarDeclStmt(IntroduceVar(x), FuncCall(_, _)) => h(x) // var x = f(e);
        case _ => lc // f(e);
      }
    }

    case _ => l
  }

  // are variables in 'e' all initialized
  def initialized(e: Expression, l: BinaryMap) = Util.fv(e).intersect(l.vars).isEmpty

  def transfer(stmt: Statement, l: BinaryMap): BinaryMap = {
    def kill_gen(y: String, e: Expression) = BinaryMap(if (initialized(e, l)) l.vars - y else l.vars + y)

    stmt match {
      case VarDeclStmt(IntroduceVar(y), e) => {
        e match {
          case EmptyExpr() => BinaryMap(l.vars + y) // var y;
          case _ => kill_gen(y, e) // var y = e;
        }
      }
      case ExprStmt(AssignExpr(_, LVarRef(y), e)) => kill_gen(y, e) // y = e;
      case ReturnStmt(e) => kill_gen(Util.ret, e) // return e;
      case _ => l
    }
  }
}
