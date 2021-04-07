package hwk6

import common._
import interprocedural._

object BinaryMap {

  implicit def latticeToObject(self: BinaryMap) = new {
    def gen(labels: String*) = BinaryMap(self.vars ++ labels.map((Util.zero, _)))

    def apply(label: String, e: Expression) = {
      val gen = Util.fv(e)
      BinaryMap {
        self.vars.flatMap { case (source, sink) => {
          if (gen.contains(sink))
            Set(label)
          else if (sink != label)
            Set(sink)
          else
            Set[String]()
        }.map((source, _))
        }
      }
    }
  }
}

case class BinaryMap(vars: Set[(String, String)]) extends Lattice[BinaryMap] {
  def lub(that: BinaryMap) = BinaryMap(vars.union(that.vars))

  override def toString = "{" + vars.toList.sortBy(_._1).sortBy(_._2).mkString(", ") + "}"
}

object IFDS {

}

// uninitialized variables: forward and may analysis
case class IFDS(stmt: Statement) extends Analysis[BinaryMap] {

  import IFDS._

  val cfg = ForwardCFG(stmt)
  val entry = real_entry
  val exit = real_exit

  val extremalValue = BinaryMap(Set((Util.zero, Util.zero)) ++ (Util.vars(stmt) map ((Util.zero, _))))
  val bottom = BinaryMap(Set())

  def transfer(node: Node, l: BinaryMap) = node match {
    case IntraNode(stmt) => transfer(stmt, l)

    // for each parameter p, if its argument is not initialized, then p is not initialized
    case CallNode(stmt, to) => {
      def h(args: List[Expression]): BinaryMap = {
        val Some(FunctionDecl(_, FunctionExpr(_, ps, _))) = to.f
        val params = ps.map(_.str)
        val r1 = (Set(Util.zero) ++ params.drop(args.length)).map(x => (Util.zero, x))
        val r2 = (params zip args).flatMap {
          case (formal, actual) => l.vars.withFilter { case (_, sink) => Util.fv(actual).contains(sink) }.map {
            case (source, _) => (source, formal)
          }
        }
        BinaryMap(r1 ++ r2) // function f(x, y) { ... }   f(10);
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
      BinaryMap(l.vars.map { case (_, sink) => (sink, sink) })
        .gen((Util.vars(stmt) -- ps.map(x => x.str) ++ Set(Util.ret)).toSeq: _*) // uninitialized parameters + all local variables (minus parameters) + return variable
    }

    case ExitNode(Some(_)) => BinaryMap(l.vars.filter { case (_, sink) => sink == Util.ret }) // keep the return variable if it is present

    case n@RetNode(stmt, _) => {
      val call = cfg.call_ret(n)
      val callEntry = entry(call)
      val callExit = exit(call)

      def h(label: String) = {
        BinaryMap(callExit.vars
          .filter { case (_, sink) => l.vars.contains((sink, Util.ret)) }
          .map { case (source, _) => (source, label) } ++
          callEntry.vars.filter { case (_, sink) => sink != label })
      }

      stmt match {
        case ExprStmt(AssignExpr(_, LVarRef(x), FuncCall(_, _))) => h(x) // x = f(e);
        case VarDeclStmt(IntroduceVar(x), FuncCall(_, _)) => h(x) // var x = f(e);
        case _ => callEntry // f(e);
      }
    }

    case _ => l
  }

  def transfer(stmt: Statement, l: BinaryMap): BinaryMap = {
    stmt match {
      case VarDeclStmt(IntroduceVar(y), e) => {
        e match {
          case EmptyExpr() => l.gen(y) // var y;
          case _ => l(y, e) // var y = e;
        }
      }
      case ExprStmt(AssignExpr(_, LVarRef(y), e)) => l(y, e) // y = e;
      case ReturnStmt(e) => l(Util.ret, e) // return e;
      case _ => l
    }
  }
}
