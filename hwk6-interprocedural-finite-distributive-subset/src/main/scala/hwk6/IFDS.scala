package hwk6

import common._
import interprocedural._

object BinaryMap {

  implicit def latticeToObject(self: BinaryMap) = new {
    def gen(labels: String*) = BinaryMap(self.vars ++ (labels map ((Util.zero, _))))

    def apply(label: String, e: Expression) = BinaryMap {
      self.vars.flatMap { case (source, sink) =>
        if (Util.fv(e).contains(sink))
          Some((source, label))
        else if (sink != label)
          Some((source, sink))
        else
          None
      }
    }
  }
}

case class BinaryMap(vars: Set[(String, String)]) extends Lattice[BinaryMap] {
  def lub(that: BinaryMap) = BinaryMap(vars ++ that.vars)

  override def toString = "{" + (vars.toList sortBy (_._1) sortBy (_._2) mkString ", ") + "}"
}

object IFDS {
  def resolve(stmt: Statement, l: BinaryMap): BinaryMap = {
    stmt match {
      case VarDeclStmt(IntroduceVar(y), e) =>
        e match {
          case EmptyExpr() => l gen y // var y;
          case _ => l(y, e) // var y = e;
        }
      case ExprStmt(AssignExpr(_, LVarRef(y), e)) => l(y, e) // y = e;
      case ReturnStmt(e) => l(Util.ret, e) // return e;
      case _ => l
    }
  }
}

// uninitialized variables: forward and may analysis
case class IFDS(stmt: Statement) extends Analysis[BinaryMap] {

  import IFDS._

  val cfg = ForwardCFG(stmt)
  val entry = real_entry
  val exit = real_exit

  val extremalValue = BinaryMap(Set((Util.zero, Util.zero)) ++ (Util.vars(stmt) map ((Util.zero, _)))) // include all labels ++ (0, 0)
  val bottom = BinaryMap(Set()) // everything may be un-initialized, useless

  def transfer(node: Node, l: BinaryMap) = node match {
    case IntraNode(stmt) => resolve(stmt, l)

    // for each parameter p, if its argument is not initialized, then p is not initialized
    case CallNode(stmt, to) => {
      def resolve(args: List[Expression]): BinaryMap = {
        val Some(FunctionDecl(_, FunctionExpr(_, ps, _))) = to.f
        val params = ps.map(_.str)
        val actuals = (params zip args).flatMap {
          case (formal, actual) => (l.vars withFilter { case (_, sink) => Util.fv(actual) contains sink }).map {
            case (source, _) => (source, formal)
          }
        }.toSet
        val formals = (Set(Util.zero) ++ (params drop args.length)).map((Util.zero, _))
        BinaryMap(formals ++ actuals) // function f(x, y) { ... }   f(10);
      }

      stmt match {
        case ExprStmt(FuncCall(_, args)) => resolve(args) // f(a);
        case ExprStmt(AssignExpr(_, _, FuncCall(_, args))) => resolve(args) // x = f(a);
        case VarDeclStmt(_, FuncCall(_, args)) => resolve(args) // var x = f(a);
        case _ => bottom
      }
    }
    // add variables appearing in the body of the function and the return variable
    case EntryNode(Some(FunctionDecl(_, FunctionExpr(_, ps, stmt)))) =>
      BinaryMap(l.vars map { case (_, sink) => (sink, sink) })
        .gen(((Util vars stmt) -- ps.map(_.str) ++ Set(Util.ret)).toSeq: _*) // uninitialized parameters + all local variables (minus parameters) + return variable

    case ExitNode(Some(_)) => BinaryMap(l.vars filter { case (_, sink) => sink == Util.ret }) // keep the return variable if it is present

    case n@RetNode(stmt, _) => {
      implicit val call = cfg call_ret n

      def resolve(label: String)(implicit call: CallNode) = BinaryMap(exit(call).vars
        .filter { case (_, sink) => l.vars contains(sink, Util.ret) }
        .map { case (source, _) => (source, label) } ++
        (entry(call).vars filter { case (_, sink) => sink != label }))

      stmt match {
        case ExprStmt(AssignExpr(_, LVarRef(x), FuncCall(_, _))) => resolve(x) // x = f(e);
        case VarDeclStmt(IntroduceVar(x), FuncCall(_, _)) => resolve(x) // var x = f(e);
        case _ => entry(call) // f(e);
      }
    }
    case _ => l // transfer function didn't change anything
  }
}
