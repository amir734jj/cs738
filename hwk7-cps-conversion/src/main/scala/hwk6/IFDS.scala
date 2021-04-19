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
    case IntraNode(stmt) => {
      l
    }

    // for each parameter p, if its argument is not initialized, then p is not initialized
    case CallNode(stmt, to) => {
      l
    }
    // add variables appearing in the body of the function and the return variable
    case EntryNode(Some(FunctionDecl(_, FunctionExpr(_, ps, stmt)))) => {
      l
    }

    case ExitNode(Some(stmt)) => {
      l
    }

    case n@RetNode(stmt, _) => {
      l
    }
    case _ => l // transfer function didn't change anything
  }
}
