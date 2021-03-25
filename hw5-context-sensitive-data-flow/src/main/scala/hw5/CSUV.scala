package hw5

import common.{AssignExpr, EmptyExpr, ExprStmt, Expression, FuncCall, FunctionDecl, FunctionExpr, IntroduceVar, LVarRef, ReturnStmt, Statement, Util, VarDeclStmt}

object Context {
  val MAX_SIZE = 2  // max call string size

  implicit def contextToObject(ctx: Context) = new {
    val extend = (id: Long) => Context((ctx.callStrings ++ Seq(id)) takeRight MAX_SIZE)
  }
}

case class Context(callStrings: List[Long]) {
  override def toString: String = f"[ ${callStrings mkString ", "} ]"
}

object ContextSensitiveLattice {
  implicit def latticeToObject(self: ContextSensitiveLattice) = new {
    val contexts: Set[Context] = self.vars map { case (context, _) => context }

    val variables = (ctx: Context) => self.vars
      .filter { case (context, _) => context == ctx }
      .map { case (_, str) => str }

    val initialized = (e: Expression, ctx: Context) => (Util.fv(e) intersect variables(ctx)).isEmpty

    val resolve = (label: String, e: Expression) => {
      val gen = contexts filter (!initialized(e, _)) map ((_, label))
      val kill = contexts filter (initialized(e, _)) map ((_, label))
      ContextSensitiveLattice(self.vars -- kill ++ gen)
    }

    def gen(labels: String*) = ContextSensitiveLattice(self.vars ++ (contexts flatMap (ctx => labels.map((ctx, _)))))

    val extendWith = (id: Long) => ContextSensitiveLattice(contexts.map(_.extend(id)).map((_, Util.zero)))
  }
}

case class ContextSensitiveLattice(vars: Set[(Context, String)]) extends Lattice[ContextSensitiveLattice] {
  override def lub(that: ContextSensitiveLattice): ContextSensitiveLattice = ContextSensitiveLattice(vars ++ that.vars)

  override def toString = f"{ ${(vars.filter { case (_, str) => str != Util.zero }.toList sortBy { case (_, str) => str }).mkString(", ")} }"
}

case class CSUV(stmt: Statement) extends Analysis[ContextSensitiveLattice] {
  override val extremalValue = ContextSensitiveLattice(((Util vars stmt) + Util.zero).map((Context(List()), _)))
  override val bottom = ContextSensitiveLattice(Set())

  // Forward CFG
  override val cfg: CFG = ForwardCFG(stmt)
  override val entry = real_entry
  override val exit = real_exit

  def transfer(node: Node, lattice: ContextSensitiveLattice): ContextSensitiveLattice = node match {
    case IntraNode(stmt) => transfer(stmt, lattice)

    // for each parameter p, if its argument is not initialized, then p is not initialized
    case CallNode(stmt, to) =>
      def resolve(args: List[Expression]) = {
        val Some(FunctionDecl(_, FunctionExpr(_, ps, _))) = to.f
        val params = ps.map(p => p.str).distinct
        val gen = lattice.contexts.flatMap(ctx => (params zip args withFilter { case (_, e) => !(lattice initialized(e, ctx)) }).map { case (y, _) => (ctx.extend(stmt.id), y) })
        val kill = lattice.contexts.flatMap(ctx => (params zip args withFilter { case (_, e) => lattice initialized(e, ctx) }).map { case (y, _) => (ctx.extend(stmt.id), y) })
        ContextSensitiveLattice((lattice.extendWith(stmt.id) gen (params: _*)).vars -- kill ++ gen) // function f(x, y) { ... }   f(10);
      }

      stmt match {
        case ExprStmt(FuncCall(_, args)) => resolve(args) // f(a);
        case ExprStmt(AssignExpr(_, _, FuncCall(_, args))) => resolve(args) // x = f(a);
        case VarDeclStmt(_, FuncCall(_, args)) => resolve(args) // var x = f(a);
        case _ => bottom
      }

    // add variables appearing in the body of the function and the return variable
    case EntryNode(Some(FunctionDecl(_, FunctionExpr(_, ps, stmt)))) =>
      lattice.gen((((Util vars stmt) -- (ps map (_.str))) ++ Seq(Util.ret)).toSeq: _*) // uninitialized parameters + all local variables (minus parameters) + return variable

    case ExitNode(Some(_)) => ContextSensitiveLattice(lattice.vars filter { case (_, str) => str == Util.ret }) // keep the return variable if it is present

    case n@RetNode(stmt, _) =>
      val callSiteLattice = entry(cfg.call_ret(n)) // dataflow facts before the call

      def resolve(x: String) = {
        val kill = lattice.contexts.filter(ctx => !(lattice.vars contains(ctx extend stmt.id, Util.ret))) map (ctx => (ctx, x))
        val gen = lattice.contexts.filter(ctx => lattice.vars contains(ctx extend stmt.id, Util.ret)) map (ctx => (ctx, x))
        ContextSensitiveLattice(callSiteLattice.vars -- kill ++ gen)
      }

      stmt match {
        case ExprStmt(AssignExpr(_, LVarRef(x), FuncCall(_, _))) => resolve(x) // x = f(e);
        case VarDeclStmt(IntroduceVar(x), FuncCall(_, _)) => resolve(x) // var x = f(e);
        case _ => callSiteLattice // f(e);
      }
    case _ => lattice
  }

  // are variables in 'e' all initialized
  def initialized(e: Expression, lattice: ContextSensitiveLattice) = (Util fv e intersect (lattice.vars map { case (_, str) => str })).isEmpty

  def transfer(stmt: Statement, lattice: ContextSensitiveLattice) = {
    stmt match {
      case VarDeclStmt(IntroduceVar(y), e) =>
        e match {
          case EmptyExpr() => lattice gen y // var y;
          case _ => lattice resolve(y, e) // var y = e;
        }
      case ExprStmt(AssignExpr(_, LVarRef(y), e)) => lattice resolve(y, e) // y = e;
      case ReturnStmt(e) => lattice resolve(Util.ret, e) // return e;
      case _ => lattice
    }
  }
}