package hw5

import common.{AssignExpr, EmptyExpr, ExprStmt, Expression, FuncCall, FunctionDecl, FunctionExpr, IntroduceVar, LVarRef, ReturnStmt, Statement, Util, VarDeclStmt}

object Context {
  val MAX_SIZE = 2
}

case class Context(callStrings: List[Long]) {

  import Context._

  def extend(l: Long) = Context((callStrings ++ Seq(l)).takeRight(MAX_SIZE))

  override def toString: String = f"[ ${callStrings.mkString(", ")} ]"
}

case class CSVars(vars: Set[(Context, String)]) extends Lattice[CSVars] {
  override def lub(that: CSVars): CSVars = CSVars(vars ++ that.vars)

  val contexts: Set[Context] = vars.map { case (context, _) => context }

  def variables(ctx: Context) = vars
    .filter { case (context, _) => context == ctx }
    .map { case (_, str) => str }

  def initialized(e: Expression, ctx: Context) = (Util.fv(e) intersect variables(ctx)).isEmpty

  def resolve(l: String, e: Expression) = {
    val gen = contexts filter (!initialized(e, _)) map ((_, l))
    val kill = contexts filter (initialized(e, _)) map ((_, l))
    CSVars(gen ++ vars -- kill)
  }

  def gen(l: String) = CSVars(vars ++ contexts.map((_, l)))

  def gen(ls: Set[String]) = CSVars(vars ++ contexts.flatMap(ctx => ls.map((ctx, _))))

  def extend(lc: Long) = CSVars(contexts.map(_.extend(lc)).map((_, Util.zero)))

  override def toString = f"{ ${vars.filter(_._2 != Util.zero).toList.sortBy { case (_, str) => str }.mkString(", ")} }"
}

case class CSUV(stmt: Statement) extends Analysis[CSVars] {
  override val cfg: CFG = ForwardCFG(stmt)
  override val extremalValue = CSVars((Util.vars(stmt) + Util.zero).map((Context(List()), _)))
  override val bottom = CSVars(Set())
  override val entry = real_entry
  override val exit = real_exit

  def transfer(node: Node, l: CSVars): CSVars = node match {
    case IntraNode(stmt) => transfer(stmt, l)

    // for each parameter p, if its argument is not initialized, then p is not initialized
    case CallNode(stmt, to) => {
      def h(args: List[Expression]) = {
        val Some(FunctionDecl(_, FunctionExpr(_, ps, _))) = to.f
        val params = ps.map(p => p.str).toSet
        val gen = l.contexts.flatMap(ctx => (params zip args).withFilter { case (_, e) => !l.initialized(e, ctx) }.map { case (y, _) => (ctx.extend(stmt.id), y) })
        val kill = l.contexts.flatMap(ctx => (params zip args).withFilter { case (_, e) => l.initialized(e, ctx) }.map { case (y, _) => (ctx.extend(stmt.id), y) })
        CSVars(gen ++ l.extend(stmt.id).gen(params).vars -- kill) // function f(x, y) { ... }   f(10);
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
      l.gen(((Util.vars(stmt) -- ps.map(_.str)) + Util.ret).toSet) // uninitialized parameters + all local variables (minus parameters) + return variable
    }

    case ExitNode(Some(_)) => CSVars(l.vars.filter(_._2 == Util.ret)) // keep the return variable if it is present

    case n@RetNode(stmt, _) => {
      val lc = entry(cfg.call_ret(n)) // dataflow facts before the call

      def h(x: String) = {
        val kill = l.contexts.filter(ctx => !l.vars.contains((ctx.extend(stmt.id), Util.ret))).map(ctx => (ctx, x))
        val gen = l.contexts.filter(ctx => l.vars.contains((ctx.extend(stmt.id), Util.ret))).map(ctx => (ctx, x))
        CSVars(gen ++ lc.vars -- kill)
      }

      stmt match {
        case ExprStmt(AssignExpr(_, LVarRef(x), FuncCall(_, _))) => h(x) // x = f(e);
        case VarDeclStmt(IntroduceVar(x), FuncCall(_, _)) => h(x) // var x = f(e);
        case _ => lc // f(e);
      }
    }
    case _ => l
  }

  // are variables in 'e' all initialized
  def initialized(e: Expression, l: CSVars) = Util.fv(e).intersect(l.vars.map(_._2)).isEmpty

  def transfer(stmt: Statement, l: CSVars) = {
    stmt match {
      case VarDeclStmt(IntroduceVar(y), e) => {
        e match {
          case EmptyExpr() => l.gen(y) // var y;
          case _ => l.resolve(y, e) // var y = e;
        }
      }
      case ExprStmt(AssignExpr(_, LVarRef(y), e)) => l.resolve(y, e) // y = e;
      case ReturnStmt(e) => l.resolve(Util.ret, e) // return e;
      case _ => l
    }
  }
}