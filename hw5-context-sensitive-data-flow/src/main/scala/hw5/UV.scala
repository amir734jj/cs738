package hw5

import common._

import scala.collection.mutable

// set of variables that may be uninitialized
case class Vars(vars: Set[String]) extends Lattice[Vars] {
  def lub(that: Vars) = Vars(vars.union(that.vars))
  
  override def toString = "{"+vars.toList.sortBy(x=>x).mkString(", ")+"}"
}

// uninitialized variables: forward and may analysis
case class UV(stmt: Statement) extends Analysis[Vars] {
  val cfg = ForwardCFG(stmt)
  val entry = real_entry
  val exit = real_exit
  
  val extremalValue = Vars(Util.vars(stmt))
  val bottom = Vars(Set())
  
  def transfer(node: Node, l: Vars) = node match {
    case IntraNode(stmt) => transfer(stmt, l)

    // for each parameter p, if its argument is not initialized, then p is not initialized 
    case CallNode(stmt, to) => {
      def h(args: List[Expression]) = {
        val Some(FunctionDecl(_, FunctionExpr(_, ps, _))) = to.f 
        val params = ps.map(p => p.str) 
        val s = for((p, e) <- params zip args; if initialized(e, l)) yield p
        Vars(params.toSet -- s)     // function f(x, y) { ... }   f(10);
      }
      
      stmt match {
        case ExprStmt(FuncCall(_, args)) => h(args)                       // f(a);
        case ExprStmt(AssignExpr(_, _, FuncCall(_, args))) => h(args)     // x = f(a);
        case VarDeclStmt(_, FuncCall(_, args)) => h(args)                 // var x = f(a);
        case _ => bottom
      }
    }
    // add variables appearing in the body of the function and the return variable
    case EntryNode(Some(FunctionDecl(_, FunctionExpr(_,ps,stmt)))) => {
      Vars(l.vars ++ (Util.vars(stmt) -- ps.map(p=>p.str)) + Util.ret) // uninitialized parameters + all local variables (minus parameters) + return variable
    }
    
    case ExitNode(Some(_)) => Vars(l.vars.intersect(Set(Util.ret))) // keep the return variable if it is present
    
    case n@RetNode(stmt, _) => {
      val lc = entry(cfg.call_ret(n))  // dataflow facts before the call
      
      def h(x: String) = Vars(if (l.vars.contains(Util.ret)) lc.vars + x else lc.vars - x)
      
      stmt match { 
        case ExprStmt(AssignExpr(_, LVarRef(x), FuncCall(_, _))) => h(x) // x = f(e);
        case VarDeclStmt(IntroduceVar(x), FuncCall(_, _)) => h(x)    // var x = f(e);
        case _ => lc // f(e);
      } 
    }
    
    case _ => l
  }
  
  // are variables in 'e' all initialized
  def initialized(e: Expression, l: Vars) = Util.fv(e).intersect(l.vars).isEmpty
  
  def transfer(stmt: Statement, l: Vars) = {
    	def kill_gen(y: String, e: Expression) = Vars(if (initialized(e, l)) l.vars - y else l.vars + y)
    	
	    stmt match {
	      case VarDeclStmt(IntroduceVar(y), e) => {     
	        e match {
	          case EmptyExpr() => Vars(l.vars + y)      // var y;
	          case _ => kill_gen(y, e)                  // var y = e;
	        }
	      }
	      case ExprStmt(AssignExpr(_, LVarRef(y), e)) => kill_gen(y, e) // y = e;
	      case ReturnStmt(e) => kill_gen(Util.ret, e)  // return e;
	      case _ => l
	    }
  }
}

object Context {
  val MAX_SIZE = 2
}

case class Context(callStrings: List[Long]) {
  import Context._

  def extend(l: Long) = Context(callStrings ++ Seq(l).takeRight(MAX_SIZE))

  override def toString: String = f"[ ${callStrings.mkString(", ")} ]"
}

case class CSVars(csVars: Set[(Context, String)]) extends Lattice[CSVars] {
  override def lub(that: CSVars): CSVars = CSVars(csVars ++ that.csVars)

  val contexts: Set[Context] = csVars.map { case (context, _) => context }

  def variables(ctx: Context) = csVars
    .filter { case (context, _) => context == ctx }
    .map { case (_, str) => str }

  def initialized(e: Expression, ctx: Context) = (Util.fv(e) intersect variables(ctx)).isEmpty

  def resolve(l: String, e: Expression) = {
    val gen = contexts filter (initialized(e, _)) map ((_, l))
    val kill = contexts filter (!initialized(e, _)) map ((_, l))
    CSVars(gen ++ csVars -- kill)
  }

  def gen(l: String) = CSVars(csVars ++ contexts.map(ctx => (ctx, l)))

  def gen(ls: List[String]) = CSVars(csVars ++ contexts.flatMap(ctx => ls.map((ctx, _))))

  def extend(lc: Long) = CSVars(contexts.map(ctx => ctx.extend(lc)).map((_, Util.zero)))

  override def toString = f"{ ${csVars.toList.sortBy{ case (_, str) => str }.mkString(", ")} }"
}

case class CSUV(stmt: Statement) extends Analysis[CSVars] {
  override val cfg: CFG = ForwardCFG(stmt)
  override val extremalValue = CSVars(Util.vars(stmt).map((Context(List()), _)))
  override val bottom = CSVars(Set())
  override val entry = real_entry
  override val exit = real_exit

  override def transfer(node: Node, l: CSVars): CSVars = {
    stmt match {
      case VarDeclStmt(IntroduceVar(y), e) => {
        e match {
          case EmptyExpr() => CSVars(l.csVars)      // var y;
          case _ => l.resolve(y, e)                  // var y = e;
        }
      }
      case ExprStmt(AssignExpr(_, LVarRef(y), e)) => l.resolve(y, e) // y = e;
      case ReturnStmt(e) => l.resolve(Util.ret, e)  // return e;
      case _ => l
    }
  }
}