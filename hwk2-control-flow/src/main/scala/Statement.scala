
sealed abstract class Statement extends AbstractSyntaxTree {
  // add label to statements and add height
  def prep {
    this.setid
    this match {
      case Script(stmts) => {
        addHeights(stmts);
        stmts.foreach(s => s.prep)
      }
      case BlockStmt(stmts) => {
        addHeights(stmts);
        stmts.foreach(s => s.prep)
      }
      case VarDeclListStmt(decls) => {
        addHeights(decls);
        decls.foreach(s => s.prep)
      }
      case FunctionDecl(_, fun) => {
        fun.height = height;
        addHeight(fun.asInstanceOf[FunctionExpr].body)
      }
      case IfStmt(_, thenPart, elsePart) => {
        addHeights(List(thenPart, elsePart));
        thenPart.prep;
        elsePart.prep
      }
      case SwitchStmt(_, cases, defaultCase) => {
        val d = cases ++ (defaultCase match {
          case Some(c) => {
            c.default = true;
            List(c)
          }
          case None => List()
        })
        addHeights(d)
        d.foreach(c => c.prep)
      }
      case CaseStmt(_, body) => {
        addHeight(body);
        body.prep
      }
      case DoWhileStmt(_, body) => {
        addHeight(body);
        body.prep
      }
      case WhileStmt(_, body) => {
        addHeight(body);
        body.prep
      }
      case ForStmt(_, _, _, body) => {
        addHeight(body);
        body.prep
      }
      case ForInStmt(_, _, body) => {
        addHeight(body);
        body.prep
      }
      case LabeledStmt(_, body) => {
        addHeight(body);
        body.prep
      }
      case _ =>
    }
  }

  override def toString = this match {
    case Script(stmts) => toString(stmts)
    case BlockStmt(stmts) => space + "{\n" + toString(stmts) + space + "}"
    case VarDeclListStmt(decls) => toString(decls)
    case EmptyStmt() => ""
    case ExprStmt(expr) => space + expr.toString
    case VarDeclStmt(name, expr) => {
      val e = expr match {
        case EmptyExpr() => ""
        case _ => " = " + expr
      }
      space + "var " + name + e
    }
    case FunctionDecl(name, fun) => fun.toString
    case ReturnStmt(expr) => space + "return " + expr
    case IfStmt(cond, thenPart, elsePart) => {
      val e = elsePart match {
        case EmptyStmt() => ""
        case _ => " else\n" + elsePart
      }
      space + "if (" + cond + ") " + "\n" + thenPart + e
    }
    case SwitchStmt(cond, cases, defaultCase) => {
      val d = cases ++ (defaultCase match {
        case Some(c) => List(c)
        case None => List()
      })
      space + "switch (" + cond + ") {\n" + d.foldRight("")((s, c) => s + "\n" + c) + space + "}"
    }
    case c@CaseStmt(expr, body) => {
      c.default match {
        case true => space + "default :\n" + body
        case false => space + "case " + expr + " :\n" + body
      }
    }
    case BreakStmt(breakLabel) => space + "break " + breakLabel
    case ContinueStmt(continueLabel) => space + "continue " + continueLabel
    case DoWhileStmt(cond, body) => space + "do\n" + body + "while (" + cond + ")\n"
    case WhileStmt(cond, body) => space + "while (" + cond + ")\n" + body
    case ForStmt(init, cond, increment, body) => {
      val c = cond match {
        case Some(x) => x.toString
        case None => ""
      }
      val i = increment match {
        case Some(x) => x.toString
        case None => ""
      }
      space + "for (" + init + "; " + c + "; " + i + ")\n" + body
    }
    case ForInStmt(init, expr, body) => space + "for (" + init + " in " + expr + ")" + body
    case LabeledStmt(label, body) => label.foldRight("")((e, c) => space + e + ":\n" + c) + body
    case _ => ""
  }

  def toString(stmts: List[Statement]) = stmts.foldRight("")((s, c) =>
    (s match {
      case WhileStmt(_, _) => s + "\n"
      case DoWhileStmt(_, _) => s + "\n"
      case ForStmt(_, _, _, _) => s + "\n"
      case ForInStmt(_, _, _) => s + "\n"
      case FunctionDecl(_, _) => s + "\n"
      case IfStmt(_, _, _) => s + "\n"
      case SwitchStmt(_, _, _) => s + "\n"
      case EmptyStmt() => ""
      case _ => s + ";\n"
    }) + c
  )

  def entry: Statement = this match {
    case Script(stmts) => stmts.head.entry
    case BlockStmt(stmts) => stmts.head.entry
    case VarDeclListStmt(decls) => decls.head.entry
    case DoWhileStmt(cond, body) => body.entry
    case _ => this
  }

  def exit: Seq[Statement] = this match {
    case Script(stmts) => stmts.last.exit
    case BlockStmt(stmts) => stmts.last.exit
    case VarDeclListStmt(decls) => decls.last.exit
    case IfStmt(cond, thenPart, elsePart) => thenPart.exit ++ (elsePart match {
      case EmptyStmt() => Seq(thenPart)
      case _ => elsePart.exit
    })
    case SwitchStmt(cond, cases, defaultCase) => (defaultCase match {
      case Some(x) => cases ++ Seq(x)
      case None => cases
    }).last.exit
    case CaseStmt(expr, body) => body.exit
    case LabeledStmt(label, stmt) => stmt.exit
    case _ => Seq(this)
  }

  var successors = Seq[Statement]()

  def appendSuccessor(stmt: Statement) = {
    successors = Seq(stmt) ++ successors
  }

  protected def visit(stmts: List[Statement]): Unit = {
    stmts match {
      case x :: y :: xs => {
        x.buildGraph
        visit(y :: xs)
        x.exit foreach (e => e.appendSuccessor(y.entry))
      }
      case x :: Nil => x.buildGraph
      case Nil => ()
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

  def dotStr(): String = "\"%s\"".format((this match {
    case Script(stmts) => stmts.head.dotStr
    case BlockStmt(stmts) => stmts.head.dotStr
    case VarDeclListStmt(decls) => decls.head.dotStr
    case IfStmt(cond, thenPart, elsePart) => cond.toString
    case WhileStmt(cond, body) => cond.toString
    case DoWhileStmt(cond, body) => cond.toString
    case FunctionDecl(name, fun) => "function " + name
    case EmptyStmt() => "<EmptyStmt>"
    case _ => this.toString.trim
  }).replaceAll(" ", " "))

  def edge(s: String, d: String) = s + " -> " + d

  def toDot(indentCount: Int): Seq[String] = this match {
    case Script(stmts) => stmts.flatMap(predecessor => {
      (predecessor toDot indentCount) ++ (predecessor.successors map (successor => edge(predecessor.dotStr, successor.dotStr)))
    })
    case BlockStmt(stmts) => stmts.flatMap(s => (s toDot indentCount + 1) ++ (s.successors map (e => edge(s.dotStr, e.dotStr))))
    case VarDeclListStmt(stmts) => stmts.flatMap(s => (s toDot indentCount) ++ (s.successors map (e => edge(s.dotStr, e.dotStr))))
    case IfStmt(cond, thenPart, elsePart) => (thenPart toDot indentCount) ++ (elsePart toDot indentCount)
    case WhileStmt(cond, body) => Seq(toSubGraph(body toDot indentCount, this.id, indentCount))
    case DoWhileStmt(cond, body) => Seq(toSubGraph(body toDot indentCount, this.id, indentCount))
    case _ => Seq()
  }

  def toDotGraph =
    s"""digraph cs738_${this.getClass.getSimpleName} {
       |${(toDot(0).zipWithIndex foldLeft "")((acc, x) => f"$acc${if (x._2 > 0) "\n" else ""}${indent(0)}${x._1}")}
       |}""".stripMargin

  def indent(count: Int) = (0 to count foldLeft "")((acc, _) => acc + "  ")

  def toSubGraph(edges: Seq[String], id: Long, indentCount: Int) = {
    s"""subgraph cluster_$id {
       |${(edges.zipWithIndex foldLeft "")((acc, x) => f"$acc${if (x._2 > 0) "\n" else ""}${indent(indentCount + 1)}${x._1}")}
       |${indent(indentCount)}}""".stripMargin
  }
}

case class Script(stmts: List[Statement]) extends Statement

case class BlockStmt(stmts: List[Statement]) extends Statement

case class VarDeclListStmt(decls: List[Statement]) extends Statement

case class EmptyStmt() extends Statement

case class ExprStmt(expr: Expression) extends Statement()

case class VarDeclStmt(name: IntroduceVar, expr: Expression) extends Statement

case class FunctionDecl(name: IntroduceVar, fun: Expression) extends Statement

case class ReturnStmt(expr: Expression) extends Statement

case class IfStmt(cond: Expression, thenPart: Statement, elsePart: Statement) extends Statement

case class SwitchStmt(cond: Expression, cases: List[CaseStmt], defaultCase: Option[CaseStmt]) extends Statement

case class CaseStmt(expr: Expression, body: Statement) extends Statement {
  var default = false
}

case class BreakStmt(breakLabel: String) extends Statement

case class ContinueStmt(continueLabel: String) extends Statement

case class DoWhileStmt(cond: Expression, body: Statement) extends Statement

case class WhileStmt(cond: Expression, body: Statement) extends Statement

case class ForStmt(init: ForInit, cond: Option[Expression], increment: Option[Expression], body: Statement) extends Statement

case class ForInStmt(init: ForInInit, expr: Expression, body: Statement) extends Statement

case class LabeledStmt(label: List[String], stmt: Statement) extends Statement

case class TryStmt(body: Statement, catchClause: List[CatchStmt], finalCatch: Option[Statement]) extends Statement

case class CatchStmt(name: IntroduceVar, body: Statement) extends Statement

case class ThrowStmt(expr: Expression) extends Statement
