package cfa2

trait Exp
trait Call
trait Clos

case class Call()

case class Time(labels: Seq[String])

// Clos = UClos + CClos
// UClos = ULam × BEnv
// CClos = (CLam × BEnv) + halt
case class UClos(a: ULam, b: BEnv) extends Clos
case class CClos(a: CLam, b: BEnv) extends Clos

// Env = VEnv + BEnv
case class VEnv(a: Map[(String, Time), Clos])
case class BEnv(a: Map[String, Time])

// Clos = UClos + CClos
// UClos = ULam × BEnv
// CClos = (CLam × BEnv) + halt
case class UClos(a: ULam, b: BEnv) extends Clos
case class CClos(a: CLam, b: BEnv) extends Clos

case class CLam (x: UVar, e: CExp) extends Exp // continuation lambda

trait BExp extends Exp // atomic or primitive expression
trait CExp extends Exp // complex expression
trait AExp extends BExp // atomic expression

case class KVar (x: String) extends Exp // continuation variable
case class KLam (x: UVar, e: CExp) extends Exp // continuation lambda

case class UVar (x: String) extends AExp // user variable
case class ULam (xs: List[UVar], k: KVar, e: CExp) extends AExp // user lambda


import Common._

trait State {
  def apply(c: CExp): State = _
}

case class CEval(a: Call, b: Stack, c: Heap) extends State // Exit node
case class UApply(a: ULam, b: UClos, c: Heap) extends State // Entry node

// its predecessor is an exit, or an inner state if its predecessor is also an inner state.
case class CApply(a: CClos, b: UClos, c: Stack, d: Heap) extends State
case class Frame(a: Map[UVar, UClos]) extends State

object halt extends State

object Common {
  type Stack = Frame
  type Heap = Frame
}

object Transitions {

  import Common._

  def succ(a: State): Set[State] = _


  def worklist() = {

    def A(e: State, psi: State, tf: State, h: State) = {
      a match {
        case CLam(_, e) => Set(e)
        case _ => if (isStackVariable(e)) tf(e) else h(e)
      }
    }

    def isInitialState(s: State): Boolean = _

    def isStackVariable(s: State): Boolean = _

    // UEA (CEval -> UApply)
    def EvalToUApply(source: CEval): UApply = {
      val CEval(Call(f, e, q, l), tf, h) = source
      val ulam = A(f, l, tf, h)
      val d = A(e, l, tf, h)
      UApply(ulam, d, h)
    }

    // UAE (UApply -> CEval)
    def UApplyToEval(source: UApply): CEval =  {
      val UApply(lam, UVar(u, k), call, d, h) = source
      if (isStackVariable(u)) {

      } else {

      }
    }

    // CEA (CEval -> CApply)
    def EValToCApply(source: CEval): CApply = {
      val CEval(Call(clam, e, l), tf, h) = source
      val d = A(e,l, tf, h)
      CApply(Call(clam, e, l), h)
    }

    // CAE (CApply -> CEval)
    def CApplyToEval(source: CApply): CEval = {
      val CApply(Call(lam, u, call), d, tf, h) = source
      CEval(call, tf, h)
    }

    def propagate(x: State, y: State) = {
      if (!seen.contains((x, y))) {
        seen += (x, y)
        W += (x, y)
      }
    }

    def update(a: State, b: State, c: State, d: State) = {
      val UApply(_, _, _) = a
      val CEval(_, _, _) = b
      val UApply(_, _, _) = c
      val CEval(Call(, e4, l4), tf4, h4) = d
      d = A(e4, l4, tf4, h4)
      if (isStackVariable()) {

      } else {

      }
      propagate(a, b)
    }

    var summary = Set[(State, State)]() // left is UApply, right is any state
    var callers = Set[(State, State, State)]() //
    var tCallers = Set[(State, State, State)]()
    var finals = Set[(State, State, State)]()

    var seen = Set[(State, State)]()
    var W = Set[(State, State)]()

    // Callback
    val finalCb: State => Any = s => {
      val CEval(Call(_, e, l), tf, h) = s
      finals += (halt, A(e, l, tf), Set(), h)
    }

    while (!W.isEmpty) {
      val item = W.head
      W = W.tail

      item._2 match {
        case _: UApply =>
        case _: CApply =>
          succ(item._2).foreach(y => propagate(item._1, y))
        case _: Call =>
          succ(item._2).foreach(y => {
            propagate(item._1, y)
            callers += (item._1, item._2, y)
            summary.foreach(z => {
              update(item._1, item._2, z._1, z._2)
            })
          })
        case _: CEval =>
          if (isInitialState(item._1)) {
            finalCb(item._2)
          } else {
            summary += item
            callers.foreach(y => update(y._1, y._2, y._3, item._2))
            tCallers.foreach(y => propagate(y._1, item._2))
          }
        case _TC => {
          succ(item._2).foreach(y => {
            propagate(y, y)
            tCallers += (item._1, item._2, y)
            summary.foreach(z => {
              propagate(y, z._2)
            })
          })
        }
      }
    }
  }
}