package examples

object ConstructiveSat
{

def not(x0: Boolean): Boolean = x0 match {
  case false =>
    true
  case true =>
    false
}

def and(x0: Boolean, x1: Boolean): Boolean = x0 match {
  case false =>
    false
  case true =>
    x1
}

def or(x0: Boolean, x1: Boolean): Boolean = x0 match {
  case false =>
    x1
  case true =>
    true
}

def implies(x0: Boolean, x1: Boolean): Boolean = x0 match {
  case false =>
    true
  case true =>
    x1
}

sealed trait Variable
object Variable {
  case object p extends Variable
  case object q extends Variable
  case object r extends Variable
}

final case class Environment(pValue: Boolean, qValue: Boolean, rValue: Boolean)

def lookup(x0: Environment, x1: Variable): Boolean = x1 match {
  case Variable.p =>
    x0.pValue
  case Variable.q =>
    x0.qValue
  case Variable.r =>
    x0.rValue
}

sealed trait Formula
object Formula {
  final case class atom(x0: Variable) extends Formula
  final case class neg(x0: Formula) extends Formula
  final case class conj(x0: Formula, x1: Formula) extends Formula
  final case class disj(x0: Formula, x1: Formula) extends Formula
  final case class impl(x0: Formula, x1: Formula) extends Formula
}

def evaluate(x0: Environment, x1: Formula): Boolean = x1 match {
  case Formula.atom(p0) =>
    lookup(x0, p0)
  case Formula.neg(p1) =>
    not(evaluate(x0, p1))
  case Formula.conj(p2, p3) =>
    and(evaluate(x0, p2), evaluate(x0, p3))
  case Formula.disj(p4, p5) =>
    or(evaluate(x0, p4), evaluate(x0, p5))
  case Formula.impl(p6, p7) =>
    implies(evaluate(x0, p6), evaluate(x0, p7))
}

def allEnvironments(): List[Environment] = Environment(false, false, false) :: Environment(false, false, true) :: Environment(false, true, false) :: Environment(false, true, true) :: Environment(true, false, false) :: Environment(true, false, true) :: Environment(true, true, false) :: Environment(true, true, true) :: Nil

def findSolution(x0: Formula, x1: List[Environment]): Option[Environment] = x1 match {
  case Nil =>
    None
  case p0 :: p1 =>
    with_102(x0, p0, evaluate(p0, x0), p1)
}

def with_102(x0: Formula, x: Environment, x2: Boolean, xs: List[Environment]): Option[Environment] = x2 match {
  case false =>
    findSolution(x0, xs)
  case true =>
    Some(x)
}

def solve(x0: Formula): Option[Environment] = findSolution(x0, allEnvironments())

def exampleSat(): Formula = Formula.conj(Formula.atom(Variable.p), Formula.neg(Formula.atom(Variable.q)))

def exampleUnsat(): Formula = Formula.conj(Formula.atom(Variable.p), Formula.neg(Formula.atom(Variable.p)))
}
