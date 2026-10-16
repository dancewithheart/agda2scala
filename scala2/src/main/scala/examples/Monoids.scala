package examples

object Monoids
{

trait Monoid[A] {
  def empty: A
  def combine: A => A => A
}

def combine3[A](x1: Monoid[A], x2: A, x3: A, x4: A): A = x1.combine(x2)(x1.combine(x3)(x4))

def prependEmpty[A](x1: Monoid[A], x2: A): A = x1.combine(x1.empty)(x2)
}
