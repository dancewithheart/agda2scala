package examples

object Coyoneda
{

trait Coyoneda[F[_], A] {
  type X
  def fx: F[X]
  def k: X => A
}

def addOne(x0: Long): Long = x0 + 1

def example(): Coyoneda[Option, Long] = new Coyoneda[Option, Long] {
  type X = Long
  def fx: Option[X] = Some(41)
  def k: X => Long = addOne
}
}
