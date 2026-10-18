package examples

object hk_typs
{

trait NatTransformer[F[_]] {
  def run: F[Long] => F[Long]
}

def polyId[F[_], A](x2: F[A]): F[A] = x2

sealed trait HKBox[F[_], A]
object HKBox {
  final case class Wrap[F[_], A](x0: F[A]) extends HKBox[F, A]
}
}
