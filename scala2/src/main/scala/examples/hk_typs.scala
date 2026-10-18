package examples

object hk_typs
{

trait NatTransformer[F[_]] {
  def run: F[Long] => F[Long]
}

def polyId[F[_], A](x2: F[A]): F[A] = x2
}
