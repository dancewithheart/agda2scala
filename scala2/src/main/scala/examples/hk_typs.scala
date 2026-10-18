package examples

object hk_typs
{

trait NatTransformer[F[_]] {
  def run: F[Long] => F[Long]
}
}
