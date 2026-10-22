package examples

object Functors:
  trait Functor[F[_]]:
    def map[A, B](x2: A => B, x3: F[A]): F[B]

  def mapMaybe[A, B](x2: A => B, x3: Option[A]): Option[B] = x3 match
    case Some(p0) =>
      Some(x2(p0))
    case None =>
      None

  def mapList[A, B](x2: A => B, x3: List[A]): List[B] = x3 match
    case Nil =>
      Nil
    case p0 :: p1 =>
      x2(p0) :: mapList(x2, p1)

  def optionFunctor(): Functor[Option] = new Functor[Option]:
    def map[A, B](x2: A => B, x3: Option[A]): Option[B] = mapMaybe(x2, x3)

  def listFunctor(): Functor[List] = new Functor[List]:
    def map[A, B](x2: A => B, x3: List[A]): List[B] = mapList(x2, x3)
