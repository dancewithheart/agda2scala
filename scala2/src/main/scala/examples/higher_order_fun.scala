package examples

object higher_order_fun
{

def id[A](x1: A): A = x1

sealed trait Maybe[+A]
object Maybe {
  final case class Just[+A](x0: A) extends Maybe[A]
  case object None extends Maybe[Nothing]
}

sealed trait List[+X]
object List {
  case object Nil extends List[Nothing]
  final case class Cons[+X](x0: X, x1: List[X]) extends List[X]
}

def emptyNatList(): List[Long] = List.Nil

def map[A, B](x2: A => B, x3: List[A]): List[B] = x3 match {
  case List.Nil =>
    List.Nil
  case List.Cons(p0, p1) =>
    List.Cons(x2(p0), map(x2, p1))
}

def foldr[A, B](x2: A => B => B, x3: B, x4: List[A]): B = x4 match {
  case List.Nil =>
    x3
  case List.Cons(p0, p1) =>
    x2(p0)(foldr(x2, x3, p1))
}

def filter[A](x1: A => Boolean, x2: List[A]): List[A] = x2 match {
  case List.Nil =>
    List.Nil
  case List.Cons(p0, p1) =>
    with_78(x1, p0, x1(p0), p1)
}

def with_78[A](x1: A => Boolean, x2: A, x3: Boolean, x4: List[A]): List[A] = x3 match {
  case false =>
    filter(x1, x4)
  case true =>
    List.Cons(x2, filter(x1, x4))
}

def maybe[A, B](x2: B, x3: A => B, x4: Maybe[A]): B = x4 match {
  case Maybe.Just(p0) =>
    x3(p0)
  case Maybe.None =>
    x2
}
}
