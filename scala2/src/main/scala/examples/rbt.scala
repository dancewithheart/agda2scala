package examples

object rbt
{

sealed trait Color
object Color {
  case object Red extends Color
  case object Black extends Color
}

sealed trait RedBlackTree[V]
object RedBlackTree {
  case object EmptyRBT extends RedBlackTree[Nothing]
  final case class RBT[V](x0: Color, x1: RedBlackTree[V], x2: Long, x3: V, x4: RedBlackTree[V]) extends RedBlackTree[V]
}

def lookup[V](x1: V, x2: Long, x3: RedBlackTree[V]): V = x3 match {
  case RedBlackTree.EmptyRBT =>
    x1
  case RedBlackTree.RBT(p0, p1, p2, p3, p4) =>
    if (x2 < p2)
      lookup(x1, x2, p1)
    else if (p2 < x2)
      lookup(x1, x2, p4)
    else
      p3
}
}
