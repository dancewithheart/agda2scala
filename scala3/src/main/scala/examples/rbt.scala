package examples

object rbt:
  enum Color:
    case Red
    case Black

  enum RedBlackTree[V]:
    case EmptyRBT extends RedBlackTree[Nothing]
    case RBT[V](x0: Color, x1: RedBlackTree[V], x2: Long, x3: V, x4: RedBlackTree[V]) extends RedBlackTree[V]

  def lookup[V](x1: V, x2: Long, x3: RedBlackTree[V]): V = x3 match
    case RedBlackTree.EmptyRBT =>
      x1
    case RedBlackTree.RBT(p0, p1, p2, p3, p4) =>
      if x2 < p2 then
        lookup(x1, x2, p1)
      else if p2 < x2 then
        lookup(x1, x2, p4)
      else
        p3

  def checkR[V](x1: Long, x2: RedBlackTree[V], x3: V, x4: RedBlackTree[V]): RedBlackTree[V] = x4 match
    case RedBlackTree.RBT(p0, p1, p2, p3, p4) =>
      p0 match
        case Color.Red =>
          p1 match
            case RedBlackTree.EmptyRBT =>
              p4 match
                case RedBlackTree.RBT(p0, p1, p2, p3, p4) =>
                  p0 match
                    case Color.Red =>
                      RedBlackTree.RBT(Color.Red, RedBlackTree.RBT(Color.Black, x2, x1, x3, RedBlackTree.EmptyRBT), p2, p3, RedBlackTree.RBT(Color.Black, p1, p2, p3, p4))
            case RedBlackTree.RBT(p0, p1, p2, p3, p4) =>
              p2 match
                case Color.Red =>
                  RedBlackTree.RBT(Color.Red, RedBlackTree.RBT(Color.Black, x2, x1, x3, p3), p4, p0, RedBlackTree.RBT(Color.Black, p1, p2, p3, p4))
                case Color.Black =>
                  p4 match
                    case RedBlackTree.RBT(p0, p1, p2, p3, p4) =>
                      p0 match
                        case Color.Red =>
                          RedBlackTree.RBT(Color.Red, RedBlackTree.RBT(Color.Black, x2, x1, x3, RedBlackTree.RBT(Color.Black, p3, p4, p0, p1)), p2, p3, RedBlackTree.RBT(Color.Black, p1, p2, p3, p4))
    case _ =>
      RedBlackTree.RBT(Color.Black, x2, x1, x3, x4)

  def checkL[V](x1: Long, x2: RedBlackTree[V], x3: V, x4: RedBlackTree[V]): RedBlackTree[V] = x2 match
    case RedBlackTree.RBT(p0, p1, p2, p3, p4) =>
      x3 match
        case Color.Red =>
          x4 match
            case RedBlackTree.EmptyRBT =>
              p2 match
                case RedBlackTree.RBT(p0, p1, p2, p3, p4) =>
                  p3 match
                    case Color.Red =>
                      RedBlackTree.RBT(Color.Red, RedBlackTree.RBT(Color.Black, RedBlackTree.EmptyRBT, p0, p1, p4), p0, p1, RedBlackTree.RBT(Color.Black, p2, x1, p3, p4))
            case RedBlackTree.RBT(p0, p1, p2, p3, p4) =>
              p0 match
                case Color.Red =>
                  RedBlackTree.RBT(Color.Red, RedBlackTree.RBT(Color.Black, p1, p2, p3, p4), p0, p1, RedBlackTree.RBT(Color.Black, p2, x1, p3, p4))
                case Color.Black =>
                  p2 match
                    case RedBlackTree.RBT(p0, p1, p2, p3, p4) =>
                      p3 match
                        case Color.Red =>
                          RedBlackTree.RBT(Color.Red, RedBlackTree.RBT(Color.Black, RedBlackTree.RBT(Color.Black, p1, p2, p3, p4), p0, p1, p4), p0, p1, RedBlackTree.RBT(Color.Black, p2, x1, p3, p4))
    case _ =>
      checkR(x1, x2, x3, x4)

  def balance[V](x1: Color, x2: RedBlackTree[V], x3: Long, x4: V, x5: RedBlackTree[V]): RedBlackTree[V] = x1 match
    case Color.Red =>
      RedBlackTree.RBT(Color.Red, x2, x3, x4, x5)
    case Color.Black =>
      checkL(x3, x2, x4, x5)

  def ins[V](x1: Long, x2: V, x3: RedBlackTree[V]): RedBlackTree[V] = x3 match
    case RedBlackTree.EmptyRBT =>
      RedBlackTree.RBT(Color.Red, RedBlackTree.EmptyRBT, x1, x2, RedBlackTree.EmptyRBT)
    case RedBlackTree.RBT(p0, p1, p2, p3, p4) =>
      if x1 < p2 then
        balance(p0, ins(x1, x2, p1), p2, p3, p4)
      else if p2 < x1 then
        balance(p0, p1, p2, p3, ins(x1, x2, p4))
      else
        RedBlackTree.RBT(p0, p1, x1, x2, p4)

  def makeBlack[V](x1: RedBlackTree[V]): RedBlackTree[V] = x1 match
    case RedBlackTree.EmptyRBT =>
      RedBlackTree.EmptyRBT
    case RedBlackTree.RBT(p0, p1, p2, p3, p4) =>
      RedBlackTree.RBT(Color.Black, p1, p2, p3, p4)

  def insert[V](x1: Long, x2: V, x3: RedBlackTree[V]): RedBlackTree[V] = makeBlack(ins(x1, x2, x3))
