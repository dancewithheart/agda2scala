package examples

import zio.test.junit.JUnitRunnableSpec
import zio.test.assertTrue

import examples.rbt.RedBlackTree
import examples.rbt.Color
import examples.rbt.RedBlackTree.EmptyRBT
import examples.rbt.lookup

object RedBlackTreeSpec extends JUnitRunnableSpec {

  // temporary until generated RedBlackTree is covariant
  // https://github.com/dancewithheart/agda2scala/issues/26
  private def emptyRBT[V]: RedBlackTree[V] =
    EmptyRBT.asInstanceOf[RedBlackTree[V]]

  def spec = suite("RedBlackTree")(
    test("lookup on empty rbt returns default value") {
      val rbt = emptyRBT[String]
      val defaultVal = "foo"
      val key = 42L
      assertTrue(defaultVal == lookup(defaultVal, key, rbt))
    },
    test("lookup returns stored value when key matches root") {
      val rbt =
        RedBlackTree.RBT[String](
          Color.Black,
          emptyRBT[String],
          42L,
          "found",
          emptyRBT[String]
        )

      assertTrue(lookup("default", 42L, rbt) == "found")
    }
  )
}
