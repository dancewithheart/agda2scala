package examples

import zio.test.junit.JUnitRunnableSpec
import zio.test.assertTrue

import examples.rbt.RedBlackTree
import examples.rbt.Color
import examples.rbt.RedBlackTree.EmptyRBT
import examples.rbt.{insert, lookup}

object RedBlackTreeSpec extends JUnitRunnableSpec:

  // temporary until generated RedBlackTree is covariant
  // https://github.com/dancewithheart/agda2scala/issues/26
  private def emptyRBT[V]: RedBlackTree[V] =
    EmptyRBT.asInstanceOf[RedBlackTree[V]]

  def spec = suite("Test Rgb")(
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
    },

    test("lookup after single insert returns inserted value") {
      val t1 = insert(42L, "found", emptyRBT[String])
      assertTrue(lookup("missing", 42L, t1) == "found")
    },

    test("lookup after several inserts returns matching values") {
      val t0 = emptyRBT[String]
      val t1 = insert(10L, "a", t0)
      val t2 = insert(5L, "b", t1)
      val t3 = insert(20L, "c", t2)

      assertTrue(
        lookup("missing", 10L, t3) == "a",
        lookup("missing", 5L, t3) == "b",
        lookup("missing", 20L, t3) == "c",
        lookup("missing", 999L, t3) == "missing"
      )
    }
  )
