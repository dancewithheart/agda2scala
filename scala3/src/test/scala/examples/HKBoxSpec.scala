package examples

import zio.test._

object HKBoxSpec extends ZIOSpecDefault {

  def spec = suite("Higher-kinded datatypes")(
    test("wraps Option values") {
      val boxed: hk_typs.HKBox[Option, Long] =
        hk_typs.HKBox.Wrap[Option, Long](Some(42L))

      boxed match {
        case hk_typs.HKBox.Wrap(value) =>
          assertTrue(value == Some(42L))
      }
    },

    test("wraps List values") {
      check(Gen.listOf(Gen.int(-1000, 1000))) { values =>
        val input = values.map(_.toLong)

        val boxed: hk_typs.HKBox[List, Long] =
          hk_typs.HKBox.Wrap[List, Long](input)

        boxed match {
          case hk_typs.HKBox.Wrap(result) =>
            assertTrue(result == input)
        }
      }
    }
  )
}
