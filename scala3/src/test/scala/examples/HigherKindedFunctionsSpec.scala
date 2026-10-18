package examples

import zio.test._

object HigherKindedFunctionsSpec extends ZIOSpecDefault {

  def spec = suite("Higher-kinded functions")(
    test("polymorphic identity works for Option") {
      val input: Option[Long] = Some(42L)

      assertTrue(
        hk_typs.polyId[Option, Long](input) == input,
        hk_typs.polyId[Option, Long](None) == None
      )
    },

    test("polymorphic identity works for List") {
      check(Gen.listOf(Gen.int(-1000, 1000))) { values =>
        val input = values.map(_.toLong)

        assertTrue(
          hk_typs.polyId[List, Long](input) == input
        )
      }
    }
  )
}
