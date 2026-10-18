package examples

import zio.test._

object NatTransformerSpec extends ZIOSpecDefault {

  private val optionIncrement: hk_typs.NatTransformer[Option] =
    new hk_typs.NatTransformer[Option] {
      override def run: Option[Long] => Option[Long] =
        _.map(_ + 1L)
    }

  private val listDouble: hk_typs.NatTransformer[List] =
    new hk_typs.NatTransformer[List] {
      override def run: List[Long] => List[Long] =
        _.map(_ * 2L)
    }

  def spec = suite("NatTransformer")(
    test("transforms Option values") {
      assertTrue(
        optionIncrement.run(Some(41L)) == Some(42L),
        optionIncrement.run(None) == None
      )
    },

    test("transforms List values") {
      check(Gen.listOf(Gen.int(-1000, 1000))) { values =>
        val input = values.map(_.toLong)

        assertTrue(
          listDouble.run(input) == input.map(_ * 2L)
        )
      }
    }
  )
}
