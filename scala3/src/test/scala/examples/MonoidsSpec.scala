package examples

import zio.test._

object MonoidsSpec extends ZIOSpecDefault:

  val natPlus = new Monoids.Monoid[Long]:
    def empty: Long = 0L
    def combine: Long => Long => Long =
      x => y => x + y

  def spec =
    suite("Monoid")(
      test("combine3") {
        assertTrue(
          Monoids.combine3(natPlus, 1L, 2L, 3L) == 6L
        )
      },
      test("prependEmpty") {
        assertTrue(
          Monoids.prependEmpty(natPlus, 42L) == 42L
        )
      }
    )
