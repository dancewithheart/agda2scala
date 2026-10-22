package examples

import zio.test._
import Monoids._

object MonoidsPropertySpec extends ZIOSpecDefault {
  def spec = suite("Monoids")(
    test("natPlusRightUnit") {
      check(Gen.long(0L, 1000000L)) { x0 =>
        assertTrue((x0 + 0) == (x0))
      }
    },
    test("natPlusLeftUnit") {
      check(Gen.long(0L, 1000000L)) { x0 =>
        assertTrue((0 + x0) == (x0))
      }
    },
    test("natPlusAssoc") {
      check(Gen.long(0L, 1000000L), Gen.long(0L, 1000000L), Gen.long(0L, 1000000L)) { (x0, x1, x2) =>
        assertTrue(((x0 + x1) + x2) == (x0 + (x1 + x2)))
      }
    }
  )
}
