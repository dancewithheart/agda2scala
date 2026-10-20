package examples

import zio.test._
import Properties._

object PropertiesPropertySpec extends ZIOSpecDefault {
  def spec = suite("Properties")(
    test("addZeroLaw") {
      check(Gen.long(0L, 1000000L)) { x0 =>
        assertTrue((addZero(x0)) == (x0))
      }
    }
  )
}
