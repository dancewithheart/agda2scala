package examples

import zio.test._
import examples.Coyoneda

object CoyonedaSpec extends ZIOSpecDefault {

  def spec = suite("Coyoneda example")(
    test("Coyoneda example") {
      val c = Coyoneda.example()
      val result: Option[Long] = c.fx.map(c.k)
      assertTrue(result.contains(42L))
    }
  )
}
