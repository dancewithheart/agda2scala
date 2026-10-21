package examples

import zio.test._
import scala.compiletime.testing.typeChecks

object DependentTypesSpec extends ZIOSpecDefault:

  def spec = suite("Dependent types")(
    test("Package preserves its dependent member") {
      val p = DependentPackage.example()

      val value: p.Carrier = p.item

      assertTrue(value == 42L)
    },

    test("Coyoneda preserves its hidden type") {
      val c = Coyoneda.example()

      val result: Option[Long] = c.fx.map(c.k)

      assertTrue(result.contains(42L))
    },

    test("Package carrier remains abstract") {
      assertTrue(!typeChecks("""
        val n: Long = examples.DependentPackage.example().item
      """))
    },

    test("Different packages have unrelated carrier types") {
      assertTrue(!typeChecks("""
        val p = examples.DependentPackage.example()
        val q = examples.DependentPackage.example()

        val value: p.Carrier = q.item
      """))
    }
  )