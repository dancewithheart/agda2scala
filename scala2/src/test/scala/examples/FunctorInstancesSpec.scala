package examples

import zio.test._

object FunctorInstancesSpec extends ZIOSpecDefault {
  def spec = suite("Functor instances")(
    test("Option functor") {
      val f = Functors.optionFunctor()
      val result = f.map((x: Long) => x + 1L, Some(41L))
      assertTrue(result == Some(42L))
    },
    test("List functor") {
      val f = Functors.listFunctor()
      val result = f.map((x: Long) => x * 2L, List(1L, 2L, 3L))
      assertTrue(result == List(2L, 4L, 6L))
    }
  )
}
