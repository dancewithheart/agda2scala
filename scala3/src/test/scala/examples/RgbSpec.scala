package examples

import zio.test.Assertion.equalTo
import zio.test.junit.JUnitRunnableSpec
import zio.test.assert
import examples.adts.{Rgb, RgbPair, constRgbPair, hello, idRgb}
import examples.adts.Rgb.{Blue, Green, Red}
import examples.adts.Bool.{False, True}

object RgbSpec extends JUnitRunnableSpec:

  def spec = suite("Test Rgb")(
    test("identity on Rgb") {
      assert(idRgb(Red))(equalTo(Red)) &&
      assert(idRgb(Green))(equalTo(Green)) &&
      assert(idRgb(Blue))(equalTo(Blue))
    },
    test("const on RgbPair") {
      assert(constRgbPair(RgbPair(Blue, True), Red))(
        equalTo(RgbPair(Blue, True))
      ) &&
      assert(constRgbPair(RgbPair(Red, False), Green))(
        equalTo(RgbPair(Red, False))
      ) &&
      assert(constRgbPair(RgbPair(Blue, True), Blue))(
        equalTo(RgbPair(Blue, True))
      )
    },
    test("hello returns Hello World string") {
      assert(hello())(equalTo("Hello, world!"))
    }
  )
