package examples

import zio.test.Assertion.equalTo
import zio.test.junit.JUnitRunnableSpec
import zio.test.assert
import examples.adts.{Rgb, RgbPair, constRgbPair, hello, idRgb}
import examples.adts.Rgb.{Blue, Green, Red}
import examples.adts.Answer.{No, Yes}

object RgbSpec extends JUnitRunnableSpec:

  def spec = suite("Test Rgb")(
    test("identity on Rgb") {
      assert(idRgb(Red))(equalTo(Red)) &&
      assert(idRgb(Green))(equalTo(Green)) &&
      assert(idRgb(Blue))(equalTo(Blue))
    },
    test("const on RgbPair") {
      assert(constRgbPair(RgbPair(Blue, Yes), Red))(
        equalTo(RgbPair(Blue, Yes))
      ) &&
      assert(constRgbPair(RgbPair(Red, No), Green))(
        equalTo(RgbPair(Red, No))
      ) &&
      assert(constRgbPair(RgbPair(Blue, Yes), Blue))(
        equalTo(RgbPair(Blue, Yes))
      )
    },
    test("hello returns Hello World string") {
      assert(hello())(equalTo("Hello, world!"))
    }
  )
