package examples

import zio.test.Assertion.equalTo
import zio.test.junit.JUnitRunnableSpec
import zio.test.assert

import examples.adts.Rgb.{Red, Green, Blue}
import examples.adts.idRgb
import examples.adts.{RgbPair, constRgbPair, hello}
import examples.adts.Answer.{Yes, No}

object RgbSpec extends JUnitRunnableSpec {

  def spec = suite("Examples test")(
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
}
