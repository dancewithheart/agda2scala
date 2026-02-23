package examples

import zio.test.Assertion.equalTo
import zio.test.junit.JUnitRunnableSpec
import zio.test.assert

import examples.adts.Rgb.{Red, Green, Blue}
import examples.adts.idRgb
import examples.adts.{RgbPair, constRgbPair, hello}
import examples.adts.Bool.{True, False}

object RgbSpec extends JUnitRunnableSpec {

  def spec = suite("Examples test")(
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
}
