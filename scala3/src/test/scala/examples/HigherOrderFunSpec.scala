package examples

import scala.annotation.tailrec

import zio.test.*

import examples.higher_order_fun
import examples.higher_order_fun.{List as HList}
import examples.higher_order_fun.{Maybe as HMaybe}

object HigherOrderFunSpec extends ZIOSpecDefault:
  private val intListGen: Gen[Any, scala.List[Int]] = Gen.listOf(Gen.int(-1000, 1000))

  private def fromScala[A](values: scala.List[A]): HList[A] =
    values.foldRight[HList[A]](HList.Nil) { (value, tail) =>
      HList.Cons(value, tail)
    }

  private def toScala[A](values: HList[A]): scala.List[A] =
    @tailrec
    def loop(remaining: HList[A], accumulator: scala.List[A]): scala.List[A] =
      remaining match
        case HList.Nil => accumulator.reverse
        case HList.Cons(head, tail) => loop(tail, head :: accumulator)
    loop(values, scala.Nil)

  def spec = suite("generated higher-order functions")(
      test("map can be used directly with generated constructors") {
        val input: HList[Int] =
          HList.Cons(
            1,
            HList.Cons(
              2,
              HList.Cons(3, HList.Nil)
            )
          )

        val result =
          higher_order_fun.map(
            (value: Int) => value * 10,
            input
          )

        val expected: HList[Int] =
          HList.Cons(
            10,
            HList.Cons(
              20,
              HList.Cons(30, HList.Nil))
          )

        assertTrue(result == expected)
      },

      test("map agrees with Scala List.map") {
        check(intListGen) { values =>
          val input     = fromScala(values)
          val increment = (value: Int) => value + 1
          val result = higher_order_fun.map(increment, input)

          assertTrue(
            toScala(result) == values.map(increment)
          )
        }
      },

      test("map satisfies the identity law") {
        check(intListGen) { values =>
          val input = fromScala(values)

          assertTrue(
            higher_order_fun.map(identity[Int], input) == input
          )
        }
      },

      test("map satisfies the composition law") {
        check(intListGen) { values =>
          val input     = fromScala(values)
          val increment = (value: Int) => value + 1
          val double    = (value: Int) => value * 2

          val sequential =
            higher_order_fun.map(
              double,
              higher_order_fun.map(increment, input)
            )
          val composed = higher_order_fun.map(double.compose(increment), input)

          assertTrue(sequential == composed)
        }
      },

      test("foldr agrees with Scala List.foldRight") {
        check(intListGen) { values =>
          val input = fromScala(values)
          // Non-associative operation checks the direction of the fold.
          val operation = (value: Int) => (accumulator: Int) => value - accumulator

          val result =
            higher_order_fun.foldr(
              operation,
              0,
              input
            )
          val expected = values.foldRight(0)(_ - _)

          assertTrue(result == expected)
        }
      },

      test("foldr can reconstruct the original list") {
        check(intListGen) { values =>
          val input = fromScala(values)

          val reconstructed =
            higher_order_fun.foldr(
              (head: Int) => (tail: HList[Int]) =>
                HList.Cons(head, tail),
              HList.Nil: HList[Int],
              input
            )

          assertTrue(reconstructed == input)
        }
      },

      test("filter agrees with Scala List.filter") {
        check(intListGen) { values =>
          val input = fromScala(values)
          val predicate = (value: Int) => value % 3 == 0
          val result = higher_order_fun.filter(predicate, input)

          assertTrue( toScala(result) == values.filter(predicate) )
        }
      },

      test("filter is idempotent") {
        check(intListGen) { values =>
          val input = fromScala(values)
          val predicate = (value: Int) => value % 2 == 0
          val filteredOnce = higher_order_fun.filter(predicate, input)
          val filteredTwice = higher_order_fun.filter(predicate, filteredOnce)

          assertTrue(filteredTwice == filteredOnce)
        }
      },

      test("maybe handles Just and None") {
        val inputGen = Gen.int(-1000, 1000).zip(Gen.int(-1000, 1000))
        check(inputGen) { case (value, defaultValue) =>
          val transform = (number: Int) => number * 2
          val justResult = higher_order_fun.maybe(defaultValue, transform, HMaybe.Just(value))
          val none: HMaybe[Int] = HMaybe.None
          val noneResult = higher_order_fun.maybe(defaultValue, transform, none)

          assertTrue(
            justResult == transform(value),
            noneResult == defaultValue
          )
        }
      }
    )
